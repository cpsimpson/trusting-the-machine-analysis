# Revision analyses (CHB Reports) ------------------------------------------
#
# Runs every analysis added or changed in the revision, in manuscript order,
# and prints the results. Run from the project root:
#   Rscript revision-analyses.R
#
# Condition labels in the data vs. the manuscript:
#   Experiment 1: Low = low, Medium = high, High = human
#   Experiment 2: Low = low, High = high

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})
source("./scripts/data-loading.R", local = TRUE)
source("./scripts/data-cleaning.R", local = TRUE)
source("./scripts/analysis-robustness.R", local = TRUE)
source("./scripts/analysis-revision.R", local = TRUE)

quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(out <- suppressMessages(suppressWarnings(expr))))
  out
}
show <- function(title, x) {
  cat("\n---", title, "---\n")
  if (is.data.frame(x)) print(dplyr::mutate(x, dplyr::across(where(is.numeric), ~ round(.x, 3))),
                              row.names = FALSE)
  else print(x, digits = 3)
}

# Data ---------------------------------------------------------------------

s1_raw <- quiet(prepare_raw_data("s1"))
s2_raw <- quiet(prepare_raw_data("s2"))
s1_filtered <- quiet(clean_data(s1_raw, "s1"))
s2_filtered <- quiet(clean_data(s2_raw, "s2"))
s1 <- quiet(build_study_data("s1", raw = s1_raw))
s2 <- quiet(build_study_data("s2", raw = s2_raw))

s1_contrasts <- list(c("High", "Low"), c("Medium", "Low"), c("High", "Medium"))
s2_contrasts <- list(c("High", "Low"))

composite_sds <- function(d) {
  sapply(c(author = "author", content = "content", author_meti = "author_meti",
           content_app = "content_app", anthropomorphism = "anthro"), function(v) sd(d[[v]]))
}

d_table <- function(d, dvs, contrasts) {
  dplyr::bind_rows(lapply(dvs, function(v) dplyr::bind_rows(
    lapply(contrasts, function(p) welch_with_d(d, v, p[1], p[2])))))
}

# ===========================================================================
cat("\n================ EXPERIMENT 1 ================\n")

# 4.2: missing values and composite SDs
show("Missing item responses before imputation", missing_item_summary(s1_filtered, "s1"))
show("Composite SDs (z-scored item means)", composite_sds(s1))

# 4.2.x: Welch tests with Cohen's d and normal-approximation 95% CIs
show("Welch t and Cohen's d [95% CI]",
     d_table(s1, c("anthro", "author", "content", "author_meti", "content_app",
                   "likeability", "competence", "writing_quality"), s1_contrasts))

# 4.2.3: author vs. content trust (mixed model and equivalent difference test)
show("Mixed model interaction, combined composites", trust_type_mixed_model(s1))
show("Mixed model interaction, validated scales", trust_type_mixed_model(s1, "author_meti", "content_app"))
show("Author-minus-content regression, combined", trust_difference_test(s1))
show("Author-minus-content regression, validated", trust_difference_test(s1, "author_meti", "content_app"))

# METI subscales (Appendix B.4): d [95% CI] and B [95% CI] with HC3 SEs
show("METI subscales, Cohen's d", d_table(s1, c("expertise", "integrity", "benevolence"),
                                          list(c("High", "Low"), c("Medium", "Low"))))
show("METI subscales, B (HC3)", dplyr::bind_rows(lapply(
  c("expertise", "integrity", "benevolence"),
  function(v) robust_condition_model(s1, v, character(0))$estimates)))

# Table 2: human vs. low under alternative specifications
s1_cc <- quiet(build_study_data("s1", impute = FALSE, raw = s1_raw))
s1_cc <- s1_cc[s1_cc$ResponseId %in% complete_case_ids(s1_filtered, "s1"), ]
show("Table 2 (human vs. low)", dplyr::bind_rows(
  robustness_row(s1, "Primary"),
  robustness_row(s1, "Validated scales only", scores = "validated"),
  robustness_row(s1_cc, "Complete cases"),
  robustness_row(s1[s1$Reread != 1, ], "Excluding rereaders"),
  robustness_row(s1, "Covariate-adjusted", covariates = robustness_covariates)))
show("Table 2 check: high vs. low never differed (Medium vs. Low)", dplyr::bind_rows(
  robustness_row(s1, "Primary", contrast = "Medium vs. Low"),
  robustness_row(s1, "Validated scales only", scores = "validated", contrast = "Medium vs. Low"),
  robustness_row(s1_cc, "Complete cases", contrast = "Medium vs. Low"),
  robustness_row(s1[s1$Reread != 1, ], "Excluding rereaders", contrast = "Medium vs. Low"),
  robustness_row(s1, "Covariate-adjusted", covariates = robustness_covariates,
                 contrast = "Medium vs. Low")))

# 4.2.4: which covariate drives the adjusted content trust effect
show("Content trust, adjusted for science-media frequency only",
     robust_condition_model(s1, "content", "science_frequency")$estimates)
show("Content trust, adjusted for all covariates except science-media frequency",
     robust_condition_model(s1, "content", setdiff(robustness_covariates, "science_frequency"))$estimates)
show("Joint test of condition, covariate-adjusted author trust",
     robust_condition_model(s1, "author", robustness_covariates)$joint)

# Appendix B.3: each composite standardized to SD = 1
show("Composite-standardized, combined", composite_standardized_models(s1))
show("Composite-standardized, combined, adjusted", composite_standardized_models(s1, robustness_covariates))
show("Composite-standardized, validated", composite_standardized_models(s1, scores = "validated"))

# Equivalence tests (Appendix B)
show("TOST, content trust", dplyr::bind_rows(
  tost_table(s1, "content", "High", "Low"),
  tost_table(s1, "content", "Medium", "Low"),
  tost_table(s1, "content_app", "High", "Low"),
  tost_table(s1, "content_app", "Medium", "Low")))

# Correlations, pooled and within condition, with the Williams test
show("Correlations", correlation_summary(s1))

# Sample balance and preregistered-style secondary correlations
show("Balance across conditions (Kruskal-Wallis p)", balance_tests(s1))
show("Secondary correlations (Spearman)", secondary_correlations(s1))
show("Exclusion flow by condition", exclusion_flow_by_condition(s1_raw, "s1"))

# ===========================================================================
cat("\n================ EXPERIMENT 2 ================\n")

show("Missing item responses before imputation", missing_item_summary(s2_filtered, "s2"))
show("Composite SDs (z-scored item means)", composite_sds(s2))
show("Exclusion flow by condition", exclusion_flow_by_condition(s2_raw, "s2"))
show("Balance across conditions (Kruskal-Wallis p)", balance_tests(s2))

# 5.1.1 and 5.2.4: power
show("Power", interaction_power(s2))

# 5.2.1: preregistered ANOVA vs. Student's t vs. Welch's t
show("Student vs. Welch vs. ANOVA", dplyr::bind_rows(lapply(c("anthro", "author", "content"),
                                                         function(v) compare_two_group_tests(s2, v))))

# 5.2.1 / Appendix B.5: preregistered exclusions only
s2_prereg <- quiet(build_study_data("s2", exclusions = "preregistered", raw = s2_raw))
show("Preregistered exclusions only: n per condition", table(s2_prereg$Condition))
show("Preregistered exclusions only: Welch t and d",
     d_table(s2_prereg, c("anthro", "author", "content", "author_meti", "content_app"), s2_contrasts))
show("Preregistered exclusions only: author-minus-content (HC3)",
     robust_trust_difference_model(s2_prereg, "author", "content", character(0))$estimates)

# 5.2.2-5.2.3: Welch tests with d
show("Welch t and Cohen's d [95% CI]",
     d_table(s2, c("anthro", "author", "content", "author_meti", "content_app",
                   "likeability", "competence", "writing_quality"), s2_contrasts))
show("Mixed model interaction, combined composites", trust_type_mixed_model(s2))
show("Author-minus-content regression, combined", trust_difference_test(s2))
show("Author-minus-content regression, validated", trust_difference_test(s2, "author_meti", "content_app"))
show("METI subscales, Cohen's d", d_table(s2, c("expertise", "integrity", "benevolence"), s2_contrasts))
show("METI subscales, B (HC3)", dplyr::bind_rows(lapply(
  c("expertise", "integrity", "benevolence"),
  function(v) robust_condition_model(s2, v, character(0))$estimates)))
show("TOST, content trust", dplyr::bind_rows(
  tost_table(s2, "content", "High", "Low"),
  tost_table(s2, "content_app", "High", "Low")))

# 5.2.4 / Table 4: high vs. low under alternative specifications
s2_cc <- quiet(build_study_data("s2", impute = FALSE, raw = s2_raw))
s2_cc <- s2_cc[s2_cc$ResponseId %in% complete_case_ids(s2_filtered, "s2"), ]
s2_retained <- quiet(build_study_data("s2", exclusions = "keep_openended", raw = s2_raw))
show("Table 4 (high vs. low)", dplyr::bind_rows(
  robustness_row(s2, "Primary"),
  robustness_row(s2, "Validated scales only", scores = "validated"),
  robustness_row(s2_cc, "Complete cases"),
  robustness_row(s2_retained, "Retaining excluded participant"),
  robustness_row(s2, "Covariate-adjusted", covariates = robustness_covariates)))
show("Composite-standardized, combined", composite_standardized_models(s2))
show("Composite-standardized, combined, adjusted", composite_standardized_models(s2, robustness_covariates))
show("Composite-standardized, validated", composite_standardized_models(s2, scores = "validated"))

# 5.2.5: conversations
responses <- chatbot_responses(s2)
flags <- conversation_flags(s2)
show("Conversation turns", conversation_turn_summary(flags))
show("Chatbot responses (clustered SEs)", chatbot_response_tests(responses))
show("Conversation flags (Fisher's exact)", conversation_flag_tests(flags))
show("Conversation flags by condition", flags |> dplyr::group_by(Condition) |>
       dplyr::summarise(dplyr::across(where(is.logical), sum), n = dplyr::n()))

# 5.2.6: correlations and Appendix B.6 preregistered secondary correlations
show("Correlations", correlation_summary(s2))
show("Secondary correlations (Spearman)", secondary_correlations(s2))
