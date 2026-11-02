# Revision analyses (CHB Reports) ------------------------------------------
#
# Functions for the analyses added or changed in the CHB Reports revision.
# Source after scripts/data-loading.R, scripts/data-cleaning.R and
# scripts/analysis-robustness.R. revision-analyses.R runs them in manuscript
# order.
#
# Requires: tidyverse, lme4, lmerTest, sandwich, lmtest, car, pwr, jsonlite
#
# Condition labels in the data vs. the manuscript:
#   Experiment 1: Low = low, Medium = high, High = human
#   Experiment 2: Low = low, High = high

library(lme4)
library(lmerTest)
library(pwr)
library(jsonlite)

# Data preparation ---------------------------------------------------------

# Composite column for a score name. Works with both versions of
# compute_scores(): z-scored composites prefixed "z_" or unprefixed.
score_col <- function(data, base) {
  z_name <- paste0("z_", base)
  if (z_name %in% names(data)) z_name else base
}

# Adds short names for the composites used throughout.
add_short_scores <- function(data) {
  data$anthro      <- data[[score_col(data, "anthropomorphism_score")]]
  data$author      <- data[[score_col(data, "author_trust_combined_score")]]
  data$content     <- data[[score_col(data, "content_trust_combined_score")]]
  data$author_meti <- data[[score_col(data, "author_trust_METI_score")]]
  data$content_app <- data[[score_col(data, "content_trust_appelman_score")]]
  data$expertise   <- data[[score_col(data, "expertise_score")]]
  data$integrity   <- data[[score_col(data, "integrity_score")]]
  data$benevolence <- data[[score_col(data, "benevolence_score")]]
  data$likeability <- data[[score_col(data, "likeability_score")]]
  data$competence  <- data[[score_col(data, "competence_score")]]
  data$writing_quality <- data[[score_col(data, "writing_quality_score")]]
  data
}

# Loads and pre-filters raw data exactly as the analysis notebooks do.
prepare_raw_data <- function(study) {
  raw <- load_data(study)
  raw <- factorize_data(study, raw)
  raw <- explode_chatbots_used(raw)
  if (study == "s1") raw <- explode_languages(raw)
  raw <- exclude_non_consenting_participants(raw)
  if (study == "s2") raw <- deduplicate_participants(raw)
  raw
}

# Imputes, recodes, standardizes, regroups and scores a filtered data set.
# Handles both standardize_data() versions (with or without a method argument).
score_data <- function(filtered, study, impute = TRUE) {
  d <- if (impute) impute_data(filtered) else filtered
  d <- recode_data(d, study)
  if ("method" %in% names(formals(standardize_data))) {
    d <- d |> standardize_data(method = "z-score") |> standardize_data(method = "min-max")
  } else {
    d <- standardize_data(d)
  }
  d <- d |> regroup_data() |> compute_scores(study)
  d |> add_short_scores() |> prepare_robustness_covariates()
}

# Builds the analysis sample.
#   exclusions = "standard"        the manuscript's exclusion rules (clean_data)
#              = "keep_openended"  Exp 2: retains the participant who said they
#                                  had not read the blog post
#              = "preregistered"   Exp 2: only the preregistered rules
#   impute = FALSE keeps missing values (for complete-case analyses)
build_study_data <- function(study, exclusions = "standard", impute = TRUE, raw = NULL) {
  if (is.null(raw)) raw <- prepare_raw_data(study)
  filtered <- switch(
    exclusions,
    standard = clean_data(raw, study),
    keep_openended = {
      raw$Unrealistic_coded <- forcats::fct_recode(raw$Unrealistic_coded,
                                                   other = "Didn't follow instructions")
      clean_data(raw, study)
    },
    preregistered = apply_preregistered_exclusions(raw)
  )
  score_data(filtered, study, impute = impute)
}

# Experiment 2 preregistered exclusions (AsPredicted #215144): incomplete
# surveys, failed attention checks, reCAPTCHA below 0, and random characters
# in free-text fields (coded "Junk Data" in either open-ended question).
apply_preregistered_exclusions <- function(raw) {
  raw |>
    dplyr::filter(Progress == 100) |>
    dplyr::filter(GodspeedMETI_28 == 20, GodspeedMETI_29 == 67) |>
    dplyr::filter(Q_RecaptchaScore >= 0) |>
    dplyr::filter(!(Unrealistic_coded %in% "Junk Data"),
                  !(SurveyTopicCheck_coded %in% "Junk Data"))
}

# Exclusion flow by condition (Appendix B), in the same order as clean_data().
exclusion_flow_by_condition <- function(raw, study) {
  d <- raw
  if (study == "s1") d <- dplyr::filter(d, EndDate >= "2024-12-04")
  rows <- list()
  step <- function(keep, name) {
    keep <- !is.na(keep) & keep
    rows[[length(rows) + 1]] <<- data.frame(step = name, t(as.matrix(table(d$Condition[!keep]))),
                                            check.names = FALSE)
    d <<- d[keep, ]
  }
  rows[[1]] <- data.frame(step = "start", t(as.matrix(table(d$Condition))), check.names = FALSE)
  step(d$Progress == 100, "incomplete")
  step(d$ConsentForm == 1, "no consent")
  step(d$Status == 0, "preview link")
  step(d$Q_RecaptchaScore >= 0.5, "reCAPTCHA < 0.5")
  step(d$`Duration (in seconds)` >= median(d$`Duration (in seconds)`, na.rm = TRUE) / 3, "too fast")
  step(d$TopicCheck == 1, "comprehension check")
  if (study == "s1") {
    step((d$AgentTypeCheck == 1 & d$Condition %in% c("Low", "Medium")) |
           (d$AgentTypeCheck == 2 & d$Condition == "High"), "agent type check")
    step(d$GodspeedMETI_28 == 100 & d$GodspeedMETI_29 == 0, "attention checks")
  } else {
    step(d$GodspeedMETI_28 == 20 & d$GodspeedMETI_29 == 67, "attention checks")
    step(d$Unrealistic_coded != "Didn't follow instructions", "did not read blog post")
  }
  rows[[length(rows) + 1]] <- data.frame(step = "final", t(as.matrix(table(d$Condition))),
                                         check.names = FALSE)
  dplyr::bind_rows(rows)
}

# Item responses that were missing before imputation (Section 4.2 / 5.2.1).
missing_item_summary <- function(filtered, study) {
  items <- grep("^(GodspeedMETI_([1-9]|1[0-9]|2[0-6])|Appelman_[1-6]|TrustBehaviour_[0-9]+)$",
                names(filtered), value = TRUE)
  if (study == "s2") items <- setdiff(items, "GodspeedMETI_12")
  na <- is.na(filtered[, items])
  list(n_participants = nrow(filtered),
       n_items = length(items),
       missing_responses = sum(na),
       participants_with_missing = sum(rowSums(na) > 0),
       missing_by_item = colSums(na)[colSums(na) > 0])
}

# IDs of participants with no missing items (for complete-case analyses).
complete_case_ids <- function(filtered, study) {
  items <- grep("^(GodspeedMETI_([1-9]|1[0-9]|2[0-6])|Appelman_[1-3]|TrustBehaviour_[0-9]+)$",
                names(filtered), value = TRUE)
  if (study == "s2") items <- setdiff(items, "GodspeedMETI_12")
  filtered$ResponseId[stats::complete.cases(filtered[, items])]
}

# Effect sizes -------------------------------------------------------------

# Cohen's d (pooled SD) with a large-sample normal-approximation 95% CI:
# SE = sqrt((n1 + n2) / (n1 n2) + d^2 / (2 (n1 + n2))).
cohens_d_ci <- function(x, y, level = 0.95) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  n1 <- length(x); n2 <- length(y)
  sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  d <- (mean(x) - mean(y)) / sp
  se <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(d = d, d_low = d - z * se, d_high = d + z * se, n1 = n1, n2 = n2)
}

# Welch's t-test with Cohen's d and its CI for condition a vs. condition b.
welch_with_d <- function(data, dv, a, b) {
  x <- data[[dv]][data$Condition == a]
  y <- data[[dv]][data$Condition == b]
  tt <- stats::t.test(x, y)
  cbind(data.frame(dv = dv, contrast = paste(a, "vs.", b),
                   t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value),
        cohens_d_ci(x, y))
}

# Student's t, Welch's t and one-way ANOVA side by side (Exp 2; Section 5.2.1).
compare_two_group_tests <- function(data, dv) {
  dd <- data.frame(y = data[[dv]], g = droplevels(factor(data$Condition)))
  s <- stats::t.test(y ~ g, data = dd, var.equal = TRUE)
  w <- stats::t.test(y ~ g, data = dd)
  a <- stats::anova(stats::lm(y ~ g, data = dd))
  data.frame(dv = dv,
             student_t = unname(s$statistic), student_df = unname(s$parameter), student_p = s$p.value,
             welch_t = unname(w$statistic), welch_df = unname(w$parameter), welch_p = w$p.value,
             anova_F = a$`F value`[1], anova_p = a$`Pr(>F)`[1])
}

# Author vs. content trust ---------------------------------------------------

# Mixed model: trust ~ condition * trust type + (1 | participant).
trust_type_mixed_model <- function(data, author = "author", content = "content",
                                   reference = "Low") {
  long <- data |>
    dplyr::mutate(Condition = stats::relevel(factor(as.character(Condition)), ref = reference)) |>
    dplyr::select(ResponseId, Condition, author = dplyr::all_of(author),
                  content = dplyr::all_of(content)) |>
    tidyr::pivot_longer(c(author, content), names_to = "trust_type", values_to = "trust") |>
    dplyr::mutate(trust_type = factor(trust_type, levels = c("content", "author")))
  fit <- lmerTest::lmer(trust ~ Condition * trust_type + (1 | ResponseId), data = long)
  coefs <- summary(fit)$coefficients
  coefs[grepl(":", rownames(coefs)), , drop = FALSE]
}

# The same interaction as an OLS regression of author-minus-content on
# condition (identical t and p with one observation of each type per person).
trust_difference_test <- function(data, author = "author", content = "content",
                                  reference = "Low") {
  data$difference <- data[[author]] - data[[content]]
  data$Condition <- stats::relevel(factor(as.character(data$Condition)), ref = reference)
  summary(stats::lm(difference ~ Condition, data = data))$coefficients
}

# Robustness table row (Tables 2 and 4): author, content and difference,
# each as B [95% CI] with HC3 SEs, for one contrast.
robustness_row <- function(data, label, scores = c("combined", "validated"),
                           covariates = character(0), contrast = "High vs. Low") {
  scores <- match.arg(scores)
  a <- if (scores == "combined") "author" else "author_meti"
  c <- if (scores == "combined") "content" else "content_app"
  fits <- list(author = robust_condition_model(data, a, covariates),
               content = robust_condition_model(data, c, covariates),
               difference = robust_trust_difference_model(data, a, c, covariates))
  out <- lapply(names(fits), function(nm) {
    e <- fits[[nm]]$estimates
    e <- e[e$contrast == contrast, ]
    data.frame(specification = label, n = e$n, outcome = nm, contrast = contrast,
               B = e$B, ci_low = e$ci_low, ci_high = e$ci_high, p = e$p)
  })
  dplyr::bind_rows(out)
}

# Composite-standardized comparison (Appendix B.3): each composite rescaled
# to SD = 1 before testing condition and the author-minus-content difference.
composite_standardized_models <- function(data, covariates = character(0),
                                          scores = c("combined", "validated")) {
  scores <- match.arg(scores)
  a <- if (scores == "combined") "author" else "author_meti"
  c <- if (scores == "combined") "content" else "content_app"
  data$A_std <- as.numeric(scale(data[[a]]))
  data$C_std <- as.numeric(scale(data[[c]]))
  list(author = robust_condition_model(data, "A_std", covariates)$estimates,
       content = robust_condition_model(data, "C_std", covariates)$estimates,
       difference = robust_trust_difference_model(data, "A_std", "C_std", covariates)$estimates)
}

# Equivalence tests ----------------------------------------------------------

# Two one-sided tests (Welch SE and df) with bounds given in pooled-SD units.
# Returns the TOST p value and the 90% CI of the difference in d units.
tost_d <- function(x, y, bound_d) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  n1 <- length(x); n2 <- length(y)
  sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  bound <- bound_d * sp
  diff <- mean(x) - mean(y)
  v1 <- var(x) / n1; v2 <- var(y) / n2
  se <- sqrt(v1 + v2)
  df <- (v1 + v2)^2 / (v1^2 / (n1 - 1) + v2^2 / (n2 - 1))
  p_lower <- 1 - stats::pt((diff + bound) / se, df)
  p_upper <- stats::pt((diff - bound) / se, df)
  ci <- diff + c(-1, 1) * stats::qt(0.95, df) * se
  data.frame(bound_d = bound_d, d = diff / sp, ci90_low = ci[1] / sp, ci90_high = ci[2] / sp,
             p_tost = max(p_lower, p_upper))
}

tost_table <- function(data, dv, a, b, bounds = c(0.20, 0.30, 0.40, 0.50)) {
  x <- data[[dv]][data$Condition == a]
  y <- data[[dv]][data$Condition == b]
  out <- dplyr::bind_rows(lapply(bounds, function(bd) tost_d(x, y, bd)))
  cbind(dv = dv, contrast = paste(a, "vs.", b), out)
}

# Correlations ---------------------------------------------------------------

pearson_with_ci <- function(x, y) {
  ct <- stats::cor.test(x, y)
  data.frame(r = unname(ct$estimate), df = unname(ct$parameter), p = ct$p.value,
             ci_low = ct$conf.int[1], ci_high = ct$conf.int[2])
}

# Correlations after centering each variable on its condition mean, so that
# condition differences produced by the manipulation do not drive them.
center_within_condition <- function(data, vars) {
  data |>
    dplyr::group_by(Condition) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~ .x - mean(.x, na.rm = TRUE))) |>
    dplyr::ungroup()
}

# Williams's t for two dependent correlations sharing variable 1:
# r12 vs. r13, given r23 (Steiger, 1980, Eq. 2). df = n - 3.
williams_test <- function(r12, r13, r23, n) {
  det_r <- 1 - r12^2 - r13^2 - r23^2 + 2 * r12 * r13 * r23
  r_bar <- (r12 + r13) / 2
  t <- (r12 - r13) * sqrt(((n - 1) * (1 + r23)) /
                            (2 * ((n - 1) / (n - 3)) * det_r + r_bar^2 * (1 - r23)^3))
  df <- n - 3
  data.frame(r12 = r12, r13 = r13, r23 = r23, t = t, df = df,
             p = 2 * stats::pt(-abs(t), df))
}

# Correlation after centering on condition means. Each removed condition mean
# costs one degree of freedom, so df = N - k - 1 and the Fisher-z CI uses
# N - k - 2 (k = number of conditions).
pearson_within <- function(x, y, n, k) {
  r <- stats::cor(x, y)
  df <- n - k - 1
  t <- r * sqrt(df / (1 - r^2))
  se <- 1 / sqrt(n - k - 2)
  data.frame(r = r, df = df, p = 2 * stats::pt(-abs(t), df),
             ci_low = tanh(atanh(r) - stats::qnorm(0.975) * se),
             ci_high = tanh(atanh(r) + stats::qnorm(0.975) * se))
}

# Pooled and within-condition correlations among anthropomorphism, author
# trust and content trust, plus the Williams test of whether anthropomorphism
# relates more strongly to author than to content trust. The within-condition
# Williams test uses an effective n of N - k + 1 (df = N - k - 2).
correlation_summary <- function(data) {
  pairs <- list(anthro_author = c("anthro", "author"),
                anthro_content = c("anthro", "content"),
                author_content = c("author", "content"))
  n <- nrow(data)
  k <- nlevels(droplevels(factor(data$Condition)))
  pooled <- dplyr::bind_rows(lapply(pairs, function(p) pearson_with_ci(data[[p[1]]], data[[p[2]]])),
                             .id = "pair")
  centered <- center_within_condition(data, c("anthro", "author", "content"))
  within <- dplyr::bind_rows(lapply(pairs, function(p) pearson_within(centered[[p[1]]], centered[[p[2]]], n, k)),
                             .id = "pair")
  w <- function(d, n_eff) williams_test(stats::cor(d$anthro, d$author), stats::cor(d$anthro, d$content),
                                        stats::cor(d$author, d$content), n_eff)
  list(pooled = pooled, within = within,
       williams_pooled = w(data, n), williams_within = w(centered, n - k + 1))
}

# Frequency questions (GenAI use, media similar to the blog post) as an
# 8-point ordinal score, 7 = more than once a day ... 0 = never. The factor
# lists "Less than once a month" before "About once a month", so those two
# are swapped to keep the order monotonic.
frequency_score <- function(x) {
  code <- as.integer(x)
  code <- ifelse(code == 5, 6L, ifelse(code == 6, 5L, code))
  8L - code
}

# Preregistered secondary correlations (Exp 2; reported for both): Spearman
# correlations of sample characteristics with the main measures.
secondary_correlations <- function(data) {
  data$woman <- as.integer(!is.na(data$Gender) & data$Gender == "Woman")
  data$ai_use <- frequency_score(data$AIChatbotsFrequency)
  data$science_media <- frequency_score(data$ScienceContent)
  chars <- c(age = "Age_1", female = "female", woman = "woman",
             ai_use = "ai_use", science_media = "science_media")
  out <- list()
  for (nm in names(chars)) for (y in c("anthro", "author", "content")) {
    ct <- suppressWarnings(stats::cor.test(data[[chars[[nm]]]], data[[y]], method = "spearman"))
    out[[length(out) + 1]] <- data.frame(characteristic = nm, measure = y,
                                         rho = unname(ct$estimate), p = ct$p.value)
  }
  dplyr::bind_rows(out)
}

# Balance of pre-existing characteristics across conditions (Kruskal-Wallis).
balance_tests <- function(data) {
  data$ai_use <- frequency_score(data$AIChatbotsFrequency)
  data$science_media <- frequency_score(data$ScienceContent)
  vars <- c(age = "Age_1", female = "female", post_secondary = "post_secondary",
            ai_use = "ai_use", science_media = "science_media")
  data.frame(characteristic = names(vars),
             p = sapply(vars, function(v) stats::kruskal.test(as.numeric(data[[v]]) ~ data$Condition)$p.value),
             row.names = NULL)
}

# Power ----------------------------------------------------------------------

# Exp 2: smallest d detectable with 80% power at the final group sizes, the
# observed size of the author-minus-content interaction in SD units of the
# difference score, power to detect it, and n per group for 80% power.
interaction_power <- function(data, a = "High", b = "Low") {
  n_per_group <- min(table(droplevels(factor(data$Condition))))
  diff <- data$author - data$content
  d_int <- cohens_d_ci(diff[data$Condition == a], diff[data$Condition == b])$d
  list(n_per_group = n_per_group,
       detectable_d = pwr::pwr.t.test(n = n_per_group, power = 0.80)$d,
       interaction_d = d_int,
       interaction_power = pwr::pwr.t.test(n = n_per_group, d = abs(d_int))$power,
       n_for_80_power = ceiling(pwr::pwr.t.test(d = abs(d_int), power = 0.80)$n))
}

# Chatbot conversations (Exp 2, Section 5.2.5) ------------------------------

emotion_pattern <- paste0("\\b(excit\\w*|happy|glad|love|thrilled|sorry|feel\\w*|wonderful|",
                          "amazing|fascinat\\w*|delight\\w*|understand how you|that sounds|",
                          "i hear you|hope)\\b")
count_matches <- function(text, pattern) {
  lengths(regmatches(text, gregexpr(pattern, text, ignore.case = TRUE, perl = TRUE)))
}

# One row per chatbot response.
chatbot_responses <- function(data) {
  rows <- lapply(seq_len(nrow(data)), function(i) {
    h <- jsonlite::fromJSON(data$ChatHistory[i], simplifyVector = TRUE)
    bot <- h$content[h$role == "assistant"]
    if (length(bot) == 0) return(NULL)
    words <- lengths(strsplit(trimws(bot), "\\s+"))
    data.frame(ResponseId = data$ResponseId[i], Condition = data$Condition[i],
               words = words,
               exclamations = count_matches(bot, "!"),
               emotion_words = count_matches(bot, emotion_pattern),
               first_person_per_100 = count_matches(bot, "\\b(i|i'm|i've|i'd|me|my)\\b") / pmax(words, 1) * 100,
               second_person_per_100 = count_matches(bot, "\\b(you|your|you're)\\b") / pmax(words, 1) * 100)
  })
  dplyr::bind_rows(rows)
}

# Response-level comparisons with SEs clustered by participant.
chatbot_response_tests <- function(responses, reference = "Low") {
  responses$Condition <- stats::relevel(factor(as.character(responses$Condition)), ref = reference)
  vars <- c("words", "exclamations", "emotion_words", "first_person_per_100", "second_person_per_100")
  dplyr::bind_rows(lapply(vars, function(v) {
    fit <- stats::lm(stats::as.formula(paste(v, "~ Condition")), data = responses)
    vc <- sandwich::vcovCL(fit, cluster = ~ ResponseId)
    n_clusters <- length(unique(responses$ResponseId))
    ct <- lmtest::coeftest(fit, vcov. = vc, df = n_clusters - 1)
    term <- grep("^Condition", rownames(ct), value = TRUE)
    means <- tapply(responses[[v]], responses$Condition, mean)
    data.frame(measure = v, mean_low = means[[reference]],
               mean_high = means[[setdiff(names(means), reference)]],
               B = ct[term, "Estimate"], t = ct[term, "t value"], df = n_clusters - 1,
               p = ct[term, "Pr(>|t|)"])
  }))
}

# One row per conversation: what was discussed (keyword flags).
conversation_flags <- function(data) {
  rows <- lapply(seq_len(nrow(data)), function(i) {
    h <- jsonlite::fromJSON(data$ChatHistory[i], simplifyVector = TRUE)
    user <- paste(h$content[h$role == "user"], collapse = " ")
    bot <- paste(h$content[h$role == "assistant"], collapse = " ")
    data.frame(
      ResponseId = data$ResponseId[i], Condition = data$Condition[i],
      user_turns = sum(h$role == "user"), bot_turns = sum(h$role == "assistant"),
      participant_asked_about_post = grepl("veloc|blog|drug|medic|trial|study|tnf|arthrit|inflam",
                                           user, ignore.case = TRUE),
      chatbot_mentioned_velocerin = grepl("velocerin", bot, ignore.case = TRUE),
      participant_asked_identity = grepl(paste0("(did you|who) (write|wrote)|author|",
                                                "are you (an? )?(ai|human|real|bot)|who are you|what are you"),
                                         user, ignore.case = TRUE),
      chatbot_fallback_reply = grepl("do not have enough information|i'm not sure right now",
                                     bot, ignore.case = TRUE))
  })
  dplyr::bind_rows(rows)
}

# Participant messages per conversation and messages without a recorded reply.
conversation_turn_summary <- function(flags) {
  user_turns <- split(flags$user_turns, flags$Condition)
  unanswered <- pmax(flags$user_turns - flags$bot_turns, 0)
  list(participant_messages = sum(flags$user_turns),
       chatbot_responses = sum(flags$bot_turns),
       median_messages = tapply(flags$user_turns, flags$Condition, stats::median),
       mean_messages = tapply(flags$user_turns, flags$Condition, mean),
       welch_p_messages = stats::t.test(user_turns[[1]], user_turns[[2]])$p.value,
       messages_without_reply = sum(unanswered),
       participants_with_unanswered = sum(unanswered > 0))
}

conversation_flag_tests <- function(flags) {
  vars <- c("participant_asked_about_post", "chatbot_mentioned_velocerin",
            "participant_asked_identity", "chatbot_fallback_reply")
  dplyr::bind_rows(lapply(vars, function(v) {
    tab <- table(flags$Condition, factor(flags[[v]], levels = c(FALSE, TRUE)))
    data.frame(flag = v, n_true = sum(flags[[v]]), n = nrow(flags),
               p_fisher = stats::fisher.test(tab)$p.value)
  }))
}
