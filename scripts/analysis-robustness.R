# Covariate-adjusted robustness models -----------------------------------
#
# Each model predicts a trust score from
# condition while adjusting for pre-existing participant characteristics,
# with HC3 heteroskedasticity-robust standard errors.
#
# Covariates (all measured before or independent of the manipulation):
#   Age_1                  age in years
#   female                 1 = female, 0 = any other response
#   post_secondary         1 = any post-secondary education (Education_regrouped)
#   ai_frequency           GenAI chatbot use, grouped (AIChatbotsFrequency_regrouped)
#   science_frequency      science-content reading, grouped (ScienceContent_regrouped)
#
# The frequency variables enter as three-level factors (frequently / occasionally /
# rarely), with "frequently" as the reference level. Post-manipulation ratings
# (likeability, perceived intelligence, writing quality) are deliberately NOT
# covariates, because the manipulation itself can change them.
#
# Requires: sandwich, lmtest, car

library(sandwich)
library(lmtest)
library(car)

robustness_covariates <- c("Age_1", "female", "post_secondary", "ai_frequency", "science_frequency")

# Use the z-scored composite when the pipeline provides one under a z_ prefix,
# otherwise the unprefixed composite (which is z-scored in the committed pipeline).
zscore_col <- function(data, base) {
  z_name <- paste0("z_", base)
  if (z_name %in% names(data)) z_name else base
}

prepare_robustness_covariates <- function(data) {
  data |>
    dplyr::mutate(
      female = as.integer(!is.na(Sex) & Sex == "Female"),
      post_secondary = as.integer(Education_regrouped == "post_secondary"),
      ai_frequency = factor(as.character(AIChatbotsFrequency_regrouped),
                            levels = c("frequently", "occasionally", "rarely")),
      science_frequency = factor(as.character(ScienceContent_regrouped),
                                 levels = c("frequently", "occasionally", "rarely"))
    )
}

# Fits dv ~ Condition (+ covariates) and returns HC3 estimates, 95% CIs and p
# values for each condition contrast, plus the joint test of condition.
robust_condition_model <- function(data, dv, covariates = robustness_covariates,
                                   reference = "Low") {
  data <- data |>
    dplyr::mutate(Condition = stats::relevel(factor(as.character(Condition)), ref = reference))
  rhs <- paste(c("Condition", covariates), collapse = " + ")
  fit <- stats::lm(stats::as.formula(paste(dv, "~", rhs)), data = data)
  vc <- sandwich::vcovHC(fit, type = "HC3")
  tests <- lmtest::coeftest(fit, vcov. = vc)
  cis <- lmtest::coefci(fit, vcov. = vc)
  terms <- grep("^Condition", rownames(tests), value = TRUE)
  estimates <- data.frame(
    dv = dv,
    contrast = paste(sub("^Condition", "", terms), "vs.", reference),
    B = unname(tests[terms, "Estimate"]),
    SE = unname(tests[terms, "Std. Error"]),
    ci_low = unname(cis[terms, 1]),
    ci_high = unname(cis[terms, 2]),
    t = unname(tests[terms, "t value"]),
    p = unname(tests[terms, "Pr(>|t|)"]),
    n = stats::nobs(fit),
    row.names = NULL
  )
  joint <- car::linearHypothesis(fit, terms, vcov. = vc, test = "F")
  list(
    fit = fit,
    estimates = estimates,
    joint = data.frame(dv = dv, F = joint$F[2], df1 = joint$Df[2],
                       df2 = joint$Res.Df[2], p = joint$`Pr(>F)`[2])
  )
}

# The condition-by-trust-type interaction in the mixed model is equivalent to
# regressing each participant's author-minus-content difference on condition.
# This version allows the same covariate adjustment as the main models.
robust_trust_difference_model <- function(data, author_col, content_col,
                                          covariates = robustness_covariates,
                                          reference = "Low") {
  data$author_minus_content <- data[[author_col]] - data[[content_col]]
  robust_condition_model(data, "author_minus_content", covariates, reference)
}

# Runs the unadjusted and adjusted models for author trust, content trust and
# their difference, for both the combined and the validated-only composites.
run_robustness_models <- function(data, reference = "Low") {
  data <- prepare_robustness_covariates(data)
  sets <- list(
    combined = c(author = zscore_col(data, "author_trust_combined_score"),
                 content = zscore_col(data, "content_trust_combined_score")),
    validated = c(author = zscore_col(data, "author_trust_METI_score"),
                  content = zscore_col(data, "content_trust_appelman_score"))
  )
  rows <- list()
  joints <- list()
  for (set_name in names(sets)) {
    cols <- sets[[set_name]]
    for (adjusted in c(FALSE, TRUE)) {
      covs <- if (adjusted) robustness_covariates else character(0)
      fits <- list(
        author = robust_condition_model(data, cols[["author"]], covs, reference),
        content = robust_condition_model(data, cols[["content"]], covs, reference),
        difference = robust_trust_difference_model(data, cols[["author"]], cols[["content"]],
                                                   covs, reference)
      )
      for (outcome in names(fits)) {
        est <- fits[[outcome]]$estimates
        est$outcome <- outcome
        est$scores <- set_name
        est$adjusted <- adjusted
        rows[[length(rows) + 1]] <- est
        jt <- fits[[outcome]]$joint
        jt$outcome <- outcome
        jt$scores <- set_name
        jt$adjusted <- adjusted
        joints[[length(joints) + 1]] <- jt
      }
    }
  }
  list(
    estimates = dplyr::bind_rows(rows) |>
      dplyr::select(scores, adjusted, outcome, contrast, B, SE, ci_low, ci_high, t, p, n),
    joint = dplyr::bind_rows(joints) |>
      dplyr::select(scores, adjusted, outcome, F, df1, df2, p)
  )
}
