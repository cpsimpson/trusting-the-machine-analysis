# library(apa)
# library(effectsize)
# library(rstatix)
# library(performance)

test_between_subjects_anova_assumptions <- function(data, formula, aov_model, dv_name){
  message("Starting test_between_subjects_anova_assumptions")
  dv <- data |> pull({{dv_name}})
  
  # # Test for Normality
  hist(dv)

  qqnorm(aov_model$residuals)
  qqline(aov_model$residuals)

  plot(aov_model)
  # 
  print(shapiro.test(dv))

  # Test for equal variance
  boxplot(formula, data = data)
  
  print(bartlett.test(formula, data = data))
  
  return()
  
}

test_within_subjects_anova_assumptions <- function(aov_model){
  message("Starting test_within_subjects_anova_assumptions")
  performance::check_homogeneity(aov_model)
  performance::check_sphericity(aov_model)
}

# run_simple_effects_t_tests <- function(data, formula){
#   data <- droplevels(data)  # remove unused factor levels
#   vars <- all.vars(formula)
#   dv <- vars[1]
#   iv <- vars[2]
#   
#   # Check that IV has at least 2 levels
#   if (length(unique(data[[iv]])) < 2) {
#     warning("Not enough levels in grouping variable for t-test.")
#     return(NULL)
#   }
#   
#   result <- rstatix::pairwise_t_test(
#     data = data,
#     formula = as.formula(paste(dv, "~", iv)),
#     p.adjust.method = "bonferroni",
#     detailed = TRUE,
#     paired = FALSE
#   )
#   
#   print(rstatix::cohens_d(
#     data = data,
#     formula = as.formula(paste(dv, "~", iv))
#   ))
#   
#   return(result)
# }

run_simple_effects_t_tests <- function(data, formula) {
  message("Starting run_simple_effects_t_tests")
  data <- base::droplevels(data)
  vars <- all.vars(formula)
  dv <- vars[1]
  iv <- vars[2]
  
  # Make sure grouping variable is a character for matching
  data[[iv]] <- as.character(data[[iv]])
  
  # Get all pairs with sufficient data (at least 2 non-NA per group)
  valid_groups <- data |>
    dplyr::group_by(.data[[iv]]) |>
    dplyr::summarise(n = sum(!is.na(.data[[dv]])), .groups = "drop") |>
    dplyr::filter(n >= 2) |>
    dplyr::pull(.data[[iv]])
  
  # Only keep data for valid groups
  filtered_data <- data |>
    dplyr::filter(.data[[iv]] %in% valid_groups)
  
  # If fewer than 2 groups remain, skip
  if (length(unique(filtered_data[[iv]])) < 2) {
    warning("Not enough valid groups to perform t-tests.")
    return(NULL)
  }
  
  # Run pairwise t-tests (now guaranteed to be safe)
  t_test_results <- rstatix::pairwise_t_test(
    data = filtered_data,
    formula = formula,
    detailed = TRUE,
    pool.sd = FALSE,
    p.adjust.method = "bonferroni"
  )
  
  print(t_test_results |> rmarkdown::paged_table())
  
  # Compute Cohen's d with same filtered data
  d_results <- dplyr::rowwise(t_test_results) |>
    dplyr::mutate(
      effsize_data = list(
        tryCatch({
          rstatix::cohens_d(
            data = filtered_data |>
              dplyr::filter(.data[[iv]] %in% c(group1, group2)),
            formula = formula,
            ci = TRUE
          )
        }, error = function(e) {
          tibble::tibble(
            effsize = NA_real_,
            effsize_conf.low = NA_real_,
            effsize_conf.high = NA_real_
          )
        })
      )
    ) |>
    tidyr::unnest_wider(effsize_data, names_sep = "_")
  
  print(d_results  |> rmarkdown::paged_table())
  
  return(d_results)
}

run_between_subjects_anova <- function(data, formula, ...){
  message("Starting run_between_subjects_anova")
  dv_name <- all.vars(formula)[1]
  grouping_var <- all.vars(formula)[2]
  
  # Count participants per group
  group_counts <- data |> count(.data[[grouping_var]])
  
  # Identify groups with <= 3 participants
  low_n_groups <- group_counts |> filter(n <= 3) |> pull(!!sym(grouping_var))
  
  # Drop low-n groups
  data <- data |> filter(!.data[[grouping_var]] %in% low_n_groups)
  
  if (length(low_n_groups) > 0) {
    message("Dropped groups with 3 or fewer participants in", grouping_var, ":", paste(low_n_groups, collapse = ", "), "\n")
  }
  
  aov_model <- aov(data = data, formula = formula)
  
  model_summ <- summary(aov_model)
  print(model_summ)
  
  print(anova_apa(aov_model))
  
  print(paste0("MSE = ", model_summ[[1]]["Mean Sq"][[1]][1]))
  
  print(effectsize::omega_squared(aov_model))
  
  # Extract p-value from the ANOVA summary
  p_value <- model_summ[[1]]["Pr(>F)"][[1]][1]
  
  message("The ANOVA p value is", p_value)
  # If the p-value is significant, run assumption tests and simple effects t-tests
  if (!is.na(p_value) && p_value < 0.05) {
    test_between_subjects_anova_assumptions(data, formula, aov_model, dv_name)
    pwt_results <- run_simple_effects_t_tests(data, formula)
    print(pwt_results)
  }
  
  return(aov_model)
}



run_inferential <- function(data, formula, share_fraction = 1 / 10) {
  
  message("Starting run_inferential.")
  dv_name <- all.vars(formula)[1]
  grouping_var <- all.vars(formula)[2]
  
  # Step 1: Drop rare levels by share
  cleaned <- drop_rare_levels_by_share(data, grouping_var, share_fraction)
  data <- cleaned$data
  dropped_levels <- cleaned$dropped_levels
  
  if (length(dropped_levels) > 0) {
    message("Dropped groups with low share of participants in ", grouping_var, ": ", paste(dropped_levels, collapse = ", "))
  }
  
  data <- droplevels(data)
  
  # Count participants per group
  group_counts <- data |> count(.data[[grouping_var]])
  low_n_groups <- group_counts |> filter(n <= 3) |> pull(!!sym(grouping_var))
  
  # Filter out low-n groups
  data <- data |> filter(!.data[[grouping_var]] %in% low_n_groups)
  
  if (length(low_n_groups) > 0) {
    message("Dropped groups with 3 or fewer participants in ", grouping_var, ": ", paste(low_n_groups, collapse = ", "))
  }
  
  # Determine number of levels in grouping variable
  num_levels <- length(unique(data[[grouping_var]]))
  
  message("There are ", num_levels, " levels in ", grouping_var)
  
  if (num_levels == 2) {
    message("Running independent samples t-test for ", dv_name, " ~ ", grouping_var)
    
    # t_result <- t.test(
    #   formula,
    #   data = data,
    #   var.equal = TRUE
    # )
    
    t_result <- rstatix::pairwise_t_test(
      data = data,
      formula = formula,
      detailed = TRUE,
      pool.sd = FALSE,
      p.adjust.method = "bonferroni"
    )
    
    d_result <- rstatix::cohens_d(data = data, formula = formula, ci = TRUE)
    
    print(t_result)
    print(d_result)
    
    return(invisible(list(
      test = "t-test",
      model = t_result,
      effect_size = d_result
    )))
  } else if (num_levels > 2) {
    message("Running ANOVA for ", dv_name, " ~ ", grouping_var)
    
    aov_model <- aov(data = data, formula = formula)
    tukey <- TukeyHSD(aov_model)
    
    
    model_summ <- summary(aov_model)
    apa_output <- anova_apa(aov_model)
    omega_sq <- effectsize::omega_squared(aov_model)
    
    print(model_summ)
    print(apa_output)
    print(paste0("MSE = ", model_summ[[1]]["Mean Sq"][[1]][1]))
    print(omega_sq)
    
    # Extract p-value
    p_value <- model_summ[[1]]["Pr(>F)"][[1]][1]
    
    message("The ANOVA p value is", p_value)
    # If the p-value is significant, run assumption tests and simple effects t-tests
    if (!is.na(p_value) && p_value < 0.05) {
      message("ANOVA was significant, checking assumptioins.")
      test_between_subjects_anova_assumptions(data, formula, aov_model, dv_name)
      
      message("ANOVA was significant, running pairwise t-tests.")
      pwt_result <- run_simple_effects_t_tests(data, formula)
      pwt_result |> rmarkdown::paged_table() |> print()
    } else {
      pwt_result <- NA
    }
    
    return(invisible(list(
      test = "ANOVA",
      model = aov_model,
      summary = model_summ,
      apa = apa_output,
      omega_squared = omega_sq,
      pwt_result = pwt_result,
      tukey = tukey
    )))
  } else {
    message("Not enough levels in grouping variable after filtering to run inferential stats for ", grouping_var, " on ", dv_name)
  }
}
