



# Helper: drop rare levels by proportion
drop_rare_levels_by_share <- function(data, vars, share_fraction) {
  dropped_levels <- list()
  total_n <- nrow(data)
  for (v in vars) {
    if (is.factor(data[[v]]) || is.character(data[[v]])) {
      var <- as.factor(data[[v]])
      counts <- table(var)
      k <- length(counts)
      expected_share <- 1 / k
      drop_threshold <- expected_share * share_fraction
      proportions <- counts / sum(counts)
      to_drop <- names(proportions[proportions < drop_threshold])
      if (length(to_drop) > 0) {
        message(
          "⚠️ Dropping rows with low-share levels in '",
          v,
          "': ",
          paste(to_drop, collapse = ", "),
          " (< ",
          round(drop_threshold * 100, 1),
          "% of total)"
        )
        dropped_levels[[v]] <- to_drop
        data <- data[!data[[v]] %in% to_drop, ]
        data[[v]] <- droplevels(factor(data[[v]]))
      }
    }
  }
  list(data = data, dropped_levels = dropped_levels)
}

# Helper: Format readable variable names using APA-friendly mapping
format_variable_name <- Vectorize(function(x) {
  label_map <- list(
    AIChatbotsFrequency_regrouped = "AI chatbot use frequency",
    AIChatbotsFrequency = "AI chatbot use frequency",
    Condition = "Anthropomorphism condition",
    Condition_Compressed = "Anthropomorphism condition",
    likeability_score = "Likeability",
    competence_score = "Competence",
    content_trust_combined_score = "Content trust",
    author_trust_combined_score = "Author trust",
    fear_of_ai_score = "Fear of AI",
    intention_to_use_score = "Intention to use AI",
    ChangedOpinionOfAI = "Changed opinion of AI",
    CouldWriteContent = "Participant could write content",
    SurveyTopicCheck_coded = "Perceived topic",
    Unrealistic = "Perceived as unrealistic",
    Unrealistic_coded = "Unrealistic code",
    professional_content_expertise = "Participant content expertise",
    Education_regrouped = "Education level",
    WellWritten = "Well-written",
    WellWritten_n = "Well-written content",
    Boring = "Perceived as boring",
    Engaging = "Perceived as engaging",
    Gender = "Gender",
    Sex = "Sex",
    Age_1 = "Age",
    ScienceContent_regrouped = "Science content use"
  )
  
  if (x %in% names(label_map)) {
    return(label_map[[x]])
  } else {
    # Fallback: convert _ to space, sentence-case it, and fix acronyms
    x <- stringr::str_replace_all(x, "_", " ")
    x <- stringr::str_to_sentence(x)
    x <- stringr::str_replace_all(x, "\\bAi\\b", "AI")
    x <- stringr::str_replace_all(x, "\\bGpt\\b", "GPT")
    x <- stringr::str_replace_all(x, "\\bMl\\b", "ML")
    return(x)
  }
})

add_sig_stars <- function(p) {
  p_num <- suppressWarnings(as.numeric(p))
  if (is.na(p_num))
    return("")
  if (p_num < 0.001)
    return("***")
  else if (p_num < 0.01)
    return("**")
  else if (p_num < 0.05)
    return("*")
  else
    return("")
}

build_regression_table_apa <- function(data,
                                       dv,
                                       predictors,
                                       study,
                                       block_name,
                                       share_fraction,
                                       title = NULL,
                                       caption = NULL) {
  if (!requireNamespace("car", quietly = TRUE))
    stop("Install 'car'.")
  if (!requireNamespace("broom", quietly = TRUE))
    stop("Install 'broom'.")
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Install 'flextable'.")
  if (!requireNamespace("officer", quietly = TRUE))
    stop("Install 'officer'.")
  if (!requireNamespace("purrr", quietly = TRUE))
    stop("Install 'purrr'.")
  if (!requireNamespace("stringr", quietly = TRUE))
    stop("Install 'stringr'.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Install 'dplyr'.")
  
  # Step 1: Drop rare levels by share
  cleaned <- drop_rare_levels_by_share(data, predictors, share_fraction)
  data <- cleaned$data
  dropped_levels <- cleaned$dropped_levels
  
  # Step 2: Drop predictors with only one level left
  one_level_predictors <- c()
  for (pred in predictors) {
    if (is.factor(data[[pred]]) || is.character(data[[pred]])) {
      if (length(unique(data[[pred]])) < 2) {
        message("⚠️ Dropping '",
                pred,
                "' because it only has one level remaining.")
        one_level_predictors <- c(one_level_predictors, pred)
      }
    }
  }
  predictors <- setdiff(predictors, one_level_predictors)
  
  
  # Stop early if fewer than 2 predictors remain
  if (length(predictors) < 2) {
    warning("⚠️ Not enough predictors remaining to build a model for: ",
            dv,
            " with block: ",
            study)
    
    # Optionally write a note to Word instead of failing
    empty_doc <- officer::read_docx()
    empty_doc <- officer::body_add_par(empty_doc, paste("Regression Model for", dv), style = "heading 1")
    empty_doc <- officer::body_add_par(
      empty_doc,
      paste(
        "Note: This model could not be estimated because fewer than two valid predictors remained after cleaning."
      ),
      style = "Normal"
    )
    
    file_path <- file.path("outputs",
                           study,
                           paste0("regression_table_", study, "_", dv, ".docx"))
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path))
      dir.create(dir_path, recursive = TRUE)
    
    print(empty_doc, target = file_path)
    message("📄 Empty regression table written to: ", file_path)
    
    return(invisible(NULL))
  }
  
  
  # Step 3: Save reference levels
  ref_levels <- list()
  for (pred in predictors) {
    if (is.factor(data[[pred]]) || is.character(data[[pred]])) {
      if (length(unique(data[[pred]])) < 2) {
        message("⚠️ Dropping '",
                pred,
                "' because it only has one level remaining.")
        one_level_predictors <- c(one_level_predictors, pred)
      }
      
      ref_levels[[pred]] <- levels(as.factor(data[[pred]]))[1]
    }
  }
  
  # Step 4: Fit model
  formula_str <- paste(dv, "~", paste(predictors, collapse = " + "))
  model <- stats::lm(as.formula(formula_str), data = data)
  
  # Step 5: Drop aliased predictors
  aliased <- c()
  alias_obj <- alias(model)$Complete
  if (!is.null(alias_obj)) {
    aliased <- names(which(apply(alias_obj, 1, function(x)
      all(x == 0))))
    if (length(aliased) > 0) {
      message("⚠️ Dropping aliased predictors:\n  - ",
              paste(aliased, collapse = ", "))
      predictors <- setdiff(predictors, aliased)
      formula_str <- paste(dv, "~", paste(predictors, collapse = " + "))
      model <- stats::lm(as.formula(formula_str), data = data)
    }
  }
  
  # Step 6: VIF
  if (length(model$coefficients) <= 2) {
    warning("⚠️ Skipping VIF: model has fewer than 2 predictors.")
    vif_values <- NA
  } else {
    # ---- Step 6: VIF check ----
    vif_sorted <- NULL
    vif_values <- NULL
    tryCatch({
      vif_values <- car::vif(model)
      if (!is.null(vif_values) &&
          is.numeric(vif_values) &&
          length(vif_values) > 1 && !any(is.na(vif_values))) {
        vif_sorted <- sort(vif_values, decreasing = TRUE)
        cat("\n📊 Variance Inflation Factors (VIF):\n")
        print(round(vif_sorted, 2))
        if (any(vif_values >= 5)) {
          warning("⚠️ One or more predictors have VIF >= 5. Consider reducing collinearity.")
        }
      }
    }, error = function(e) {
      warning("⚠️ Unable to calculate VIFs: ", e$message)
    })
    
  }
  
  
  
  
  # Step 7: Tidy + readable labels
  tidy_output <- broom::tidy(model, conf.int = TRUE)
  
  if (any(is.na(tidy_output$term))) {
    warning("⚠️ Some term names in tidy_output are NA — skipping those.")
  }
  
  predictor_names <- names(ref_levels)
  predictor_names <- predictor_names[!is.na(predictor_names) &
                                       predictor_names != ""]
  
  if (length(predictor_names) == 0) {
    warning("No valid predictor names found in ref_levels.")
  }
  
  tidy_output <- tidy_output |>
    dplyr::mutate(
      base_predictor = purrr::map_chr(term, function(t) {
        if (is.na(t) || is.null(t) || length(predictor_names) == 0) {
          return(NA_character_)
        }
        # Make sure pattern is a single string for str_starts()
        matches <- predictor_names[which(stringr::str_starts(t, predictor_names))]
        if (length(matches) > 0)
          matches[which.max(nchar(matches))]
        else
          NA_character_
      }),
      Reference = purrr::map_chr(
        base_predictor,
        ~ if (!is.na(.x) &&
              .x %in% names(ref_levels))
          as.character(ref_levels[[.x]])
        else
          NA_character_
      ),
      stars = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      B = paste0(round(estimate, 2), stars),
      SE = round(std.error, 2),
      t = round(statistic, 2),
      p = formatC(round(p.value, 3), format = "f", digits = 3),
      `95% CI` = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]"),
      Variable = dplyr::case_when(
        term == "(Intercept)" ~ "(Intercept)",!is.na(base_predictor) &
          !is.na(Reference) ~ paste0(
            format_variable_name(base_predictor),
            " (",
            stringr::str_replace(stringr::str_remove(term, base_predictor), "^[:_]*", ""),
            " vs ",
            Reference,
            ")"
          ),
        TRUE ~ format_variable_name(term)
      )
    )
  
  apa_table <- tidy_output |>
    dplyr::select(Variable, B, SE, t, p, `95% CI`) |>
    as.data.frame()
  
  # Expand categorical variables into grouped blocks with indented levels
  apa_table_expanded <- purrr::map_dfr(unique(tidy_output$base_predictor), function(bp) {
    if (is.na(bp)) {
      # Continuous or intercept terms
      tidy_output |>
        dplyr::filter(is.na(base_predictor)) |>
        dplyr::select(Variable, B, SE, t, p, `95% CI`)
    } else {
      block <- tidy_output |> dplyr::filter(base_predictor == bp)
      if (nrow(block) == 0)
        return(NULL)
      
      base_row <- data.frame(
        Variable = format_variable_name(bp),
        B = NA,
        SE = NA,
        t = NA,
        p = NA,
        `95% CI` = NA
      )
      
      contrast_rows <- block |>
        dplyr::mutate(Variable = paste0("  (", stringr::str_replace(Variable, ".*\\((.*)", "\\1"))) |>
        dplyr::select(Variable, B, SE, t, p, `95% CI`)
      
      dplyr::bind_rows(base_row, contrast_rows)
    }
  })
  apa_table_expanded <- apa_table_expanded[, intersect(c("Variable", "B", "SE", "t", "p", "95% CI"),
                                                       colnames(apa_table))]
  
  print(apa_table_expanded, row.names = FALSE)
  
  # Step 8: Word doc export
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, ifelse(
    is.null(title),
    paste("Regression Model for", dv),
    paste0("Table: ", title)
  ), style = "heading 1")
  
  threshold_note <- paste0(
    "Note: Factor levels were dropped if their proportion of the sample was less than one-third ",
    "of the expected share, assuming even distribution across levels."
  )
  doc <- officer::body_add_par(doc, threshold_note, style = "Normal")
  
  if (length(dropped_levels) > 0) {
    doc <- officer::body_add_par(doc, "\nDropped levels due to low sample share:", style = "heading 2")
    for (v in names(dropped_levels)) {
      doc <- officer::body_add_par(doc, paste0("• ", v, ": ", paste(dropped_levels[[v]], collapse = ", ")), style = "Normal")
    }
  }
  
  if (length(one_level_predictors) > 0) {
    doc <- officer::body_add_par(doc,
                                 "\nPredictors dropped due to having only one level after cleaning:",
                                 style = "heading 2")
    doc <- officer::body_add_par(doc, paste0("• ", paste(one_level_predictors, collapse = ", ")), style = "Normal")
  }
  
  ft <- flextable::flextable(apa_table_expanded)
  ft <- flextable::autofit(ft)
  doc <- flextable::body_add_flextable(doc, ft)
  
  if (is.null(caption)) {
    # Significance key
    doc <- officer::body_add_par(doc, "Note. * p < .05, ** p < .01, *** p < .001", style = "Normal")
  } else {
    doc <- officer::body_add_par(doc, caption, style = "Normal")
  }
  
  file_path <- file.path("outputs", study, paste0(paste(
    sep = "_", "regression_table", block_name, dv
  ), ".docx"))
  print(doc, target = file_path)
  message("📄 Regression table saved to: ", file_path)
  
  return(invisible(
    list(
      table = apa_table,
      vif = vif_sorted,
      dropped_levels = dropped_levels,
      dropped_due_to_single_level = one_level_predictors,
      dropped_due_to_aliasing = aliased,
      reference_levels = ref_levels
    )
  ))
}


run_post_hoc_regression_analysis <- function(data, dv, study, share_fraction = 1 /
                                               10) {
  # Friendly label for dependent variable
  dv_label_map <- list(
    author_trust_combined_score = "Author Trust",
    content_trust_combined_score = "Content Trust"
  )
  
  dv_label <- dv_label_map[[dv]]
  if (is.null(dv_label)) {
    warning("No pretty label defined for DV '", dv, "'. Using raw name.")
    dv_label <- dv
  }
  
  predictor_blocks <- NA
  
  # Create dv-specific trust block
  if (dv == "content_trust_combined_score") {
    predictor_blocks <- list(
      study_quality = c(
        "SurveyTopicCheck_coded",
        "Unrealistic",
        "Unrealistic_coded",
        "TechnicalIssues"
      ),
      participant_characteristics = c("Age_1", # "Gender",
                                      "Sex", "Education_regrouped"),
      author_trust = c("author_trust_combined_score"),
      author_perceptions = c("likeability_score", "competence_score"),
      
      ai_attitudes = c(
        "AIChatbotsFrequency_regrouped",
        "intention_to_use_score",
        "ChangedOpinionOfAI",
        "fear_of_ai_score"
      ),
      content_experience = c("science_expertise", "ScienceContent_regrouped"),
      writing_experience = c("writing_expertise"),
      content_quality = c("writing_quality"),
      manipulation = c("Condition")
    )
    
    trust_block_name <- "author_trust"
    # trust_predictors <- c("author_trust_combined_score")
    trust_title <- paste("Author Trust Predicting", dv_label)
  } else if (dv == "author_trust_combined_score") {
    predictor_blocks <- list(
      study_quality = c(
        "SurveyTopicCheck_coded",
        "Unrealistic",
        "Unrealistic_coded",
        "TechnicalIssues"
      ),
      participant_characteristics = c("Age_1", # "Gender",
                                      "Sex", "Education_regrouped"),
      content_experience = c("science_expertise", "ScienceContent_regrouped"),
      writing_experience = c("writing_expertise"),
      content_quality = c("writing_quality"),
      content_trust = c("content_trust_combined_score"),
      ai_attitudes = c(
        "intention_to_use_score",
        "ChangedOpinionOfAI",
        "fear_of_ai_score"
      ),
      author_perceptions = c("likeability_score", "competence_score"),
      manipulation = c("Condition")
    )
    trust_block_name <- "content_trust"
    # trust_predictors <- c("content_trust_combined_score")
    trust_title <- paste("Content Trust Predicting", dv_label)
  } else {
    stop("Unknown DV provided.")
  }
  
  
  # if (dv == "content_trust_combined_score") {
  #
  #   trust_block_name <- "author_trust"
  #   trust_predictors <- c("author_trust_combined_score")
  #   trust_title <- paste("Author Trust Predicting", dv_label)
  # } else if (dv == "author_trust_combined_score") {
  #
  #
  #   trust_block_name <- "content_trust"
  #   trust_predictors <- c("content_trust_combined_score")
  #   trust_title <- paste("Content Trust Predicting", dv_label)
  # } else {
  #   stop("Unknown DV provided.")
  # }
  #
  
  # predictor_blocks <- list(
  #   study_quality = c(
  #     "SurveyTopicCheck_coded",
  #     "Unrealistic",
  #     "Unrealistic_coded",
  #     "TechnicalIssues"
  #   ),
  #   participant_characteristics = c(
  #     "Age_1",
  #     # "Gender",
  #     "Sex",
  #     "Education_regrouped"
  #   ),
  #   participant_experience = c(
  #     "AIChatbotsFrequency_regrouped",
  #     "professional_content_expertise",
  #     "ScienceContent_regrouped",
  #     "CouldWriteContent"
  #   ),
  #   ai_attitudes = c(
  #     "intention_to_use_score",
  #     "ChangedOpinionOfAI",
  #     "fear_of_ai_score"
  #   ),
  #   author_perceptions = c(
  #     "likeability_score",
  #     "competence_score" #,
  #   ),
  #   content_quality = c("WellWritten", "Boring", "Engaging")
  # )
  #
  # # Add the dynamically named trust block
  # predictor_blocks[[trust_block_name]] <- trust_predictors
  #
  # # Add final block
  # predictor_blocks$manipulation <- c("Condition")
  
  
  block_titles <- list(
    # study_quality = paste("Study Related Issues Predicting", dv_label),
    participant_characteristics = paste("Participant Characteristics Predicting", dv_label),
    participant_experience = paste("Participant Experience with AI and content", dv_label),
    ai_attitudes = paste("AI Attitudes Predicting", dv_label),
    author_perceptions = paste("Author Perceptions Predicting", dv_label),
    content_quality = paste("Content Evaluations Predicting", dv_label)
  )
  
  # Add dynamic title
  block_titles[[trust_block_name]] <- trust_title
  
  # Add final title
  block_titles$manipulation <- paste("Effect of Anthropomorphism Condition on", dv_label)
  block_titles$full_model <- paste("Full Model Predicting", dv_label)
  
  for (block_name in names(predictor_blocks)) {
    predictors <- predictor_blocks[[block_name]]
    title <- block_titles[[block_name]]
    
    build_regression_table_apa(
      data = data,
      dv = dv,
      predictors = predictors,
      study = study,
      block_name = block_name,
      title = title,
      caption = paste0("Note. ", title, ". *p* < .05, **p* < .01, ***p* < .001."),
      share_fraction = share_fraction
    )
  }
  
  hierarchical_results <- list()
  all_predictors <- c()
  
  for (block_name in names(predictor_blocks)) {
    all_predictors <- c(all_predictors, predictor_blocks[[block_name]])
    model <- lm(as.formula(paste(
      dv, "~", paste(all_predictors, collapse = " + ")
    )), data = data)
    summary_model <- summary(model)
    r2 <- summary_model$r.squared
    adj_r2 <- summary_model$adj.r.squared
    
    hierarchical_results[[block_name]] <- list(
      block = block_name,
      predictors = all_predictors,
      r2 = r2,
      adj_r2 = adj_r2
    )
  }
  
  for (i in 2:length(hierarchical_results)) {
    current <- hierarchical_results[[i]]
    previous <- hierarchical_results[[i - 1]]
    
    delta_r2 <- current$r2 - previous$r2
    
    cat("\n📊 Step:", i, "-", block_titles[[current$block]], "\n")
    cat(
      "R² =",
      round(current$r2, 3),
      "| ΔR² =",
      round(delta_r2, 3),
      "| Adj. R² =",
      round(current$adj_r2, 3),
      "\n"
    )
  }
  
  
  # Prepare hierarchical R² + ΔR² + F + p summary
  r2_summary <- data.frame(
    Step = character(),
    Block = character(),
    R2 = numeric(),
    Delta_R2 = character(),
    # character to hold empty or numeric
    F = character(),
    df = character(),
    p = character(),
    Adj_R2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  model_list <- list()
  all_predictors <- c()
  
  for (i in seq_along(predictor_blocks)) {
    block_name <- names(predictor_blocks)[i]
    predictors <- predictor_blocks[[block_name]]
    all_predictors <- c(all_predictors, predictors)
    
    model <- lm(as.formula(paste(
      dv, "~", paste(all_predictors, collapse = " + ")
    )), data = data)
    model_list[[i]] <- model
    
    r2 <- round(summary(model)$r.squared, 3)
    adj_r2 <- round(summary(model)$adj.r.squared, 3)
    
    
    if (i == 1) {
      r2_summary[i, ] <- list(
        Step = paste0("Step ", i),
        Block = format_variable_name(block_name),
        R2 = r2,
        Delta_R2 = NA,
        F = NA,
        df = NA,
        p = NA,
        Adj_R2 = adj_r2
      )
    } else {
      test <- anova(model_list[[i - 1]], model)
      
      if (!is.null(test) && !is.na(test[["Pr(>F)"]][2])) {
        delta_r2 <- round(r2 - summary(model_list[[i - 1]])$r.squared, 3)
        f_val <- round(test$F[2], 2)
        df_val <- paste0(test$Df[2], ", ", test$Res.Df[2])
        p_val <- format.pval(as.numeric(test$`Pr(>F)`[2]),
                             digits = 3,
                             eps = 0.001)
        sig <- add_sig_stars(p_val)
      } else {
        # Leave ΔR² and p blank or set to NA
        delta_r2 <- NA
        f_val <- NA
        df_val <- NA
        p_val <- NA
        sig <- ""
      }
      
      r2_summary[i, ] <- list(
        Step = paste0("Step ", i),
        Block = format_variable_name(block_name),
        R2 = r2,
        Delta_R2 = paste0(delta_r2, sig),
        F = f_val,
        df = df_val,
        p = p_val,
        Adj_R2 = adj_r2
      )
    }
  }
  
  # 📈 Save a line plot of R² progression across blocks
  plot_path <- file.path("plots", study, paste0("hierarchical_r2_plot_", dv, ".png"))
  dir.create(dirname(plot_path),
             showWarnings = FALSE,
             recursive = TRUE)
  
  
  
  # Step 1: Significance stars
  r2_summary$sig <- ifelse(
    is.na(r2_summary$p),
    "",
    dplyr::case_when(
      r2_summary$p < 0.001 ~ "***",
      r2_summary$p < 0.01  ~ "**",
      r2_summary$p < 0.05  ~ "*",
      TRUE ~ ""
    )
  )
  
  # Step 2: p-label (for display)
  r2_summary$p_label <- ifelse(
    is.na(r2_summary$p),
    "",
    dplyr::case_when(r2_summary$p < 0.001 ~ "p < .001", TRUE ~ paste0(
      "p = ", formatC(
        as.numeric(r2_summary$p),
        digits = 3,
        format = "f"
      )
    ))
  )
  
  # Step 3: Plot/legend grouping
  r2_summary$significance_group <- factor(
    dplyr::case_when(
      is.na(r2_summary$p) ~ "ns",
      r2_summary$p < 0.001 ~ "p < .001",
      r2_summary$p < 0.01  ~ "p < .01",
      r2_summary$p < 0.05  ~ "p < .05",
      TRUE ~ "ns"
    ),
    levels = c("p < .001", "p < .01", "p < .05", "p < .10", "ns")
  )
  
  # r2_summary$label <- ifelse(r2_summary$Delta_R2 != "",
  #                            paste0("ΔR² = ", r2_summary$Delta_R2, r2_summary$sig),
  #                            "")
  # r2_summary$label[nrow(r2_summary)] <- paste0("Total R² = ", round(r2_summary$R2[nrow(r2_summary)], 3))
  #
  r2_summary$label <- ifelse(
    r2_summary$Delta_R2 != "",
    paste0("ΔR² = ", r2_summary$Delta_R2, "\n", r2_summary$p_label),
    ""
  )
  
  # Build the plot
  plot <- ggplot2::ggplot(r2_summary, ggplot2::aes(x = seq_along(R2), y = R2)) +
    ggplot2::geom_line(color = "grey", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(color = significance_group), size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      nudge_y = 0.03,
      size = 3,
      check_overlap = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = setNames(fill_pal, levels(r2_summary$significance_group)),
      name = expression(Delta * R^2 * " Significance")
    ) +
    ggplot2::scale_x_continuous(breaks = seq_along(r2_summary$Step),
                                labels = r2_summary$Block) +
    ggplot2::labs(
      title = paste(
        "Cumulative R² by Hierarchical Block for",
        format_variable_name(dv)
      ),
      x = "Predictor Block",
      y = expression(R^2)
    ) +
    my_theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      plot.margin = ggplot2::margin(
        t = 20,
        r = 20,
        b = 40,
        l = 20
      ),
      legend.position = "bottom"
    ) +
    ggplot2::expand_limits(y = max(r2_summary$R2) + 0.05) #+
  #   ggplot2::aes(color = significance_group) +
  #   ggplot2::scale_color_manual(values = c(
  #   "Significant (p < .05)" = "#196389",
  #   "Not Significant" = "gray60"
  # ))
  
  print(plot)
  
  ggplot2::ggsave(
    plot_path,
    plot = plot,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )
  message("📊 R² line plot saved to: ", plot_path)
  
  # Save Word summary
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc,
                               paste(
                                 "Hierarchical Regression Summary for",
                                 format_variable_name(dv)
                               ),
                               style = "heading 1")
  doc <- officer::body_add_par(
    doc,
    "Each step adds predictors from a conceptual block. ΔR² reflects added explanatory power.",
    style = "Normal"
  )
  
  ft <- flextable::flextable(r2_summary)
  ft <- flextable::autofit(ft)
  ft <- flextable::add_footer_lines(ft, values = "Note. ΔR² significance: *p* < .05, **p* < .01, ***p* < .001.")
  
  doc <- flextable::body_add_flextable(doc, ft)
  
  
  # Add line plot to the Word doc
  doc <- officer::body_add_par(doc, "Visual Summary of R² Growth", style = "heading 2")
  dof <- officer::body_add_par(doc,
                               paste(
                                 "Cumulative R² by Hierarchical Block for",
                                 format_variable_name(dv)
                               ),
                               style = "Normal")
  doc <- officer::body_add_img(
    doc,
    src = plot_path,
    width = 6,
    height = 4,
    style = "centered"
  )
  
  file_path <- file.path("outputs",
                         study,
                         paste0("hierarchical_r2_summary_", dv, ".docx"))
  dir.create(dirname(file_path),
             showWarnings = FALSE,
             recursive = TRUE)
  print(doc, target = file_path)
  
  message("📄 Hierarchical R² summary saved to: ", file_path)
  
  
  
  
  
  
}