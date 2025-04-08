# library(mediation)
# library(lavaan)


test_med_mod_model <- function(data, model){
  
  fit <- lavaan::sem(model, data = data, se = "bootstrap", bootstrap = 5000)
  lavaan::summary(fit, standardized = TRUE, fit.measures = TRUE, ci = TRUE)
  
  return(fit)
  
}

# Main function to generate tidy SEM summary table
get_sem_summary_table <- function(fit, save_doc = FALSE, outfile = "") {
  summary_table <- lavaan::parameterEstimates(fit, standardized = TRUE) %>%
    mutate(
      `Parameter Type` = case_when(
        op == "~" ~ "Regression Path",
        op == "=~" ~ "Latent Loading",
        op == "~~" ~ "Covariance",
        op == ":=" ~ "Indirect Effect",
        TRUE ~ "Other"
      ),
      `Estimate (Standardized)` = ifelse(
        is.na(std.all),
        "",
        paste0(format(round(std.all, 2), nsmall = 2), sapply(pvalue, add_stars))
      ),
      p_value = round(pvalue, 3)
    ) %>%
    select(
      From = lhs,
      Operator = op,
      To = rhs,
      `Parameter Type`,
      `Estimate (Standardized)`, 
      P = p_value
    )
  
  if(isTRUE(save_doc)){
    # Create a flextable from your summary
    ft <- flextable::flextable(summary_table)
    ft <- flextable::autofit(ft)  # makes column widths tidy
    
    # Create a new Word document and add the table
    template <- system.file(package = "officer",
                            "doc_examples", "landscape.docx")
    doc <- officer::read_docx(path = template)
    doc <- officer::body_add_par(doc, "SEM Summary Table", style = "heading 2")
    doc <- flextable::body_add_flextable(doc, ft)
    
    # Save it
    print(doc, target = outfile)
  }
  
  return(summary_table)
  
}


export_sem_results_to_word <- function(fit, study, model_name) {
  if (!requireNamespace("broom", quietly = TRUE)) stop("Install 'broom'.")
  if (!requireNamespace("flextable", quietly = TRUE)) stop("Install 'flextable'.")
  if (!requireNamespace("officer", quietly = TRUE)) stop("Install 'officer'.")
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Install 'lavaan'.")
  
  # Extract standardized estimates
  std_est <- lavaan::parameterEstimates(fit, standardized = TRUE, boot.ci.type = "perc", ci = TRUE)
  std_est <- std_est[std_est$op == "~", ]
  
  # Clean for APA table
  paths_table <- std_est |>
    dplyr::mutate(
      Path = paste0(
        format_variable_name(lhs),
        " \u2190 ",
        format_variable_name(rhs)
      ),
      `β` = round(std.all, 2),
      SE = round(se, 2),
      z = round(z, 2),
      p = dplyr::case_when(
        pvalue < .001 ~ "<.001",
        TRUE ~ formatC(round(pvalue, 3), format = "f", digits = 3)
      ),
      `95% CI` = paste0("[", round(ci.lower, 2), ", ", round(ci.upper, 2), "]")
    ) |>
    dplyr::select(Path, `β`, SE, z, p, `95% CI`)
  
  # Indirect effects
  # Get all estimates
  all_params <- lavaan::parameterEstimates(fit, standardized = TRUE, boot.ci.type = "perc", ci = TRUE)
  
  # Filter for indirect effects
  indirect <- all_params[all_params$op == ":=", ]
  
  # Get all paths (needed to match labels like b1, e1 to actual variable paths)
  path_lookup <- all_params[all_params$op == "~" & all_params$label != "", c("lhs", "rhs", "label")]
  
  # Build readable indirect path descriptions
  indirect$Path <- purrr::map_chr(indirect$rhs, function(path_expr) {
    # e.g., "b1 * e1" → ["b1", "e1"]
    terms <- unlist(strsplit(path_expr, "\\s*\\*\\s*"))
    labels <- purrr::map_chr(terms, function(term) {
      row <- dplyr::filter(path_lookup, label == term)
      if (nrow(row) == 1) {
        paste0(format_variable_name(row$lhs), " \u2190 ", format_variable_name(row$rhs))
      } else {
        term  # fallback
      }
    })
    paste(labels, collapse = " → ")
  })
  
  indirect_table <- indirect |>
    dplyr::mutate(
      `β` = round(std.all, 2),
      SE = round(se, 2),
      z = round(z, 2),
      p = dplyr::case_when(
        pvalue < .001 ~ "<.001",
        TRUE ~ formatC(round(pvalue, 3), format = "f", digits = 3)
      ),
      `95% CI` = paste0("[", round(ci.lower, 2), ", ", round(ci.upper, 2), "]")
    ) |>
    dplyr::select(Path, `β`, SE, z, p, `95% CI`)

  
  # R² values
  r2_table <- data.frame(
    `Outcome Variable` = format_variable_name(names(lavaan::inspect(fit, "r2"))),
    `R²` = round(unlist(lavaan::inspect(fit, "r2")), 2),
    row.names = NULL
  )
  
  # Create Word doc
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "Standardized Path Estimates", style = "heading 1")
  doc <- flextable::body_add_flextable(doc, flextable::autofit(flextable::flextable(paths_table)))
  
  if (exists("indirect_table")) {
    doc <- officer::body_add_par(doc, "Indirect Effects", style = "heading 1")
    doc <- flextable::body_add_flextable(doc, flextable::autofit(flextable::flextable(indirect_table)))
  }
  
  doc <- officer::body_add_par(doc, "Explained Variance (R²)", style = "heading 1")
  doc <- flextable::body_add_flextable(doc, flextable::autofit(flextable::flextable(r2_table)))
  
  # Save file
  
  file_path <- file.path("outputs",
                         study,
                         paste0("sem_summary_", model_name, ".docx"))
  
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = file_path)
  
  message("✅ SEM results exported to Word: ", file_path)
}