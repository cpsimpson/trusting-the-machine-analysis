# library(mediation)
# library(lavaan)


test_med_mod_model <- function(data, model){
  
  fit <- lavaan::sem(model, data = data, se = "bootstrap", bootstrap = 5000)
  summary(fit, standardized = TRUE, fit.measures = TRUE)
  
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