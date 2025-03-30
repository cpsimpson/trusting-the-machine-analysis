library(mediation)
library(lavaan)


test_med_mod_model <- function(data, model){
  
  fit <- sem(model, data = data, se = "bootstrap", bootstrap = 5000)
  summary(fit, standardized = TRUE, fit.measures = TRUE)
  
  return(fit)
  
}

