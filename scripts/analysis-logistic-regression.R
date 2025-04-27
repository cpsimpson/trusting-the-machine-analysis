require(MASS)


run_sample_characteristic_logistic_regression(data, formula){
  #TODO: make this generic
  data$Condition <- factor(data$Condition, 
                                  levels = c("Low", "Medium", "High"), 
                                  ordered = TRUE)
  
  model <- MASS::polr(formula, data = data, Hess = TRUE)
  summary(model)
  
  ctable <- coef(summary(model))
  p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p_values)
  ctable
  
  exp(coef(model))  # Odds ratios for predictors
}