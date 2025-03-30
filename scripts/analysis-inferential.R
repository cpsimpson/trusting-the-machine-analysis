library(apa)
library(effectsize)
library(rstatix)
library(performance)

run_between_subjects_anova <- function(data, formula, dv_name){
  aov_model <- aov(data = data, formula = formula)
  
  test_between_subjects_anova_assumptions(data, formula, aov_model, dv_name)
  
  model_summ <- summary(aov_model)
  print(model_summ)
  
  print(anova_apa(aov_model))
  
  print(paste0("MSE = ", model_summ[[1]]["Mean Sq"][[1]][1]))
  
  print(effectsize::omega_squared(aov_model))
  
  
  return(aov_model)
}


test_between_subjects_anova_assumptions <- function(data, formula, aov_model, dv_name){
  
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
  
}

test_within_subjects_anova_assumptions <- function(aov_model){
  check_homogeneity(aov_model)
  check_sphericity(aov_model)
}

run_simple_effects_t_tests <- function(data, formula){
  result <- rstatix::pairwise_t_test(data = data, 
                           formula = formula,  
                           p.adjust.method = "bonferroni", 
                           detailed = TRUE, 
                           paired = FALSE)
  
  print(rstatix::cohens_d(data = data, formula = formula))
  
  return(result)
}