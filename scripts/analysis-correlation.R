library(tidyverse)
library(apa)
library(jtools)

test_correlation <- function(data, x_name, y_name){
  x <- data |> pull({{x_name}})
  y <- data |> pull({{y_name}})
  
  cor_result <- cor.test(
    x, y, method = "pearson")
  
  apa::cor_apa(cor_result, r_ci = TRUE)
  
}

linear_model <- function(data, formula){
  
  fit <- lm(formula, data = data)
  print(summ(fit, confint = TRUE, digits = 3, ci.width = .95))
  
  return(fit)
}