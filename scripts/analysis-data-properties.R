library(tidyverse)
library(moments)  # For skewness calculation

check_skewness <- function(data, study){
  # Compute skewness for each numeric variable
  skewness_results <- data %>%
    summarise(across(where(is.numeric), ~ skewness(., na.rm = TRUE)))
  
  write_csv(skewness_results, 
            paste(sep = "/", "outputs", study, "skewness_results.csv"),
            
            )
            
  return(skewness_results)
}

check_normality <- function(data, study){
  #Shapiro-Wilk normality test (only works for n <= 5000)
  shapiro_results <- data %>%
    summarise(across(where(is.numeric), ~ shapiro.test(.[!is.na(.)])$p.value))
  
  print(shapiro_results)
  write_csv(shapiro_results, paste(sep = "/", 
                                   "outputs", study,"shapiro_results.csv"))
}


