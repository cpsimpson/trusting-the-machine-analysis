
library(tidyverse)
library(haven)
library(rcartocolor)




draw_interaction_plot <- function(x, trace, y, x_label, trace_label, y_label) {
  
  
  df<- na.omit(data.frame(x, trace, y))
  
  interaction.plot(x.factor = df$x, 
                   trace.factor = df$trace, 
                   response = df$y, 
                   fun = mean, type = "b", 
                   xlab = x_label, 
                   ylab=y_label, 
                   trace.label = trace_label,
                   col="blue", pch=1:5)
}

simple_effects_between_anova <- function(data, tom_value, dv, between) {
  subset_data <- data |> filter(tom == tom_value)
  # model <- lm(as.formula(formula), data = subset_data)
  model <- anova_test(data = subset_data, dv = dv, between = between, detailed=TRUE)
  get_anova_table(model)
}

simple_effects_within_anova <- function(data, condition, dv, within) {
  subset_data <- subset(data, Condition == condition)
  model <- anova_test(data = subset_data, dv = dv, wid = ResponseId, within = within, detailed=TRUE)
  get_anova_table(model)
}




bar_plot <- function(data, y, group, title, 
                        legend_label = "Anthropomorphism \n Condition", 
                        x_label = "Condition", y_label = "Score") {
  data |>
    ggplot(aes( y = y, fill = group)) +
    geom_bar(alpha = 0.5) +
    ggtitle(title) + 
    labs(y = y_label, fill = legend_label)
}



linear_regression <- function(formula, data) {
  result <- tryCatch({
    # Code produces error
    fit_i <- lm(formula, 
                data = data)
    
    summ(fit_i) |>
      print()
    
    return(fit_i)
    
  }, error = function(e) {
    # Handle the error
    cat("An error occurred:", e$message, "\n")
    return(NA)
  })
  
}
