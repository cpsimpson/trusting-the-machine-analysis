
library(tidyverse)
library(haven)

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


violin_plot <- function(data, x, y, group, title) {
  data |>
    ggplot(aes(x = x, y = y)) +
    geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
    ggtitle(title) 
}

# This was used to output the column information.
export_variables_as_markdown <- function(raw_data){
  dictionary <- labelled::generate_dictionary(raw_data)
  variables_md <- kable(dictionary, align = "c")
  save_kable(variables_md, "variables.md")
}




