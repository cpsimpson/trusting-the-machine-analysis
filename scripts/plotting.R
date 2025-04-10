
safe_pal <- rcartocolor::carto_pal(12, "Safe")


pal = c("#7F2543", "#196389", "#2f2589", "#267843", "grey40", "#f4849f")
fill_pal = c("#7F254350", "#19638950", "#2f258950", "#26784350", "grey80", "#f4849f50")


# Create a customized ggplot2::theme and apply it as the default in this rmarkdown file.
my_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    # panel.grid.minor = ggplot2::element_blank(),  # hide the minor grid lines
    axis.line = ggplot2::element_line(colour = "grey70"),
    axis.ticks = ggplot2::element_line(colour = "grey70"),
    legend.position = "top",
    plot.title = ggplot2::element_blank() #ggtext::element_markdown(hjust=.5, vjust=.5),  # center the title
  )

get_anthropomorphism_comparisons <- function(study) {
  if (study == "s1") {
    anthropomorphism_comparisons <- list(c("Low", "Medium"), c("Medium", "High"), c("Low", "High"))
  } else if (study == "s2") {
    anthropomorphism_comparisons <- list(c("Low", "High"))
  }
  
  return(anthropomorphism_comparisons)
}

get_label <- function(column_name) {
  switch(
    column_name,
    "Condition" = "Anthropomorphism Condition",
    "anthropomorphism_score" = "Anthropomorphism Score",
    "likeability_score" = "Likeability Score",
    "competence_score" = "Competence Score",
    "content_trust_combined_score" = "Content Trust Score",
    "author_trust_combined_score" = "Author Trust Score",
    "Age_1" = "Participant Age",
    "age_range" = "Participant Age Range",
    "AIChatbotsFrequency_regrouped" = "AI Usage Frequency", 
    "Sex" = "Sex",
    "Gender" = "Gender",
    "Education_regrouped" = "Education",
    "ScienceContent_regrouped" = "Science Content Consumption Frequency",
    "intention_to_use_score" = "Intention to Use AI",
    "ChangedOpinionOfAI" = "Changed Opinion of AI",
    "CouldWriteContent" = "Could Write Blog Post",
    "fear_of_ai_score" = "Fear of AI",
    "professional_content_expertise" = "Professional Experience with Content or Writing",
    "WellWritten" = "Writing Quality",
    "SurveyTopicCheck_coded" = "Reported Survey Purpose",
    "unknown"
  )
}


violin_plot <- function(data,
                        study,
                        x_col_name,
                        y_col_name,
                        include_legend = TRUE,
                        save = TRUE, 
                        comparisons = NULL) {
  if (include_legend == TRUE) {
    legend_position = "top"
  } else {
    legend_position = "none"
  }
  
  if (is.null(comparisons)){
    comparisons = get_anthropomorphism_comparisons(study)
  }
  x = data |> pull({{ x_col_name }})
  y = data |> pull({{ y_col_name }})
  x_label = get_label(x_col_name)
  y_label = get_label(y_col_name)
  title = paste(y_label, "by", x_label)
  legend_label = x_label
  
  plot <- data |>
    ggplot(aes(x = x, y = y, fill = x)) +
    geom_violin(alpha = 0.5) +
    ggtitle(title) +
    labs(x = x_label, y = y_label, fill = legend_label)  +
    scale_fill_manual(values = fill_pal) +
    scale_color_manual(values = pal) +
    geom_boxplot(width = 0.1) +
    ggplot2::theme(legend.position = legend_position) +
    ggpubr::stat_compare_means(
      comparisons = comparisons,
      method = "t.test",
      label = "p.signif",
      paired = FALSE
    )
  
  filename <- paste0("violin_", y_col_name,"_", x_col_name, ".png")
  
  ggsave(
    paste(sep= "/", "plots", study, filename),
    plot = plot,
    create.dir = TRUE
  )
  
  return(plot)
}


# prediction_plot <- function(data,
#                             study,
#                             x_col_name,
#                             y_col_name,
#                             save = TRUE)
# {
#   # Fit linear model
#   model <- lm({{y_col_name}} ~ {{x_col_name}}, data = data)
#   
#   # Create new data for predictions (optional: use df directly)
#   new_data <- data.frame(y_col_name = data |> pull({{y_col_name}}), 
#                          x_col_name = data |> pull({{x_col_name}}))
#   
#   # Get predictions with prediction intervals
#   preds <- predict(model, data = new_data, interval = "prediction", level = 0.95)
#   
#   # Combine predictions with original data
#   new_data$fit <- preds[, "fit"]
#   new_data$lwr <- preds[, "lwr"]
#   new_data$upr <- preds[, "upr"]
#   
#   # Plot
#   ggplot(new_data, aes(x = Fear_of_AI, y = Content_Trust_Score)) +
#     geom_point(alpha = 0.6, color = "#196389") +
#     geom_line(aes(y = fit), color = "#7F2543", size = 1) +
#     geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#7F254350", alpha = 0.5) +
#     labs(
#       x = get_label(x_col_name),
#       y = get_label(y_col_name),
#       title = paste("Prediction Interval for", get_label(y_col_name), "vs.",  get_label(x_col_name))
#     ) +
#     ggplot2::theme_minimal()
# }


correlation_plot <- function(data,
                             study,
                             x_col_name,
                             y_col_name,
                             save = TRUE){
  
  legend_position = "none"
  x = data |> pull({{ x_col_name }})
  y = data |> pull({{ y_col_name }})
  x_label = get_label(x_col_name)
  y_label = get_label(y_col_name)
  title = paste("Correlation Between", y_label, "and", x_label)
  
  plot_lm <- data |>
    ggplot(aes(x = x, y = y)) +
    labs(x = x_label, y = y_label)  +
    geom_point(col = "#196389", size = 0.5) +
    stat_smooth(method = "lm", formula = "y ~ x",
                col = "#7F2543", se = TRUE, size = 1, 
                fill = "#7F254350") 
  
  filename <- paste0("correlation_", "lm_", y_col_name,"_", x_col_name, ".png")
  
  ggsave(
    paste(sep= "/", "plots", study, filename),
    plot = plot_lm,
    create.dir = TRUE
  )
  
  plot_loess <- data |>
    ggplot(aes(x = x, y = y)) +
    labs(x = x_label, y = y_label)  +
    geom_point(col = "#196389", size = 0.5) +
    stat_smooth(method = "loess", formula = "y ~ x",
                col = "#267843", se = TRUE, size = 1, 
                fill = "#26784350")
  
  filename <- paste0("correlation_", "loess_", y_col_name,"_", x_col_name, ".png")
  
  ggsave(
    paste(sep= "/", "plots", study, filename),
    plot = plot_loess,
    create.dir = TRUE
  )
  
  
  return(list(lm = plot_lm, loess = plot_loess))
  
}

categorical_interaction_plot_3 <- function(data, lm_model, pred_name, mod_name, target_name, study){
  plot <- cat_plot(lm_model, 
           pred = {{ pred_name }},
           modx = {{ mod_name }}, 
           x.label = get_label(pred_name),
           y.label = get_label(target_name),
           legend.main = get_label(mod_name),
           plot.points = TRUE, 
           jitter = 0.5, 
           point.alpha = 0.25, 
           geom="line", 
           colors = safe_pal) +
    ggplot2::theme(legend.position = "top")
  
  filename <- paste0("interaction_", target_name, "_", pred_name, "_", mod_name, ".png")
  
  ggsave(
    paste(sep = "/", "plots", study, filename),
    plot = plot,
    create.dir = TRUE
  )
  
  return(plot)
}

interaction_plot_3 <- function(data, lm_model, pred_name, mod_name, target_name, study){
  
  plot <- interact_plot(lm_model,
                        data = data,
                        pred = {{ pred_name }},
                        modx = {{ mod_name }},
                        x.label = get_label(pred_name),
                        y.label = get_label(target_name),
                        legend.main = get_label(mod_name),
                        plot.points = TRUE, 
                        jitter = 0.5, 
                        point.alpha = 0.25, 
                        geom = "bar",
                        rug = TRUE,
                        rug.sides = "bl",
                        colors = safe_pal) +
    ggplot2::theme(legend.position = "top")
  
  filename <- paste0("interaction_", target_name, "_", pred_name, "_", mod_name, ".png")
  
  ggsave(
    paste(sep = "/", "plots", study, filename),
    plot = plot,
    create.dir = TRUE
  )
  
  return(plot)
}

interaction_plot_4 <- function(data, lm_model, pred_name, mod_name, mod2_name, mod2_v, target_name, study){
  
  plot <- interact_plot(lm_model,
                        data = data,
                        pred = {{ pred_name }},
                        modx = {{ mod_name }},
                        mod2 = {{ mod2_name }},
                        mod2.values = mod2_v,
                        mod2.labels = mod2_v,
                        x.label = get_label(pred_name),
                        y.label = get_label(target_name),
                        legend.main = get_label(mod_name),
                        plot.points = TRUE, 
                        jitter = 0.5, 
                        point.alpha = 0.25, 
                        geom = "bar",
                        rug = TRUE,
                        rug.sides = "bl",
                        colors = safe_pal) +
    ggplot2::theme(legend.position = "top")
  
  filename <- paste0("interaction_", target_name, "_", pred_name, "_", mod_name, "_", mod2_name, ".png")
  
  ggsave(
    paste(sep = "/", "plots", study, filename),
    plot = plot,
    create.dir = TRUE
  )
  
  return(plot)
}


get_sem_label <- function(column_name) {
  switch(
    column_name,
    "Condition" = "Anthropomorphism\nManipulation",
    "Condition_Compressed" = "Anthropomorphism\nManipulation",
    "Condition_Medium" = "Anthropomorphism\nManipulation\nMedium",
    "Condition_High" = "Anthropomorphism\nManipulation\nHigh",
    "anthropomorphism_score" = "Perceived\nAnthropomorphism",
    "likeability_score" = "Perceived\nLikeability",
    "competence_score" = "Perceived\nCompetence",
    "content_trust_combined_score" = "Content Trust",
    "author_trust_combined_score" = "Author Trust",
    "Age_1" = "Participant Age",
    "age_range" = "Participant Age Range",
    "AIChatbotsFrequency_regrouped" = "AI Usage Frequency", 
    "Sex" = "Sex",
    "Gender" = "Gender",
    "Education_regrouped" = "Education",
    "ScienceContent_regrouped" = "Science Content Consumption Frequency",
    "intention_to_use_score" = "Intention to Use AI",
    "ChangedOpinionOfAI" = "Changed Opinion of AI",
    "CouldWriteContent" = "Could Write Blog Post",
    "fear_of_ai_score" = "Fear of AI",
    "professional_content_expertise" = "Professional Experience with Content or Writing",
    "AIChatbotsFrequency_ordinal" = "AI Usage Frequency", 
    "ScienceContent_ordinal" = "Science Content Frequency", 
    "WellWritten_n" = "Writing Quality",
    "Engaging_n" = "Writing Engagement",
    "SurveyTopicCheck_coded" = "Reported Survey Purpose",
    "anthro_x_usage" = "AI Usage Frequency",
    column_name
  )
}

# Function to add asterisks
add_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("")
}

# plot_model <- function(fit, study, subset_nodes=NULL){
#   # plot the variables with edges represented by standardized effect size (Beta) and significance stars
#   
#   # Create semPlotModel object
#   sem_model <- semPlot::semPlotModel(fit)
# 
#   
#   # 3. Keep only desired Vars
#   keep_vars <- which(sem_model@Vars$name %in% subset_nodes)
#   sem_model@Vars <- sem_model@Vars[keep_vars, ]
#   
#   # 4. Remove edges (Pars) connected to dropped variables
#   remaining_names <- sem_model@Vars$name
#   keep_edges <- sem_model@Pars$lhs %in% remaining_names & sem_model@Pars$rhs %in% remaining_names
#   sem_model@Pars <- sem_model@Pars[keep_edges, ]
#   
#   node_names <- sem_model@Vars$name
#   node_labels <- map_chr(node_names, get_sem_label)
#   
#   
#   std_estimates <- lavaan::parameterEstimates(fit, standardized = TRUE)
#   
#   
#   std_estimates$starred_label <- ifelse(
#     is.na(std_estimates$std.all),
#     "",  # If there's no standardized estimate, leave blank
#     paste0(
#       format(round(std_estimates$std.all, 2), nsmall = 2),
#       sapply(std_estimates$pvalue, add_stars)
#     )
#   )
# 
# 
#   
#   regression_labels <- std_estimates$starred_label[std_estimates$op == "~"]
#   # loading_labels <- std_estimates$starred_label[std_estimates$op == "=~"]
#   # correlation_labels <- std_estimates$starred_label[std_estimates$op == "~~"]
#   # indirect_labels <- std_estimates$starred_label[std_estimates$op == ":="]
#   # 
#   all_labels <- std_estimates$starred_label
#   
#   filename <- paste0("sem_", paste(node_names[1:5], collapse="_"))
#   path <- paste(sep = "/", "plots", study, filename)
#   
#   sem_obj <- semPlot::semPaths(
#     object = fit,              # your lavaan model object
#     what = "std",
#     whatLabels = "est",
#     edgeLabels = regression_labels,
#     # edgeLabels = all_labels,
#     layout = "spring",           # "tree", "circle", "spring", etc.
#     # edge.label.cex = 1.2,      # size of edge (path) labels
#     # sizeMan = 10,               # size of variable boxes
#     # sizeLat = 0,               
#     residuals = FALSE,         # don't show residual variances
#     nCharNodes = 0,            # show full variable names
#     title = FALSE,
#     nodeLabels = node_labels,
#     label.scale = FALSE, 
#     label.cex = 0.5, 
#     # filetype = "png",
#     # filename = path,
#     repulsion = 0.9,
#     # curveAll = TRUE,
#     bg = "transparent",
#     curvePivot = TRUE     # improves curve spacing
#     
#   )
#   
#   ragg::agg_png(paste0(path, "_transparent.png"), width = 2400, height = 1800, res = 300, background = "transparent")
#   plot(sem_obj)
#   dev.off()
#   
# 
#   return(sem_obj)
# }

node_colors <- c(
  "#196389",  # an_ (Anthropomorphism)
  "#2f2589",  # lk_ (Likeability)
  "#267843",  # a__ (Author Trust)
  "#f4849f",  # c__ (Content Trust)
  "#2f2589",  # cm_ (Competence)
  "#7F2543",  # Cnd (Condition)
  "#f4849f",  # AIC (Chatbot Frequency)
  "#f4849f",  # A_6 (Appelman 6)
  "#f4849f",  # SC_ (Science Content Frequency)
  "#f4849f"   # A_4 (Appelman 4)
)


plot_model <- function(fit, study, subset=NULL){
  # plot the variables with edges represented by standardized effect size (Beta) and significance stars

  sem_model <- semPlot::semPlotModel(fit)
  
  node_names <- sem_model@Vars$name
  node_labels <- map_chr(node_names, get_sem_label)
  # 
  # std_estimates <- lavaan::parameterEstimates(fit, standardized = TRUE)
  # 
  # # Remove non-significant regressions (p >= .05)
  # sig_regressions <- std_estimates$op == "~" & std_estimates$pvalue < 0.05
  # 
  # # Get all parameters (paths) in the SEM diagram
  # sem_pars <- sem_model@Pars
  # 
  # # Mark non-significant regression paths as invisible
  # is_regression <- sem_model@Pars$edge == "regression"
  # is_significant <- sem_model@Pars$p < 0.05
  # sem_model@Pars$t <- !is_regression | (is_regression & is_significant)
  # 
  # format_estimate <- function(est, pval) {
  #   if (is.na(est)) return("")
  #   stars <- if (is.na(pval)) "" else if (pval < .001) "***" else if (pval < .01) "**" else if (pval < .05) "*" else ""
  #   paste0(formatC(est, format = "f", digits = 2), stars)
  # }
  # 
  # # Apply across all visible paths
  # all_labels <- mapply(
  #   format_estimate,
  #   sem_model@Pars$std,
  #   sem_model@Pars$p
  # )
  # 
  # edgeLabels_filtered <- all_labels[sem_model@Pars$t]
  #   
  # is_displayed <- sem_model@Pars$t
  # is_sig <- sem_model@Pars$p < 0.05
  # edgeLabels_filtered <- ifelse(is_displayed & is_sig, all_labels[is_displayed], "")

  
  filename <- paste0("sem_", paste(node_names[1:5], collapse="_"))
  path <- paste(sep = "/", "plots", study, filename)

  sem_obj <- semPlot::semPaths(
    object = sem_model,          # <- use sem_model, not fit
    what = "std",
    whatLabels = "est",
    # edgeLabels = edgeLabels_filtered,
    layout = "spring",
    residuals = FALSE,
    nCharNodes = 0,
    title = FALSE,
    nodeLabels = node_labels,
    label.scale = FALSE,
    label.cex = 0.5,
    repulsion = 1.0,
    bg = "transparent",
    edge.label.cex = 0.9,
    sizeMan = 7,
    sizeLat = 8,
    fade = FALSE,
    mar = c(3, 3, 3, 3),
    style = "lisrel",
    curvePivot = TRUE,
    intercepts = FALSE
    # color = node_colors  # You can keep this as long as node_colors is correctly ordered
  )

  # Save with transparency
  # svglite::svglite(paste0(path, "_transparent.svg"), width = 10, height = 8, bg = "transparent")
  ragg::agg_png(paste0(path, "_transparent.png"), width = 2400, height = 1800, res = 300, background = "transparent")
  plot(sem_obj)
  dev.off()


  # indirects <- std_estimates[std_estimates$op == ":=", c("lhs", "rhs", "label", "std.all", "pvalue")]
  # indirects$starred <- paste0(round(indirects$std.all, 2), sapply(indirects$pvalue, add_stars))
  # print(indirects)
  return(sem_obj)
}

