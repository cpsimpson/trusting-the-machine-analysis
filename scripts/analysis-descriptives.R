library(tidyverse)

summarize_columns <- function(data, ...){
  result <- data |> 
    group_by(...) |> 
    summarise(n=n(), .groups = "keep")
  
  return(result)
}


summarize_exploded_columns <- function(data, col_prefix, ...){
  extended_prefix = paste0(col_prefix, "_")
  result <- data |> 
    select(starts_with(extended_prefix), Condition) |>
    pivot_longer(names_to = col_prefix, cols = starts_with(extended_prefix), 
                 names_prefix = extended_prefix) |>
    group_by(...) |>
    summarise(n = sum(value), .groups = "keep")
  
  return(result)
}

count_recruited_participants <- function(data, study){
  if(study == "s1"){
     return(data |>
      filter(EndDate >= "2024-12-04") |>  # Started on or after "2024-12-04" to remove test runs
      filter(Status == 0) |> # Used anonymous link to complete survey (not preview)
      nrow() )
  } else if(study == "s2"){
    return(
      data |>
      filter(Status == 0) |> # Used anonymous link to complete survey (not preview)
      nrow()
      )
  }
}

basic_descriptives <- function(x, digits = 2){
  tibble(
    median = round(median(x, na.rm = TRUE), digits),
    mean = round(mean(x, na.rm = TRUE), digits),
    sd = round(sd(x, na.rm = TRUE), digits),
    min = round(min(x, na.rm = TRUE), digits),
    max = round(max(x, na.rm = TRUE), digits)
  )
}

descriptives_by_group <- function(df, formula) {
  dv <- all.vars(formula)[1]
  group <- all.vars(formula)[2]
  
  df %>%
    group_by(.data[[group]]) %>%
    summarise(
      n = n(),
      Min = min(.data[[dv]], na.rm = TRUE),
      Q1 = quantile(.data[[dv]], 0.25, na.rm = TRUE),
      Median = median(.data[[dv]], na.rm = TRUE),
      Mean = mean(.data[[dv]], na.rm = TRUE),
      Q3 = quantile(.data[[dv]], 0.75, na.rm = TRUE),
      Max = max(.data[[dv]], na.rm = TRUE),
      SD = sd(.data[[dv]], na.rm = TRUE),
      var = var(.data[[dv]], na.rm = TRUE),
      .groups = "drop"
    )
}

descriptives_by_group <- function(df, group_col, target_col) {
  df %>%
    group_by({{ group_col }}) %>%
    summarise(
      n = n(),
      Min = min({{ target_col }}, na.rm = TRUE),
      Q1 = quantile({{ target_col }}, 0.25, na.rm = TRUE),
      Median = median({{ target_col }}, na.rm = TRUE),
      Mean = mean({{ target_col }}, na.rm = TRUE),
      Q3 = quantile({{ target_col }}, 0.75, na.rm = TRUE),
      Max = max({{ target_col }}, na.rm = TRUE),
      SD = sd({{ target_col }}, na.rm = TRUE),
      var = var({{ target_col }}, na.rm = TRUE),
      .groups = "drop"
    )
}

