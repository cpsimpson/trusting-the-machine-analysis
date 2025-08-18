#!/usr/bin/env Rscript

# run_pipeline.R

# Load required packages ----
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("The 'rmarkdown' package is required. Install it with install.packages('rmarkdown').")
}

# Create rendered/ folder if it doesn't already exist ----
out_dir <- "rendered"
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# 1. Knit power-analysis.Rmd ----
rmarkdown::render(
  input      = "power-analysis.Rmd",
  output_dir = out_dir
)

# 2. Knit analysis-experiment-1.qmd ----
rmarkdown::render(
  input      = "analysis-experiment-1.qmd",
  output_dir = out_dir
)

# 3. Knit analysis-experiment-2.qmd ----
rmarkdown::render(
  input      = "analysis-experiment-2.qmd",
  output_dir = out_dir
)

cat("Pipeline finished successfully.\n")