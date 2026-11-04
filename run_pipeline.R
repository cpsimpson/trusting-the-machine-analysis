#!/usr/bin/env Rscript

# run_pipeline.R
#
# Reproduces everything the manuscript reports. Run from the project root:
#   Rscript run_pipeline.R
# Set MANUSCRIPT_DIR to also copy the generated files into the manuscript
# repository, e.g. MANUSCRIPT_DIR=../trusting-the-machine-manuscript.

out_dir <- "rendered"
if (!dir.exists(out_dir)) dir.create(out_dir)

# 1. A priori power analysis (design of both experiments) ----
if (requireNamespace("rmarkdown", quietly = TRUE)) {
  tryCatch(rmarkdown::render(input = "power-analysis.Rmd", output_dir = out_dir),
           error = function(e) message("power-analysis.Rmd was not rendered: ", conditionMessage(e)))
} else {
  message("Skipping power-analysis.Rmd: install the 'rmarkdown' package to render it.")
}

# 2. Statistics, tables, and figures used in the manuscript ----
source("export-manuscript-stats.R", local = new.env())

# 3. R and package versions and their citations ----
source("export-software-citations.R", local = new.env())

# 4. Readable output of all revision analyses ----
sink(file.path(out_dir, "revision-analyses-output.txt"))
source("revision-analyses.R", local = new.env())
sink()

cat("Pipeline finished successfully.\n")
