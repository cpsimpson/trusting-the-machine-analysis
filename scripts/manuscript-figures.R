# Manuscript figures ---------------------------------------------------------
#
# Violin plots for Figures 3, 4, 6 and 7, drawn from the same analysis data as
# the manuscript statistics. Brackets show Welch's t-tests (Bonferroni-corrected
# across the three comparisons in Experiment 1), the same tests as the text.
# Source after scripts/analysis-revision.R. Requires ggplot2 and ggpubr.

# Condition colours (fills with 50% alpha): low, high, human.
figure_fills <- c("#7F254350", "#19638950", "#2f258950")

figure_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "grey70"),
    axis.ticks = ggplot2::element_line(colour = "grey70"),
    axis.text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 16),
    legend.position = "none"
  )

# Significance symbols, as in ggpubr.
signif_cutpoints <- data.frame(
  upper = c(1e-4, 1e-3, 1e-2, 0.05, Inf),
  symbol = c("****", "***", "**", "*", "ns"),
  legend = c("$p < .0001$ (****)", "$p < .001$ (***)", "$p < .01$ (**)",
             "$p < .05$ (*)", "$p \\geq .05$ (ns)")
)
signif_symbol <- function(p) {
  vapply(p, function(x) signif_cutpoints$symbol[which(x < signif_cutpoints$upper)[1]], character(1))
}

# Caption text for the symbols that appear in a figure, e.g.
# "$p < .0001$ (****); $p \geq .05$ (ns)".
signif_legend <- function(symbols) {
  used <- signif_cutpoints$symbol %in% unique(symbols)
  paste(signif_cutpoints$legend[used], collapse = "; ")
}

# conditions: display label -> data label, in plotting order,
#   e.g. c(Low = "Low", High = "Medium", Human = "High").
# comparisons: pairs of display labels, drawn from the bottom bracket up.
violin_figure <- function(data, dv, y_label, conditions, comparisons) {
  data$group <- factor(names(conditions)[match(as.character(data$Condition), conditions)],
                       levels = names(conditions))
  n_comparisons <- length(comparisons)
  tests <- do.call(rbind, lapply(comparisons, function(cmp) {
    w <- welch_with_d(data, dv, conditions[[cmp[2]]], conditions[[cmp[1]]])
    data.frame(group1 = cmp[1], group2 = cmp[2], p = min(1, w$p * n_comparisons))
  }))
  tests$p.signif <- signif_symbol(tests$p)
  y <- data[[dv]]
  span <- diff(range(y))
  tests$y.position <- max(y) + span * (0.08 + 0.12 * (seq_len(nrow(tests)) - 1))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = group, y = .data[[dv]])) +
    ggplot2::geom_violin(ggplot2::aes(fill = group), alpha = 0.5) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = group), width = 0.1) +
    ggplot2::scale_fill_manual(values = figure_fills[seq_along(conditions)]) +
    ggplot2::labs(x = "Author Condition", y = y_label) +
    ggpubr::stat_pvalue_manual(tests, label = "p.signif", tip.length = 0.02, size = 5) +
    figure_theme
  list(plot = plot, tests = tests)
}

# Writes the six violin plots (PDF and PNG) and adds a significance-legend stat for
# each figure caption.
export_figures <- function(store, s1, s2, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  e1_conditions <- c(Low = "Low", High = "Medium", Human = "High")
  e1_comparisons <- list(c("Low", "High"), c("High", "Human"), c("Low", "Human"))
  e2_conditions <- c(Low = "Low", High = "High")
  e2_comparisons <- list(c("Low", "High"))
  outcomes <- c(anthropomorphism = "Anthropomorphism Rating",
                "content-trust" = "Content Trust", "author-trust" = "Author Trust")
  columns <- c(anthropomorphism = "anthro", "content-trust" = "content", "author-trust" = "author")

  symbols <- list()
  for (study in c("e1", "e2")) {
    data <- if (study == "e1") s1 else s2
    conditions <- if (study == "e1") e1_conditions else e2_conditions
    comparisons <- if (study == "e1") e1_comparisons else e2_comparisons
    for (outcome in names(outcomes)) {
      fig <- violin_figure(data, columns[[outcome]], outcomes[[outcome]], conditions, comparisons)
      # PDF for the LaTeX manuscript, PNG for the Word version (build-docx.sh).
      for (ext in c("pdf", "png")) {
        ggplot2::ggsave(file.path(out_dir, paste0(study, "-", outcome, ".", ext)), fig$plot,
                        width = 7, height = 7, dpi = 300)
      }
      symbols[[paste(study, outcome)]] <- fig$tests$p.signif
    }
    store$add(paste0("fig-", study, "-anthropomorphism-signif"),
              signif_legend(symbols[[paste(study, "anthropomorphism")]]))
    store$add(paste0("fig-", study, "-trust-signif"),
              signif_legend(c(symbols[[paste(study, "content-trust")]],
                              symbols[[paste(study, "author-trust")]])))
  }
  invisible(symbols)
}
