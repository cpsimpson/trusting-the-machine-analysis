library(tidyverse)
library(apa)
# library(jtools)
library(interactions)
library(rlang)   # for ensym/as_name
library(ppcor)
# --- Optional plotting.R hooks (used if available) ---
.theme_ai   <- get0("theme_ai",      ifnotfound = ggplot2::theme_minimal)
.palette_ai <- get0("palette_ai",    ifnotfound = function(n) scales::hue_pal()(n))
.save_figure<- get0("save_figure",   ifnotfound = function(plot, filename, width=8, height=5, dpi=300){ ggplot2::ggsave(filename, plot, width=width, height=height, dpi=dpi) })
.fig_size   <- get0("figure_size",   ifnotfound = function(name) list(width=8, height=5, dpi=300))

test_correlation <- function(data, x_name, y_name){
  x <- data |> pull({{x_name}})
  y <- data |> pull({{y_name}})

  cor_result <- cor.test(
    as.numeric(x), as.numeric(y), method = "pearson")

  print(cor_result)
  apa::cor_apa(cor_result, r_ci = TRUE)

}

linear_model <- function(data, formula){

  fit <- lm(formula, data = data)
  print(jtools::summ(fit, confint = TRUE, digits = 3, ci.width = .95))

  return(fit)
}

delta_r_boot <- function(data, x, y, z, R = 5000, seed = 123) {
  set.seed(seed)
  xv <- data[[x]]; yv <- data[[y]]; zv <- data[[z]]
  keep <- complete.cases(xv, yv, zv)
  xv <- xv[keep]; yv <- yv[keep]; zv <- zv[keep]
  n <- length(xv)
  
  # point estimates
  r_xy   <- cor(xv, yv)
  r_xy_z <- ppcor::pcor.test(xv, yv, zv)$estimate
  delta  <- as.numeric(r_xy - r_xy_z)
  
  # bootstrap
  deltas <- replicate(R, {
    idx <- sample.int(n, n, replace = TRUE)
    r_b   <- suppressWarnings(cor(xv[idx], yv[idx]))
    rp_b  <- suppressWarnings(ppcor::pcor.test(xv[idx], yv[idx], zv[idx])$estimate)
    as.numeric(r_b - rp_b)
  })
  
  ci <- quantile(deltas, c(.025, .975), na.rm = TRUE)
  # two-sided p from bootstrap (proportion crossing 0)
  p_boot <- 2 * min(mean(deltas >= 0, na.rm = TRUE), mean(deltas <= 0, na.rm = TRUE))
  
  list(
    n = n,
    r_xy = r_xy,
    r_xy_z = r_xy_z,
    delta_r = delta,
    ci95 = unname(ci),
    p_boot = p_boot,
    boot = deltas
  )
}

bootstrap_correlation_comparison <- function(
    data, x, y, z,
    method = "pearson",     # "pearson" or "spearman"
    R = 5000,               # bootstrap resamples
    seed = 123,
    labels = NULL,          # e.g., c(x="Anthro", y="Author trust", z="Content trust")
    verbose = TRUE
) {
  stopifnot(method %in% c("pearson", "spearman"))
  xv <- data[[x]]; yv <- data[[y]]; zv <- data[[z]]
  keep <- stats::complete.cases(xv, yv, zv)
  xv <- xv[keep]; yv <- yv[keep]; zv <- zv[keep]
  n <- length(xv)
  if (n < 10) stop("Not enough complete cases for a stable estimate.")
  
  # --- point estimates ---
  r_xy   <- suppressWarnings(stats::cor(xv, yv, method = method))
  rp_out <- ppcor::pcor.test(xv, yv, zv, method = method)
  r_xy_z <- as.numeric(rp_out$estimate)
  delta  <- as.numeric(r_xy - r_xy_z)
  
  # --- bootstrap Δr ---
  set.seed(seed)
  deltas <- replicate(R, {
    idx <- sample.int(n, n, replace = TRUE)
    rx  <- suppressWarnings(stats::cor(xv[idx], yv[idx], method = method))
    rpx <- tryCatch(
      as.numeric(ppcor::pcor.test(xv[idx], yv[idx], zv[idx], method = method)$estimate),
      error = function(e) NA_real_
    )
    rx - rpx
  })
  deltas <- deltas[is.finite(deltas)]
  ci <- stats::quantile(deltas, c(.025, .975), names = FALSE, type = 6)
  
  # two-sided bootstrap p: proportion of resampled Δr on the opposite side of 0
  p_boot <- {
    p_hi <- mean(deltas >= 0)
    p_lo <- mean(deltas <= 0)
    2 * min(p_hi, p_lo)
  }
  
  # helper: APA p
  fmt_p <- function(p) ifelse(p < .001, "p < .001", sprintf("p = %.3f", p))
  fmt_r <- function(r) sprintf("%.2f", r)
  
  # df for r tests
  df_zero    <- n - 2
  df_partial <- n - 3
  # p for zero-order (t test)
  p_zero <- {
    tval <- r_xy * sqrt(df_zero) / sqrt(1 - r_xy^2)
    2 * stats::pt(-abs(tval), df = df_zero)
  }
  # p for partial (from ppcor)
  p_partial <- as.numeric(rp_out$p.value)
  
  # optional header
  if (!is.null(labels) && all(c("x","y","z") %in% names(labels)) && verbose) {
    cat(sprintf("\n— %s ↔ %s (controlling %s) —\n",
                labels[["x"]], labels[["y"]], labels[["z"]]))
  }
  
  if (verbose) {
    cat("\n--- Zero-order vs. Partial correlation (bootstrap Δr) ---\n")
    cat(sprintf("Zero-order: r(%d) = %s, %s\n",
                df_zero, fmt_r(r_xy), fmt_p(p_zero)))
    cat(sprintf("Partial (controlling %s): r(%d) = %s, %s\n",
                ifelse(is.null(labels), z, labels[["z"]]),
                df_partial, fmt_r(r_xy_z), fmt_p(p_partial)))
    cat(sprintf("Difference: Δr = %.2f, 95%% CI [%.2f, %.2f], %s (R = %d)\n",
                delta, ci[1], ci[2], fmt_p(p_boot), R))
    cat(sprintf("N = %d, method = %s\n", n, method))
  }
  
  invisible(list(
    n = n,
    method = method,
    r = list(zero_order = r_xy, partial = r_xy_z),
    df = list(zero = df_zero, partial = df_partial),
    p = list(zero = p_zero, partial = p_partial, bootstrap_delta = p_boot),
    delta_r = delta,
    delta_ci95 = ci,
    boot = deltas
  ))
}


partial_correlation_test <- function(data, x_name, y_name, z_name){
  
  x <- data |> dplyr::pull({{x_name}})
  y <- data |> dplyr::pull({{y_name}})
  z <- data |> dplyr::pull({{z_name}})
  
  # --- Partial correlation ---
  pcor_result <- ppcor::pcor.test(
    x = x,
    y = y,
    z = z,
    method = "pearson"
  )
  
  # --- Zero-order correlations ---
  r_xy <- cor(x, y, use = "pairwise.complete.obs")
  r_xz <- cor(x, z, use = "pairwise.complete.obs")
  r_yz <- cor(y, z, use = "pairwise.complete.obs")
  n <- sum(complete.cases(x, y, z))
  
  print(paste("r_xy = ", r_xy, "r_xz = ", r_xz, "r_yz = ", r_yz))
  # --- Test: difference between r(X,Y) and partial r(X,Y|Z) ---
  cocor_result <- cocor::cocor.dep.groups.overlap(
    r.jk = r_xy,  # r(X,Y)
    r.jh = r_xz,  # r(X,Z)
    r.kh = r_yz,  # r(Y,Z)
    n = n,
    alternative = "two.sided",
    test = "all"  # gives Steiger, Williams, Hotelling, etc.
  )
  
  # --- Output ---
  cat("\nPartial correlation (ppcor):\n")
  print(pcor_result)
  
  cat("\nZero-order vs partial correlation test (cocor):\n")
  print(cocor_result)
  
  # return invisibly so it can be captured if needed
  invisible(list(
    partial = pcor_result,
    cocor = cocor_result
  ))
}

# ===============================================================
# Reusable partial-correlation helpers & plotting utilities
# These prefer helpers from plotting.R when available:
#   - theme_ai(), palette_ai(n), save_figure(plot, filename, width, height, dpi),
#   - figure_size(key) -> list(width,height,dpi)
# If absent, safe fallbacks are used.
# ===============================================================

# Vectorized stars
p_stars <- function(p) {
  dplyr::case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
}

# ---------- Core computation ----------
partial_corr_df <- function(data, x, y, z, method = c("pearson","spearman")) {
  method <- match.arg(method)
  df <- data %>% dplyr::select(all_of(c(x, y, z))) %>% tidyr::drop_na()
  res <- ppcor::pcor.test(x = df[[x]], y = df[[y]], z = df[[z]], method = method)
  tibble::tibble(
    x = x, y = y, control = z,
    estimate = as.numeric(res$estimate),
    p.value  = as.numeric(res$p.value),
    statistic = as.numeric(res$statistic),
    n = nrow(df),
    gp = length(z),
    method = method
  )
}



# ---------- Bar plot: raw vs partial ----------
plot_corr_vs_partial_bars <- function(data, study, name, 
                                      x, y, z,
                                      x_label, y_label, z_label,
                                      save_path = NULL,
                                      size_key = "corr_vs_partial_bars") {
  fs <- .fig_size(size_key); if (is.null(fs$dpi)) fs$dpi <- 300
  df <- data %>% dplyr::select(all_of(c(x, y, z))) %>% tidyr::drop_na()

  cor_xy  <- stats::cor.test(df[[x]], df[[y]])
  cor_xz  <- stats::cor.test(df[[x]], df[[z]])
  cor_yz  <- stats::cor.test(df[[y]], df[[z]])
  pcor_xy <- ppcor::pcor.test(x = df[[x]], y = df[[y]], z = df[[z]])
  pcor_xz <- ppcor::pcor.test(x = df[[x]], y = df[[z]], z = df[[y]])
  pcor_yz <- ppcor::pcor.test(x = df[[y]], y = df[[z]], z = df[[x]])

  plot_df <- tibble::tibble(
    pair = c(paste(x_label, y_label, sep = " – "), paste(x_label, y_label, sep = " – "), 
             paste(x_label, z_label, sep = " – "), paste(x_label, z_label, sep = " – "),
             paste(y_label, z_label, sep = " – "), paste(y_label, z_label, sep = " – ")),
    type = factor(c("Zero-order", paste0("Partial (adjust for ", z_label, ")"), 
                    "Zero-order", paste0("Partial (adjust for ", y_label, ")"), 
                    "Zero-order", paste0("Partial (adjust for ", x_label, ")")),
                  levels = c("Zero-order", 
                             paste0("Partial (adjust for ", z_label, ")"), 
                             paste0("Partial (adjust for ", y_label, ")"), 
                             paste0("Partial (adjust for ", x_label, ")") 
                             )
                  ),
    r = c(unname(cor_xy$estimate), unname(pcor_xy$estimate),
          unname(cor_xz$estimate), unname(pcor_xz$estimate), 
          unname(cor_yz$estimate), unname(pcor_yz$estimate)),
    p = c(cor_xy$p.value, pcor_xy$p.value, 
          cor_xz$p.value, pcor_xz$p.value, 
          cor_yz$p.value, pcor_yz$p.value)
  ) %>% dplyr::mutate(label = sprintf("%.2f%s", r, p_stars(p)))

  g <- ggplot2::ggplot(plot_df, ggplot2::aes(x = pair, y = r, fill = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label),
              position = ggplot2::position_dodge(width = 0.7),
              vjust = ifelse(plot_df$r >= 0, -0.4, 1.2), size = 4) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = paste("Bivariate vs Partial Correlations for", x_label, ",", y_label, ",", z_label),
      x = NULL, y = "Correlation (r)", fill = NULL
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.4, 0.8)) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)  # no rotation needed
      #axis.text.x = ggplot2::element_text(angle = 45, hjust = 1) # <-- rotation
    )

  
  filename <- paste0("covar_", name, ".png")
  save_path <- paste(sep = "/", "plots", study, filename)
  
  if (!is.null(save_path)) .save_figure(g, save_path, width = fs$width, height = fs$height, dpi = fs$dpi)
  g
}



# ---------- Convenience wrapper ----------
generate_partial_correlation_figures <- function(data, x, y1, y2, out_dir = NULL) {
  if (!is.null(out_dir) && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  plot_partial_residuals(data, x = x, y = y1, z = y2,
                         save_path = if (!is.null(out_dir)) file.path(out_dir, "partials_residuals.png") else NULL)
  plot_correlation_network(data, vars = c(x, y1, y2), type = "bivariate",
                           labels = c(x, y1, y2),
                           save_path = if (!is.null(out_dir)) file.path(out_dir, "network_bivariate.png") else NULL)
  plot_correlation_network(data, vars = c(x, y1, y2), type = "partial",
                           labels = c(x, y1, y2),
                           save_path = if (!is.null(out_dir)) file.path(out_dir, "network_partial.png") else NULL)
  plot_corr_vs_partial_bars(data, x = x, y = y1, z = y2,
                            save_path = if (!is.null(out_dir)) file.path(out_dir, "corr_vs_partial_bars.png") else NULL)
  plot_variance_partition(data, y = y1, x = x, z = y2,
                          set_x = x, set_z = y2,
                          title = sprintf("Variance in %s explained by %s and %s", y1, x, y2),
                          save_path = if (!is.null(out_dir)) file.path(out_dir, "venn_y1.png") else NULL)
  plot_variance_partition(data, y = y2, x = x, z = y1,
                          set_x = x, set_z = y1,
                          title = sprintf("Variance in %s explained by %s and %s", y2, x, y1),
                          save_path = if (!is.null(out_dir)) file.path(out_dir, "venn_y2.png") else NULL)
  plot_partial_three_venn(data, a = x, b = y1, c = y2,
                          save_path = if (!is.null(out_dir)) file.path(out_dir, "venn_three_partials.png") else NULL)
  invisible(TRUE)
}
