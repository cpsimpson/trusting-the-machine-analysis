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

  apa::cor_apa(cor_result, r_ci = TRUE)

}

linear_model <- function(data, formula){

  fit <- lm(formula, data = data)
  print(jtools::summ(fit, confint = TRUE, digits = 3, ci.width = .95))

  return(fit)
}

partial_correlation_test <- function(data, x_name, y_name, z_name){

  x <- data |> pull({{x_name}})
  y <- data |> pull({{y_name}})
  z <- data |> pull({{z_name}})

  pcor_result <- ppcor::pcor.test(
    x = x,
    y = y,
    z = z,
    method = "pearson" # can also use "spearman" if data not normal
  )

  print(pcor_result)

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

# ---------- Residual scatterplots (two panels) ----------
plot_partial_residuals <- function(data, x, y, z,
                                   method = "pearson",
                                   save_path = NULL,
                                   size_key = "partials_residuals") {
  suppressPackageStartupMessages(library(patchwork))
  fs <- .fig_size(size_key); if (is.null(fs$dpi)) fs$dpi <- 300

  df <- data %>% dplyr::select(all_of(c(x, y, z))) %>% tidyr::drop_na()

  # A: r(x,y|z)
  res_x_z <- resid(lm(df[[x]] ~ df[[z]]))
  res_y_z <- resid(lm(df[[y]] ~ df[[z]]))
  rA <- stats::cor(res_x_z, res_y_z, use = "complete.obs",
                   method = ifelse(method=="spearman","spearman","pearson"))
  pA <- stats::cor.test(res_x_z, res_y_z, method = ifelse(method=="spearman","spearman","pearson"))$p.value

  pA_plot <- tibble::tibble(rx = res_x_z, ry = res_y_z) %>%
    ggplot2::ggplot(ggplot2::aes(rx, ry)) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    ggplot2::labs(
      title = sprintf("%s \u2194 %s (controlling %s)", x, y, z),
      subtitle = sprintf("partial r = %.2f%s", rA, p_stars(pA)),
      x = sprintf("Residual %s | %s", x, z),
      y = sprintf("Residual %s | %s", y, z)
    ) +
    .theme_ai()

  # B: r(x,z|y)
  res_x_y <- resid(lm(df[[x]] ~ df[[y]]))
  res_z_y <- resid(lm(df[[z]] ~ df[[y]]))
  rB <- stats::cor(res_x_y, res_z_y, use = "complete.obs",
                   method = ifelse(method=="spearman","spearman","pearson"))
  pB <- stats::cor.test(res_x_y, res_z_y, method = ifelse(method=="spearman","spearman","pearson"))$p.value

  pB_plot <- tibble::tibble(rx = res_x_y, ry = res_z_y) %>%
    ggplot2::ggplot(ggplot2::aes(rx, ry)) +
    ggplot2::geom_point(alpha = 0.55) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    ggplot2::labs(
      title = sprintf("%s \u2194 %s (controlling %s)", x, z, y),
      subtitle = sprintf("partial r = %.2f%s", rB, p_stars(pB)),
      x = sprintf("Residual %s | %s", x, y),
      y = sprintf("Residual %s | %s", z, y)
    ) +
    .theme_ai()

  out <- pA_plot + pB_plot
  if (!is.null(save_path)) .save_figure(out, save_path, width = fs$width, height = fs$height, dpi = fs$dpi)
  out
}

# ---------- Correlation / Partial-correlation network ----------
plot_correlation_network <- function(data, vars, type = c("bivariate","partial"),
                                     labels = NULL,
                                     save_path = NULL,
                                     size_key = "network") {
  if (!requireNamespace("qgraph", quietly = TRUE)) stop("Package 'qgraph' required.")
  if (!requireNamespace("corpcor", quietly = TRUE)) stop("Package 'corpcor' required.")
  fs <- .fig_size(size_key); if (is.null(fs$width)) fs$width <- 11; if (is.null(fs$height)) fs$height <- 9; if (is.null(fs$dpi)) fs$dpi <- 300

  type <- match.arg(type)
  if (is.null(labels)) labels <- vars

  M <- data %>% dplyr::select(all_of(vars)) %>% tidyr::drop_na() %>% as.matrix()
  cor_mat <- stats::cor(M)
  W <- if (type == "bivariate") cor_mat else corpcor::cor2pcor(cor_mat)

  par(mfrow = c(1,1))
  q <- qgraph::qgraph(
    W,
    layout = "spring",
    labels = labels,
    title = ifelse(type=="bivariate","Bivariate correlations","Partial correlations (control others)"),
    edge.labels = TRUE,
    edge.label.cex = 1.2,
    edge.label.position = 0.5,
    edge.label.bg = "white",
    label.scale = FALSE
  )
  if (!is.null(save_path)) {
    grDevices::dev.copy(png, filename = save_path, width = fs$width*96, height = fs$height*96) ; invisible(grDevices::dev.off())
  }
  invisible(q)
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
  pcor_xy <- ppcor::pcor.test(x = df[[x]], y = df[[y]], z = df[[z]])
  pcor_xz <- ppcor::pcor.test(x = df[[x]], y = df[[z]], z = df[[y]])

  plot_df <- tibble::tibble(
    pair = c(paste(x_label, y_label, sep = "–"), paste(x_label, y_label, sep = "–"), paste(x_label, z_label, sep = "–"), 
             paste(x_label, z_label, sep = "–")),
    type = factor(c("Bivariate", "Partial (control other)", "Bivariate", "Partial (control other)"),
                  levels = c("Bivariate", "Partial (control other)")),
    r = c(unname(cor_xy$estimate), unname(pcor_xy$estimate),
          unname(cor_xz$estimate), unname(pcor_xz$estimate)),
    p = c(cor_xy$p.value, pcor_xy$p.value, cor_xz$p.value, pcor_xz$p.value)
  ) %>% dplyr::mutate(label = sprintf("%.2f%s", r, p_stars(p)))

  g <- ggplot2::ggplot(plot_df, ggplot2::aes(x = pair, y = r, fill = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label),
              position = ggplot2::position_dodge(width = 0.7),
              vjust = ifelse(plot_df$r >= 0, -0.4, 1.2), size = 4) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = .palette_ai(2)) +
    ggplot2::labs(
      title = "Bivariate vs Partial Correlations",
      x = NULL, y = "Correlation (r)", fill = NULL,
      caption = "Stars: * < .05, ** < .01, *** < .001"
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.4, 0.8)) +
    .theme_ai()

  
  filename <- paste0("covar_", name, ".png")
  save_path <- paste(sep = "/", "plots", study, filename)
  
  if (!is.null(save_path)) .save_figure(g, save_path, width = fs$width, height = fs$height, dpi = fs$dpi)
  g
}

# ---------- Variance partition (single outcome) ----------
variance_partition <- function(data, y, x, z) {
  df <- data %>% dplyr::select(all_of(c(y, x, z))) %>% tidyr::drop_na()
  f_full    <- lm(df[[y]] ~ df[[x]] + df[[z]])
  f_minus_x <- lm(df[[y]] ~ df[[z]])
  f_minus_z <- lm(df[[y]] ~ df[[x]])

  R2_full <- summary(f_full)$r.squared
  R2_mx   <- summary(f_minus_x)$r.squared
  R2_mz   <- summary(f_minus_z)$r.squared

  sr2_x  <- R2_full - R2_mx
  sr2_z  <- R2_full - R2_mz
  shared <- max(R2_full - sr2_x - sr2_z, 0)

  list(R2_full = R2_full, sr2_x = sr2_x, sr2_z = sr2_z, shared = shared)
}

plot_variance_partition <- function(data, y, x, z,
                                    set_x = x, set_z = z,
                                    title = NULL,
                                    save_path = NULL,
                                    size_key = "venn_single") {
  if (!requireNamespace("eulerr", quietly = TRUE)) stop("Package 'eulerr' required.")
  fs <- .fig_size(size_key); if (is.null(fs$dpi)) fs$dpi <- 300; if (is.null(fs$width)) fs$width <- 7; if (is.null(fs$height)) fs$height <- 5.5

  parts <- variance_partition(data, y, x, z)
  areas <- c(parts$sr2_x + parts$shared, parts$sr2_z + parts$shared, parts$shared)
  names(areas) <- c(set_x, set_z, paste(set_x, set_z, sep = "&"))

  fit <- tryCatch(eulerr::euler(areas, shape = "ellipse"),
                  error = function(e) eulerr::euler(areas))

  g <- plot(
    fit,
    quantities = list(type = "percent", fmt = function(p) sprintf("%.1f%%", p)),
    fills = list(fill = .palette_ai(2), alpha = 0.85),
    edges = list(col = "grey20"),
    labels = list(col = "black", cex = 1.15),
    main = sprintf("%s\nTotal R² = %.2f",
                   ifelse(is.null(title), sprintf("Variance in %s explained by %s and %s", y, x, z), title),
                   parts$R2_full)
  )
  if (!is.null(save_path)) ggplot2::ggsave(save_path, g, width = fs$width, height = fs$height, dpi = fs$dpi)
  g
}

# ---------- Three-circle diagram with partial r labels ----------
plot_partial_three_venn <- function(data, a, b, c,
                                    save_path = NULL,
                                    circle_alpha = 0.55,
                                    circle_r = 1.75,
                                    size_key = "venn_three_partials") {
  if (!requireNamespace("ggforce", quietly = TRUE)) stop("Package 'ggforce' required.")
  fs <- .fig_size(size_key); if (is.null(fs$dpi)) fs$dpi <- 300; if (is.null(fs$width)) fs$width <- 7.5; if (is.null(fs$height)) fs$height <- 6.5

  dat <- data %>% dplyr::select(all_of(c(a, b, c))) %>% tidyr::drop_na()

  p_ab_c <- ppcor::pcor.test(x = dat[[a]], y = dat[[b]], z = dat[[c]])$estimate
  p_ac_b <- ppcor::pcor.test(x = dat[[a]], y = dat[[c]], z = dat[[b]])$estimate
  p_bc_a <- ppcor::pcor.test(x = dat[[b]], y = dat[[c]], z = dat[[a]])$estimate

  centers <- tibble::tibble(
    name = c(a, b, c),
    x = c(-1.8, 1.8, 0.0),
    y = c(0.0, 0.0, 2.2),
    fill = .palette_ai(3)
  )

  overlap_pts <- tibble::tibble(
    pair = c(paste(a,b,sep="–"), paste(a,c,sep="–"), paste(b,c,sep="–")),
    x = c(mean(c(-1.8, 1.8)),
          mean(c(-1.8, 0.0)) - 0.10,
          mean(c(1.8, 0.0)) + 0.10),
    y = c(mean(c(0.0, 0.0)) - 0.15,
          mean(c(0.0, 2.2)) + 0.05,
          mean(c(0.0, 2.2)) + 0.05),
    partial = sprintf("partial r = %s",
                      c(sprintf("%.2f", p_ab_c), sprintf("%.2f", p_ac_b), sprintf("%.2f", p_bc_a)))
  )

  g <- ggplot2::ggplot() +
    ggforce::geom_circle(data = centers,
                         ggplot2::aes(x0 = x, y0 = y, r = circle_r, fill = fill),
                         color = "grey20", alpha = circle_alpha, linewidth = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(data = centers,
              ggplot2::aes(x = x, y = y + circle_r + 0.25, label = name),
              size = 5.2, fontface = "bold") +
    ggplot2::geom_label(data = overlap_pts,
               ggplot2::aes(x = x, y = y, label = partial),
               size = 4.2, label.padding = grid::unit(0.15, "lines"),
               label.size = 0, fill = "white") +
    ggplot2::coord_equal(xlim = c(-3.8, 3.8), ylim = c(-1.2, 4.2), expand = FALSE) +
    ggplot2::labs(title = "Partial correlations in overlaps (areas not to scale)") +
    .theme_ai()

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
