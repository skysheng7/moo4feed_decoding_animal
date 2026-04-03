###################################################################################################
################################## 12b. Predictability — Results & Diagnostics ###################
###################################################################################################
# Prerequisites: run 12a_predictability_models.r first (or ensure .rds files exist under
# results/12_predictability/).  master_data must also be available.

library(brms)
library(coda)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggrepel)
library(ggforce)
library(bayesplot)
library(moo4feed)

output_dir <- "results/12_predictability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Load master_data if not already in memory -----------------------------------------------
if (!exists("master_data")) {
  load("results/11_repeatability/master_data.rda")
}

response_vars <- c(
  "feed_intake", "feed_duration", "feed_visits",
  "water_intake", "water_duration", "water_visits",
  "total_meals", "median_meal_duration", "median_visit_per_meal",
  "median_intake_per_meal", "median_feeding_pct_per_meal",
  "number_of_non_nutritive_visits", "median_pct_feed_remaining",
  "median_non_nutritive_per_meal", "total_actor", "total_reactor",
  "median_feed_rate", "median_water_rate"
)

# ---- Load models from .rds if not already in memory ------------------------------------------
if (!exists("models")) {
  models <- list()
}
for (rv in response_vars) {
  if (!is.null(models[[rv]])) next
  rds_path <- file.path(output_dir, paste0("m2_brm_", rv, ".rds"))
  if (!file.exists(rds_path)) {
    message("Skipping ", rv, " (file not found: ", rds_path, ")")
    next
  }
  cat("Loading:", rv, "\n")
  models[[rv]] <- readRDS(rds_path)
}
response_vars <- intersect(response_vars, names(models))

###################################################################################################
################################## Helper: extract IIV summaries ##################################
###################################################################################################
# rIIV  = variance of the sigma random effect (on the log scale)
# CVP   = coefficient of variation of predictability = sqrt(exp(sd^2) - 1)
extract_iiv <- function(m2_brm, response_var) {
  ps  <- as_draws_df(m2_brm)
  fam <- family(m2_brm)$family

  sd_sigma <- ps$"sd_cow__sigma_Intercept"

  var_res  <- exp(sd_sigma)^2
  log_norm <- exp(sd_sigma^2)
  CVP      <- sqrt(log_norm - 1)

  cat("\n===", response_var, "(family:", fam, ") ===\n")
  cat("rIIV (mean):  ", round(mean(var_res), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(var_res), 0.95), 4), "\n")
  cat("CVP  (mean):  ", round(mean(CVP), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVP), 0.95), 4), "\n")

  list(
    response = response_var,
    family   = fam,
    var_res  = var_res,
    CVP      = CVP
  )
}

###################################################################################################
################################## Extract IIV for all variables ##################################
###################################################################################################
iiv_results <- mapply(
  extract_iiv,
  m2_brm       = models,
  response_var = response_vars,
  SIMPLIFY     = FALSE
)
names(iiv_results) <- response_vars

###################################################################################################
################################## Summary table #################################################
###################################################################################################
results_table <- do.call(rbind, lapply(iiv_results, function(p) {
  data.frame(
    variable       = p$response,
    family         = p$family,
    rIIV_mean      = round(mean(p$var_res), 4),
    rIIV_lower     = round(HPDinterval(as.mcmc(p$var_res), 0.95)[1], 4),
    rIIV_upper     = round(HPDinterval(as.mcmc(p$var_res), 0.95)[2], 4),
    CVP_mean       = round(mean(p$CVP), 4),
    CVP_lower      = round(HPDinterval(as.mcmc(p$CVP), 0.95)[1], 4),
    CVP_upper      = round(HPDinterval(as.mcmc(p$CVP), 0.95)[2], 4)
  )
}))

print(results_table)
write.csv(results_table,
          file.path(output_dir, "predictability_summary.csv"),
          row.names = FALSE)

###################################################################################################
################################## Posterior IIV ridge plots ######################################
###################################################################################################
plot_posterior_iiv <- function(m2_brm, response_var,
                               out_dir = "results/12_predictability") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  fam      <- family(m2_brm)$family
  ps       <- as_draws_df(m2_brm)
  iiv_cols <- grep("^r_cow__sigma\\[", names(ps), value = TRUE)

  posteriorIIV <- ps[, iiv_cols] %>%
    tidyr::pivot_longer(
      cols      = everything(),
      names_to  = "raw_col",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      cow = sub("^r_cow__sigma\\[(.+),Intercept\\]$", "\\1", raw_col)
    ) %>%
    dplyr::select(-raw_col)

  posteriorIIV$value <- posteriorIIV$value +
    fixef(m2_brm, pars = "sigma_Intercept")[1]

  posteriorIIV$value <- exp(posteriorIIV$value)

  posteriorIIV <- posteriorIIV %>%
    dplyr::group_by(cow) %>%
    dplyr::mutate(meanIIV = mean(value)) %>%
    dplyr::ungroup()

  focal_cows   <- c("5042", "5120", "6022", "7169", "3067")
  focal_colors <- c("#C77CFF", "#F8766D", "#7CAE00", "#FFCC00", "#00BFC4")
  names(focal_colors) <- focal_cows

  posteriorIIV$col <- ifelse(
    posteriorIIV$cow %in% focal_cows,
    posteriorIIV$cow,
    "Other individuals"
  )

  fill_values <- c(focal_colors, "Other individuals" = "gray")
  n_cows      <- dplyr::n_distinct(posteriorIIV$cow)

  cow_summary <- posteriorIIV %>%
    dplyr::distinct(cow, meanIIV, col) %>%
    dplyr::mutate(cow_ordered = reorder(as.factor(cow), meanIIV))

  IIV_plot <- ggplot() +
    ggridges::geom_density_ridges(
      data  = posteriorIIV,
      aes(x    = value,
          y    = reorder(as.factor(cow), meanIIV),
          fill = col),
      scale = 3,
      alpha = 0.6
    ) +
    geom_point(
      data = cow_summary,
      aes(x = meanIIV, y = cow_ordered, col = col),
      size = 1
    ) +
    labs(y = "",
         x = paste0("Within-individual variation \u2014 ", response_var,
                     if (fam == "lognormal") " (SD on log scale)"
                     else " (SD on original scale)"),
         fill = "ID", col = "ID") +
    theme_classic() +
    scale_fill_manual(values  = fill_values) +
    scale_color_manual(values = fill_values)

  ggsave(
    filename = file.path(out_dir, paste0("IIV_plot_", response_var, ".png")),
    plot     = IIV_plot,
    width    = 8,
    height   = max(4, n_cows * 0.35)
  )

  return(IIV_plot)
}

iiv_plots <- list()
for (rv in names(models)) {
  iiv_plots[[rv]] <- plot_posterior_iiv(models[[rv]], rv)
}

###################################################################################################
############################### Model diagnostics #################################################
###################################################################################################
diag_dir <- file.path(output_dir, "diagnostics")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

diag_summary <- data.frame(
  variable           = character(),
  family             = character(),
  expected_ESS       = numeric(),
  min_Bulk_ESS       = numeric(),
  Bulk_ESS_pct       = numeric(),
  min_Tail_ESS       = numeric(),
  Tail_ESS_pct       = numeric(),
  max_Rhat           = numeric(),
  n_divergent        = integer(),
  pareto_k_above_07  = integer(),
  flag               = character(),
  stringsAsFactors   = FALSE
)

for (rv in response_vars) {
  rds_path <- file.path(output_dir, paste0("m2_brm_", rv, ".rds"))
  if (!file.exists(rds_path)) {
    message("Skipping ", rv, " (file not found: ", rds_path, ")")
    next
  }

  cat("\n", strrep("=", 60), "\n")
  cat("Diagnosing:", rv, "\n")
  cat(strrep("=", 60), "\n\n")

  m <- readRDS(rds_path)

  s <- summary(m)
  print(s)

  fe <- s$fixed
  re <- do.call(rbind, s$random)
  spec <- s$spec_pars
  all_params <- rbind(fe, re)
  if (!is.null(spec) && nrow(spec) > 0) all_params <- rbind(all_params, spec)

  max_rhat     <- max(all_params[, "Rhat"],     na.rm = TRUE)
  min_bulk_ess <- min(all_params[, "Bulk_ESS"], na.rm = TRUE)
  min_tail_ess <- min(all_params[, "Tail_ESS"], na.rm = TRUE)

  n_iter   <- m$fit@sim$iter
  n_warmup <- m$fit@sim$warmup
  n_thin   <- m$fit@sim$thin
  n_chains <- m$fit@sim$chains
  expected_ess <- ((n_iter - n_warmup) / n_thin) * n_chains

  cat("  Expected ESS:   ", expected_ess,
      sprintf(" (iter=%d, warmup=%d, thin=%d, chains=%d)\n",
              n_iter, n_warmup, n_thin, n_chains))
  cat("  Max Rhat:       ", round(max_rhat, 4), "\n")
  cat("  Min Bulk ESS:   ", min_bulk_ess,
      sprintf(" (%.1f%% of expected)\n", 100 * min_bulk_ess / expected_ess))
  cat("  Min Tail ESS:   ", min_tail_ess,
      sprintf(" (%.1f%% of expected)\n", 100 * min_tail_ess / expected_ess))

  np    <- nuts_params(m)
  n_div <- sum(np$Value[np$Parameter == "divergent__"])
  cat("  Divergent transitions: ", n_div, "\n")

  pp_dens <- pp_check(m, ndraws = 100) + ggplot2::ggtitle(paste("pp_check:", rv))
  ggsave(file.path(diag_dir, paste0("pp_check_", rv, ".png")),
         pp_dens, width = 7, height = 4)

  pp_mean <- pp_check(m, type = "stat", stat = "mean") +
    ggplot2::ggtitle(paste("pp_check mean:", rv))
  ggsave(file.path(diag_dir, paste0("pp_check_mean_", rv, ".png")),
         pp_mean, width = 7, height = 4)

  pp_sd <- pp_check(m, type = "stat", stat = "sd") +
    ggplot2::ggtitle(paste("pp_check sd:", rv))
  ggsave(file.path(diag_dir, paste0("pp_check_sd_", rv, ".png")),
         pp_sd, width = 7, height = 4)

  loo_m    <- loo(m)
  n_high_k <- sum(loo_m$diagnostics$pareto_k > 0.7)
  cat("  LOO Pareto k > 0.7: ", n_high_k, "obs\n")
  print(loo_m)

  has_issues <- (max_rhat > 1.01) || (min_bulk_ess < 1000) ||
                (min_tail_ess < 1000) || (n_div > 0)

  if (has_issues) {
    cat("  >> Issues detected — saving trace plots\n")
    trace_plt <- mcmc_plot(m, type = "trace") +
      ggplot2::ggtitle(paste("Trace:", rv))
    ggsave(file.path(diag_dir, paste0("trace_", rv, ".png")),
           trace_plt, width = 10, height = min(49, max(4, 1.5 * length(variables(m)))))
  }

  extract_iiv(m, rv)

  flags <- character()
  if (max_rhat > 1.01)     flags <- c(flags, "high_Rhat")
  if (min_bulk_ess < 1000)  flags <- c(flags, "low_Bulk_ESS")
  if (min_tail_ess < 1000)  flags <- c(flags, "low_Tail_ESS")
  if (n_div > 0)            flags <- c(flags, "divergences")
  if (n_high_k > 0)         flags <- c(flags, paste0("pareto_k(", n_high_k, ")"))

  diag_summary <- rbind(diag_summary, data.frame(
    variable           = rv,
    family             = family(m)$family,
    expected_ESS       = expected_ess,
    min_Bulk_ESS       = min_bulk_ess,
    Bulk_ESS_pct       = round(100 * min_bulk_ess / expected_ess, 1),
    min_Tail_ESS       = min_tail_ess,
    Tail_ESS_pct       = round(100 * min_tail_ess / expected_ess, 1),
    max_Rhat           = round(max_rhat, 4),
    n_divergent        = n_div,
    pareto_k_above_07  = n_high_k,
    flag               = if (length(flags) == 0) "OK" else paste(flags, collapse = "; "),
    stringsAsFactors   = FALSE
  ))
}

cat("\n\n", strrep("=", 70), "\n")
cat("DIAGNOSTICS SUMMARY\n")
cat(strrep("=", 70), "\n\n")
print(diag_summary, right = FALSE)

flagged <- diag_summary[diag_summary$flag != "OK", ]
if (nrow(flagged) > 0) {
  cat("\nModels requiring attention:\n")
  print(flagged, right = FALSE)
} else {
  cat("\nAll models passed convergence and fit checks.\n")
}

write.csv(diag_summary,
          file.path(output_dir, "diagnostics_summary.csv"),
          row.names = FALSE)

###################################################################################################
################################## Repeatability vs Predictability scatter ########################
###################################################################################################
rep_summary  <- moo4feed::read_data_safely("results/11_repeatability/repeatability_summary.csv",
                                           header = TRUE, sep = ",")
pred_summary <- moo4feed::read_data_safely("results/12_predictability/predictability_summary.csv",
                                           header = TRUE, sep = ",")

combined_summary <- rep_summary %>%
  dplyr::select(variable, family, CVi_mean, CVi_lower, CVi_upper) %>%
  dplyr::inner_join(
    pred_summary %>% dplyr::select(variable, CVP_mean, CVP_lower, CVP_upper,
                                   rIIV_mean, rIIV_lower, rIIV_upper),
    by = "variable"
  ) %>%
  dplyr::mutate(label = gsub("_", " ", variable))

n_vars     <- nrow(combined_summary)
colour_pal <- scales::hue_pal()(n_vars)
names(colour_pal) <- combined_summary$variable

# ---- Scatter 1: CVP vs CVi (ellipses = 95 % credible intervals) ----
combined_summary <- combined_summary %>%
  dplyr::mutate(
    a_cvi_cvp = (CVi_upper - CVi_lower) / 2,
    b_cvi_cvp = (CVP_upper - CVP_lower) / 2
  )

scatter_CVP_CVi <- ggplot(combined_summary,
                          aes(x0 = CVi_mean, y0 = CVP_mean,
                              a = a_cvi_cvp, b = b_cvi_cvp, angle = 0,
                              fill = variable, colour = variable)) +
  ggforce::geom_ellipse(alpha = 0.7, linewidth = 0.4) +
  geom_point(aes(x = CVi_mean, y = CVP_mean, colour = variable,
                 shape = family),
             size = 2.5, show.legend = TRUE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = CVi_mean, y = CVP_mean, label = label, fill = variable),
    colour             = "black",
    fontface           = "bold",
    size               = 4,
    alpha              = 0.5,
    label.padding      = unit(0.15, "lines"),
    box.padding        = unit(0.5, "lines"),
    point.padding      = unit(0.3, "lines"),
    force              = 50,
    force_pull         = 0.3,
    max.iter           = 20000,
    direction          = "both",
    min.segment.length = 0,
    segment.size       = 0.3,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    inherit.aes        = FALSE
  ) +
  scale_x_continuous(limits = c(0, 0.65), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0, 0.65), expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_manual(values   = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  scale_shape_manual(values = c("gaussian" = 16, "lognormal" = 17)) +
  labs(
    x     = "CVi (Repeatability \u2014 between-individual variation)",
    y     = "CVP (Predictability \u2014 within-individual variation)",
    shape = "Likelihood family"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    axis.title      = element_text(size = 18),
    axis.text       = element_text(size = 14)
  ) +
  guides(fill = "none", colour = "none")

ggsave(
  filename = file.path(output_dir, "scatter_CVP_vs_CVi.png"),
  plot     = scatter_CVP_CVi,
  width    = 10,
  height   = 9
)

# ---- Scatter 2: rIIV vs CVi (ellipses = 95 % credible intervals) ----
combined_summary <- combined_summary %>%
  dplyr::mutate(
    a_cvi_riiv = (CVi_upper  - CVi_lower)  / 2,
    b_cvi_riiv = (rIIV_upper - rIIV_lower) / 2
  )

scatter_rIIV_CVi <- ggplot(combined_summary,
                           aes(x0 = CVi_mean, y0 = rIIV_mean,
                               a = a_cvi_riiv, b = b_cvi_riiv, angle = 0,
                               fill = variable, colour = variable)) +
  ggforce::geom_ellipse(alpha = 0.7, linewidth = 0.4) +
  geom_point(aes(x = CVi_mean, y = rIIV_mean, colour = variable,
                 shape = family),
             size = 2.5, show.legend = TRUE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = CVi_mean, y = rIIV_mean, label = label, fill = variable),
    colour             = "black",
    fontface           = "bold",
    size               = 4,
    alpha              = 0.5,
    label.padding      = unit(0.15, "lines"),
    box.padding        = unit(0.5, "lines"),
    point.padding      = unit(0.3, "lines"),
    force              = 50,
    force_pull         = 0.3,
    max.iter           = 20000,
    direction          = "both",
    min.segment.length = 0,
    segment.size       = 0.3,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    inherit.aes        = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_fill_manual(values   = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  scale_shape_manual(values = c("gaussian" = 16, "lognormal" = 17)) +
  labs(
    x     = "CVi (Repeatability \u2014 between-individual variation)",
    y     = "rIIV (Predictability \u2014 residual intra-individual variance)",
    shape = "Likelihood family"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    axis.title      = element_text(size = 18),
    axis.text       = element_text(size = 14)
  ) +
  guides(fill = "none", colour = "none")

ggsave(
  filename = file.path(output_dir, "scatter_rIIV_vs_CVi.png"),
  plot     = scatter_rIIV_CVi,
  width    = 10,
  height   = 9
)
