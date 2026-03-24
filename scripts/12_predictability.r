###################################################################################################
################################## 12. Predictability Analysis ###################################
###################################################################################################
# Predictability is quantified as within-individual variance (IIV) using a double hierarchical
# generalised linear model (DHGLM) in brms.  A sub-model for the residual SD (sigma) includes a
# random intercept per cow, so that each individual's intra-individual variability can be
# estimated.  A low IIV means the individual is highly predictable (consistent day-to-day).
#
# External packages
library(brms)        # for brm(), fixef(), as_draws_df()
library(coda)        # for as.mcmc(), HPDinterval()
library(tidyverse)   # for %>%, gather(), separate(), left_join(), group_by(), mutate(), ungroup()
library(ggplot2)     # for ggplot(), geom_point(), labs(), theme_classic(), scale_fill_manual()
library(ggridges)    # for geom_density_ridges()
library(ggrepel)     # for geom_label_repel()
library(parallel)    # for detectCores()

###################################################################################################
################################## Load master data ###############################################
###################################################################################################
load("results/11_repeatability/master_data.rda")

###################################################################################################
################################## Helper: extract IIV summaries ##################################
###################################################################################################
# rIIV  = variance of the sigma random effect (on the log scale)
# CVP   = coefficient of variation of predictability = sqrt(exp(sd^2) - 1)
extract_iiv <- function(m2_brm, response_var) {
  ps <- as_draws_df(m2_brm)

  sd_sigma <- ps$"sd_cow__sigma_Intercept"

  var_res  <- exp(sd_sigma)^2
  log_norm <- exp(sd_sigma^2)
  CVP      <- sqrt(log_norm - 1)

  cat("\n===", response_var, "===\n")
  cat("rIIV (mean):  ", round(mean(var_res), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(var_res), 0.95), 4), "\n")
  cat("CVP  (mean):  ", round(mean(CVP), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVP), 0.95), 4), "\n")

  list(
    response = response_var,
    var_res  = var_res,
    CVP      = CVP
  )
}

###################################################################################################
################################## Response variables #############################################
###################################################################################################
response_vars <- c(
  # Basic feeding / watering behaviour
  "feed_intake",
  "feed_duration",
  "feed_visits",
  "water_intake",
  "water_duration",
  "water_visits",

  # Meal-level summaries
  "total_meals",
  "median_meal_duration",
  "median_visit_per_meal",
  "median_intake_per_meal",
  "median_unique_bins_per_meal",
  "median_feeding_pct_per_meal",

  # Bin-visit summaries
  "unique_feed_bins_visited",
  "unique_water_bins_visited",
  "total_bins_visited",

  # Non-nutritive / availability / social behaviour
  "number_of_non_nutritive_visits",
  "median_pct_feed_remaining",
  "median_non_nutritive_per_meal",
  "median_pct_actor",
  "median_pct_reactor"
)

###################################################################################################
################################## Run DHGLM models ###############################################
###################################################################################################
# ---- Configuration: set which variable(s) to run ------------------------------------------------
# To debug one variable at a time, set run_vars to a single variable, e.g.:
#   run_vars <- "feed_intake"
# To run all variables, set:
#   run_vars <- response_vars
run_vars <- response_vars

output_dir <- "results/12_predictability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Variables that need more iterations to resolve Bulk ESS warnings
high_iter_vars <- c("feed_visits", "water_duration", "water_visits")

# Identify which variables need refitting vs loading from existing .rds
refit_vars <- character(0)
load_vars  <- setdiff(run_vars, refit_vars)

# ---- Load existing models from .rds --------------------------------------------------------------
cat("\nLoading existing models from .rds files...\n")
models <- list()
for (rv in load_vars) {
  rds_path <- file.path(output_dir, paste0("m2_brm_", rv, ".rds"))
  cat("  Loading:", rv, "\n")
  models[[rv]] <- readRDS(rds_path)
}
cat(sprintf("Loaded %d models.\n", length(models)))

# ---- Refit models that need re-running -----------------------------------------------------------
brm_cores <- 4L

run_predictability <- function(response_var) {
  rds_path <- file.path(output_dir, paste0("m2_brm_", response_var, ".rds"))

  # Mean sub-model mirrors the repeatability formula (script 11)
  # Sigma sub-model adds (1 | cow) to let each cow have its own residual SD
  double_model <- brms::bf(
    as.formula(paste0(
      response_var,
      " ~ DIM + parity + THI_mean + month + I(month^2) + (1 | cow)"
    )),
    sigma ~ (1 | cow)
  )

  # Use more iterations for variables with ESS issues
  n_iter <- if (response_var %in% high_iter_vars) 10000L else 6000L

  # Capture all warnings during model fitting
  warnings_list <- list()
  m2_brm <- withCallingHandlers(
    brms::brm(
      formula = double_model,
      data    = master_data,
      warmup  = 1000,
      iter    = n_iter,
      thin    = 1,
      chains  = 4,
      init    = "random",
      cores   = brm_cores,
      seed    = 12345,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    ),
    warning = function(w) {
      warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  saveRDS(m2_brm, rds_path)

  list(model = m2_brm, warnings = warnings_list)
}

cat("\nRefitting models for:", paste(refit_vars, collapse = ", "), "\n")
for (rv in refit_vars) {
  result <- run_predictability(rv)
  models[[rv]] <- result$model

  w <- result$warnings
  w <- w[!grepl("was built under R version", w)]
  if (length(w) > 0) {
    cat("\n--- Warnings for", rv, "---\n")
    for (msg in w) cat("  WARNING:", msg, "\n")
  } else {
    cat("  No warnings for", rv, "(excluding package version notices).\n")
  }
}

# Ensure models are in the same order as run_vars
models <- models[run_vars]

###################################################################################################
################################## Extract IIV for all variables ##################################
###################################################################################################
iiv_results <- mapply(
  extract_iiv,
  m2_brm       = models,
  response_var = run_vars,
  SIMPLIFY     = FALSE
)
names(iiv_results) <- run_vars

###################################################################################################
################################## Summary table #################################################
###################################################################################################
results_table <- do.call(rbind, lapply(iiv_results, function(p) {
  data.frame(
    variable       = p$response,
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
          "results/12_predictability/predictability_summary.csv",
          row.names = FALSE)

###################################################################################################
################################## Posterior IIV ridge plot #######################################
###################################################################################################
plot_posterior_iiv <- function(m2_brm, response_var,
                               output_dir = "results/12_predictability") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ps       <- as_draws_df(m2_brm)
  iiv_cols <- grep("^r_cow__sigma\\[", names(ps), value = TRUE)

  # Pivot to long format; column names look like r_cow__sigma[5042,Intercept]
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

  # Shift by the population-level sigma intercept to get absolute IIV on the log scale
  posteriorIIV$value <- posteriorIIV$value +
    fixef(m2_brm, pars = "sigma_Intercept")[1]

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

  # Build a summary for the mean-dot layer (one row per cow, ordered by meanIIV)
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
    labs(y = "", x = paste0("rIIV \u2014 ", response_var), fill = "ID", col = "ID") +
    theme_classic() +
    scale_fill_manual(values  = fill_values) +
    scale_color_manual(values = fill_values)

  ggsave(
    filename = file.path(output_dir, paste0("IIV_plot_", response_var, ".png")),
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

  # ---------- 1. Summary: Rhat + ESS -----------------------------------------
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

  # ---------- 2. Divergent transitions ----------------------------------------
  np    <- nuts_params(m)
  n_div <- sum(np$Value[np$Parameter == "divergent__"])
  cat("  Divergent transitions: ", n_div, "\n")

  # ---------- 3. Posterior predictive checks ----------------------------------
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

  # ---------- 4. LOO diagnostics (Pareto k) -----------------------------------
  loo_m    <- loo(m)
  n_high_k <- sum(loo_m$diagnostics$pareto_k > 0.7)
  cat("  LOO Pareto k > 0.7: ", n_high_k, "obs\n")
  print(loo_m)

  # ---------- 5. Trace plots (only when issues detected) ----------------------
  has_issues <- (max_rhat > 1.01) || (min_bulk_ess < 1000) ||
                (min_tail_ess < 1000) || (n_div > 0)

  if (has_issues) {
    cat("  >> Issues detected — saving trace plots\n")
    trace_plt <- mcmc_plot(m, type = "trace") +
      ggplot2::ggtitle(paste("Trace:", rv))
    ggsave(file.path(diag_dir, paste0("trace_", rv, ".png")),
           trace_plt, width = 10, height = min(49, max(4, 1.5 * length(variables(m)))))
  }

  # ---------- 6. IIV extraction -----------------------------------------------
  extract_iiv(m, rv)

  # ---------- Collect into summary table --------------------------------------
  flags <- character()
  if (max_rhat > 1.01)     flags <- c(flags, "high_Rhat")
  if (min_bulk_ess < 1000)  flags <- c(flags, "low_Bulk_ESS")
  if (min_tail_ess < 1000)  flags <- c(flags, "low_Tail_ESS")
  if (n_div > 0)            flags <- c(flags, "divergences")
  if (n_high_k > 0)         flags <- c(flags, paste0("pareto_k(", n_high_k, ")"))

  diag_summary <- rbind(diag_summary, data.frame(
    variable           = rv,
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

# ---------- Print and save overall diagnostics summary ------------------------
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
################################## Repeatability vs Predictability ################################
###################################################################################################
# Combine CVi (repeatability) and CVP (predictability) summaries and plot them against each other.
# High CVi  = high between-individual variation (animals differ a lot on average)
# High CVP  = high within-individual variation  (animals are unpredictable day-to-day)

rep_summary  <- moo4feed::read_data_safely("results/11_repeatability/repeatability_summary.csv",
                                           header = TRUE, sep = ",")
pred_summary <- moo4feed::read_data_safely("results/12_predictability/predictability_summary.csv",
                                           header = TRUE, sep = ",")

combined_summary <- rep_summary  %>%
  dplyr::select(variable, CVi_mean) %>%
  dplyr::inner_join(
    pred_summary %>% dplyr::select(variable, CVP_mean, rIIV_mean),
    by = "variable"
  ) %>%
  dplyr::filter(TRUE)

# ---- shared label layer helper -----------------------------------------------
repred_label_layer <- function() {
  ggrepel::geom_label_repel(
    aes(fill = variable),
    colour          = "black",
    fontface        = "bold",
    size            = 3,
    alpha           = 0.7,
    label.padding   = unit(0.2, "lines"),
    box.padding     = unit(0.4, "lines"),
    point.padding   = unit(0.3, "lines"),
    direction       = "both",
    max.overlaps    = Inf,
    show.legend     = FALSE
  )
}

# ---- Plot 1: CVP_mean (y) vs CVi_mean (x) ------------------------------------
scatter_CVP_CVi <- ggplot(combined_summary,
                          aes(x = CVi_mean, y = CVP_mean,
                              colour = variable, label = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  repred_label_layer() +
  labs(
    x      = "CVi mean (Repeatability \u2014 between-individual variation)",
    y      = "CVP mean (Predictability \u2014 within-individual variation)",
    colour = "Variable"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(
  filename = "results/12_predictability/scatter_CVP_vs_CVi.png",
  plot     = scatter_CVP_CVi,
  width    = 10,
  height   = 8
)

# ---- Plot 2: rIIV_mean (y) vs CVi_mean (x) -----------------------------------
scatter_rIIV_CVi <- ggplot(combined_summary,
                           aes(x = CVi_mean, y = rIIV_mean,
                               colour = variable, label = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  repred_label_layer() +
  labs(
    x      = "CVi mean (Repeatability \u2014 between-individual variation)",
    y      = "rIIV mean (Predictability \u2014 residual intra-individual variance)",
    colour = "Variable"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(
  filename = "results/12_predictability/scatter_rIIV_vs_CVi.png",
  plot     = scatter_rIIV_CVi,
  width    = 10,
  height   = 8
)
