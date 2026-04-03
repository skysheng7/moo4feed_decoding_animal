###################################################################################################
################################## 11c. Repeatability — Results & Diagnostics ####################
###################################################################################################
# Prerequisites: run 11a then 11b (or load .rds files from results/11_repeatability/).
# This script assumes `master_data`, `partition_variance`, `models`, and `response_vars`
# are already in the environment (either from running 11a + 11b, or by loading them below).

library(brms)
library(coda)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(bayesplot)
library(moo4feed)

output_dir <- "results/11_repeatability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- If starting fresh (not continuing from 11b), load master_data + helpers ------------------
if (!exists("master_data")) {
  load("results/11_repeatability/master_data.rda")
  source("scripts/11a_repeatability_setup.r")   # re-defines partition_variance
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

# ---- Load models from .rds if not already in memory -------------------------------------------
if (!exists("models")) {
  models <- list()
  for (rv in response_vars) {
    rds_path <- file.path(output_dir, paste0("m1_brm_", rv, ".rds"))
    cat("Loading:", rv, "\n")
    models[[rv]] <- readRDS(rds_path)
  }
}



###################################################################################################
################################## Variance partitioning helper ###################################
###################################################################################################
# Repeatability and CVi for Bayesian GLMMs with (1 | cow) as the sole
# random effect.  Variance is partitioned into:
#   var.cow  = between-individual (random intercept) variance
#   var.res  = residual (within-individual) variance = sigma^2
# Repeatability R = var.cow / (var.cow + var.res)
#
# CVi (coefficient of individual variation) — both on the original data scale
# so that gaussian and lognormal models are directly comparable:
#   gaussian  (identity link): CVi = sigma_cow / mu_bar
#   lognormal (log link):      CVi = sqrt(exp(var.cow) - 1)
#
# References
# ----------
# Nakagawa, S., Johnson, P. C. D. & Schielzeth, H. (2017). The coefficient
#   of determination R^2 and intra-class correlation coefficient from
#   generalized linear mixed-effects models revisited and expanded.
#   J. R. Soc. Interface 14: 20170213.
#   https://doi.org/10.1098/rsif.2017.0213
#
# Nakagawa, S. & Schielzeth, H. (2010). Repeatability for Gaussian and
#   non-Gaussian data: a practical guide for biologists.  Biological Reviews
#   85: 935-956.
#   https://doi.org/10.1111/j.1469-185X.2010.00141.x
partition_variance <- function(m1_brm, response_var, data, n_sim = 10000) {
  ps  <- as_draws_df(m1_brm)
  fam <- family(m1_brm)$family

  var.cow <- ps$"sd_cow__Intercept"^2

  if (fam %in% c("gaussian", "lognormal")) {
    var.res   <- ps$"sigma"^2
    var.total <- var.cow + var.res
  } else {
    stop("Unsupported family: ", fam)
  }

  R_cow <- var.cow / var.total
  R_res <- var.res / var.total

  if (fam == "gaussian") {
    CVi <- sqrt(var.cow) / mean(data[[response_var]])
  } else if (fam == "lognormal") {
    CVi <- sqrt(exp(var.cow) - 1)
  }

  cat("\n===", response_var, "(family:", fam, ") ===\n")
  cat("Repeatability (R_cow):   ", round(mean(R_cow), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(R_cow), 0.95), 4), "\n")
  cat("R_residual:              ", round(mean(R_res), 4), "\n")
  cat("CVi:                     ", round(mean(CVi), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVi), 0.95), 4), "\n")

  list(
    response = response_var,
    family   = fam,
    R_cow    = R_cow,
    R_res    = R_res,
    CVi      = CVi
  )
}


###################################################################################################
################################## Variance partitioning ##########################################
###################################################################################################
partitions <- mapply(
  partition_variance,
  m1_brm       = models,
  response_var = response_vars,
  MoreArgs     = list(data = master_data),
  SIMPLIFY     = FALSE
)
names(partitions) <- response_vars

###################################################################################################
################################## Summary table #################################################
###################################################################################################
results_table <- do.call(rbind, lapply(partitions, function(p) {
  data.frame(
    variable    = p$response,
    family      = p$family,
    R_cow_mean  = round(mean(p$R_cow), 4),
    R_cow_lower = round(HPDinterval(as.mcmc(p$R_cow), 0.95)[1], 4),
    R_cow_upper = round(HPDinterval(as.mcmc(p$R_cow), 0.95)[2], 4),
    R_residual  = round(mean(p$R_res), 4),
    CVi_mean    = round(mean(p$CVi), 4),
    CVi_lower   = round(HPDinterval(as.mcmc(p$CVi), 0.95)[1], 4),
    CVi_upper   = round(HPDinterval(as.mcmc(p$CVi), 0.95)[2], 4)
  )
}))

print(results_table)
write.csv(results_table,
          file.path(output_dir, "repeatability_summary.csv"),
          row.names = FALSE)

###################################################################################################
################################## Posterior BT plots ############################################
###################################################################################################
plot_posterior_bt <- function(m1_brm, response_var, data,
                              out_dir = "results/11_repeatability") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  fam <- family(m1_brm)$family
  ps  <- as_draws_df(m1_brm)

  # Select only the mu-part cow random effects (exclude hu, zoi, coi parts)
  cow_cols <- grep("^r_cow\\[", names(ps), value = TRUE)
  cow_cols <- cow_cols[!grepl("__(hu|zoi|coi)", cow_cols)]

  posteriorBT <- ps[, cow_cols] %>%
    as.data.frame() %>%
    gather(cow, value) %>%
    separate(cow,
             c(NA, NA, "cow", NA),
             sep = "([\\_\\[\\,])", fill = "right")

  posteriorBT$value <- posteriorBT$value + fixef(m1_brm, pars = "Intercept")[1]

  if (fam == "lognormal") {
    posteriorBT$value <- exp(posteriorBT$value)
  }

  # Undo the +1 offset applied in 11a for zero-capable lognormal variables
  offset_vars <- c("median_non_nutritive_per_meal", "total_actor", "total_reactor")
  if (response_var %in% offset_vars) {
    posteriorBT$value <- posteriorBT$value - 1
  }

  posteriorBT <- posteriorBT %>%
    dplyr::group_by(cow) %>%
    dplyr::mutate(meanBT = mean(value)) %>%
    dplyr::ungroup()

  focal_cows   <- c("5042", "5120", "6022", "7169", "3067")
  focal_colors <- c("#C77CFF", "#F8766D", "#7CAE00", "#FFCC00", "#00BFC4")
  names(focal_colors) <- focal_cows

  posteriorBT$col <- ifelse(
    posteriorBT$cow %in% focal_cows,
    posteriorBT$cow,
    "Other individuals"
  )

  fill_values <- c(focal_colors, "Other individuals" = "gray")
  n_cows <- n_distinct(posteriorBT$cow)

  x_lab <- response_var

  BT <- ggplot() +
    ggridges::geom_density_ridges(data = posteriorBT,
                                  aes(x      = value,
                                      y      = reorder(as.factor(cow), meanBT),
                                      height = after_stat(density),
                                      fill   = col,
                                      scale  = 3),
                                  alpha = 0.6) +
    geom_point(data = posteriorBT[!duplicated(posteriorBT$cow), ],
               aes(x = meanBT, y = as.factor(cow), col = col),
               size = 1) +
    labs(y = "", x = x_lab, fill = "ID", col = "ID") +
    theme_classic() +
    scale_fill_manual(values  = fill_values) +
    scale_color_manual(values = fill_values)

  ggsave(
    filename = file.path(out_dir, paste0("BT_plot_", response_var, ".png")),
    plot     = BT,
    width    = 8,
    height   = max(4, n_cows * 0.35)
  )

  return(BT)
}

bt_plots <- list()
for (rv in response_vars) {
  bt_plots[[rv]] <- plot_posterior_bt(models[[rv]], rv, master_data)
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
  rds_path <- file.path(output_dir, paste0("m1_brm_", rv, ".rds"))
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
  all_params <- rbind(fe, re, spec)

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

  partition_variance(m, rv, master_data)

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
