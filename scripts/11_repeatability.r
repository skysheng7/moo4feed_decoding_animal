###################################################################################################
################################## 11. Repeatability Analysis ####################################
###################################################################################################
# External packages
library(brms)        # for brm(), add_criterion(), fixef()
library(coda)        # for as.mcmc(), HPDinterval()
library(tidyverse)   # for %>%, gather(), separate(), left_join(), group_by(), mutate(), ungroup()
library(ggplot2)     # for ggplot(), geom_point(), labs(), theme_classic(), scale_fill_manual()
library(parallel)    # for detectCores()
library(lubridate)   # for ymd()
library(bayesplot)   # for pp_check(), mcmc_plot(), nuts_params()
library(moo4feed)    # for read_data_safely()

###################################################################################################
################################## Load and prepare data ##########################################
###################################################################################################
# Load the filtered cow-date combinations (only clean days from selected stable groups)
load("results/9_filter_problematic_days/all_info_final.rda")

# Read daily summary variables
summary_df <- moo4feed::read_data_safely("results/1_data_cleaning/summary_df.csv",
                                         header = TRUE, sep = ",")

# Ensure date types match for joining
summary_df$date    <- ymd(summary_df$date, tz = "America/Los_Angeles")
all_info_final$date <- ymd(all_info_final$date, tz = "America/Los_Angeles")
all_info_final$cow  <- as.integer(all_info_final$cow)
summary_df$cow      <- as.integer(summary_df$cow)

# Filter summary_df to only the cow-date combinations present in all_info_final
data <- summary_df %>%
  semi_join(all_info_final, by = c("cow", "date"))

# Merge in covariates needed for the model (DIM, parity, THI_mean)
data <- data %>%
  left_join(
    all_info_final %>%
      select(cow, date, days_in_milk, Parity, milk_production, Elo, THI_mean) %>%
      rename(DIM = days_in_milk, parity = Parity),
    by = c("cow", "date")
  )

# Derive month as numeric for seasonal fixed effect
data$month <- lubridate::month(data$date)

# Confirm no missing covariate rows
stopifnot(all(!is.na(data$DIM)))

###################################################################################################
################################## Build master daily summary dataframe ##########################
###################################################################################################

# ---- meal_summaries.csv: median meal-level stats per cow per day --------------------------------
meal_summaries <- moo4feed::read_data_safely("results/2_meal_clustering/meal_summaries.csv",
                                             header = TRUE, sep = ",")
meal_summaries$date <- ymd(meal_summaries$date, tz = "America/Los_Angeles")
meal_summaries$cow  <- as.integer(meal_summaries$cow)

meal_daily <- meal_summaries %>%
  group_by(cow, date) %>%
  summarise(
    total_meals                 = n(),
    median_meal_duration        = median(meal_duration,       na.rm = TRUE),
    median_visit_per_meal       = median(visit_count,         na.rm = TRUE),
    median_intake_per_meal      = median(total_intake,        na.rm = TRUE),
    median_unique_bins_per_meal = median(unique_bins_count,   na.rm = TRUE),
    median_feeding_pct_per_meal = median(feeding_percentage,  na.rm = TRUE),
    .groups = "drop"
  )

# ---- bin_visits.csv: unique bins visited per cow per day ----------------------------------------
bin_visits <- moo4feed::read_data_safely("results/3_bin_visit_analysis/bin_visits.csv",
                                         header = TRUE, sep = ",")
bin_visits$date <- ymd(bin_visits$date, tz = "America/Los_Angeles")
bin_visits$cow  <- as.integer(bin_visits$cow)

# ---- non_nutritive.rda: total non-nutritive visits per cow per day ------------------------------
load("results/6_non_nutritive_visit_analysis/non_nutritive.rda")
non_nutritive_daily <- imap(non_nutritive, ~ mutate(.x, date = .y)) %>%
  bind_rows() %>%
  mutate(
    date = lubridate::ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  )

# ---- availability.rda: median feed availability per cow per day ---------------------------------
load("results/7_feed_availability_analysis/availability.rda")
avail_daily <- do.call(rbind, availability$daily_summary) %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(cow, date, median_pct_feed_remaining)

# ---- all_meal_visits.rda: median non-nutritive & empty-bin visits per meal ----------------------
load("results/8_meal_level_behavior_analysis/all_meal_visits.rda")
all_meal_visits <- all_meal_visits %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(-total_meals) %>%
  select(cow, date, median_non_nutritive_per_meal)

# ---- all_daily_roles.rda: median actor/reactor percentages per cow per day ----------------------
load("results/8_meal_level_behavior_analysis/all_daily_roles.rda")
all_daily_roles <- all_daily_roles %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(-total_meals) %>%
  select(cow, date,
         median_pct_actor,
         median_pct_reactor,
         median_pct_actor_reactor)

# ---- Assemble master dataframe ------------------------------------------------------------------
master_data <- data %>%
  left_join(meal_daily,          by = c("cow", "date")) %>%
  left_join(bin_visits,          by = c("cow", "date")) %>%
  left_join(non_nutritive_daily, by = c("cow", "date")) %>%
  left_join(avail_daily,         by = c("cow", "date")) %>%
  left_join(all_meal_visits,     by = c("cow", "date")) %>%
  left_join(all_daily_roles,     by = c("cow", "date"))

save(master_data, file = "results/11_repeatability/master_data.rda")

###################################################################################################
################################## Variance partitioning helper ###################################
###################################################################################################
# Model has only (1 | cow) as random effect, so variance is partitioned into:
#   var.cow  = between-individual variance
#   var.res  = residual (within-individual) variance
# Repeatability R = var.cow / (var.cow + var.res)
partition_variance <- function(m1_brm, response_var, data) {
  ps <- as_draws_df(m1_brm)

  var.cow   <- ps$"sd_cow__Intercept"^2
  var.res   <- ps$"sigma"^2
  var.total <- var.cow + var.res

  R_cow <- var.cow / var.total
  R_res <- var.res / var.total

  CVi <- sqrt(var.cow) / mean(data[[response_var]], na.rm = TRUE)

  cat("\n===", response_var, "===\n")
  cat("Repeatability (R_cow):   ", round(mean(R_cow), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(R_cow), 0.95), 4), "\n")
  cat("R_residual:              ", round(mean(R_res), 4), "\n")
  cat("CVi:                     ", round(mean(CVi), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVi), 0.95), 4), "\n")

  list(
    response = response_var,
    R_cow    = R_cow,
    R_res    = R_res,
    CVi      = CVi
  )
}

###################################################################################################
################################## Run repeatability for all master_data variables ################
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
  "median_pct_reactor",
  "median_pct_actor_reactor"
)

# ---- Configuration: set which variable(s) to run ------------------------------------------------
# To debug one variable at a time, set run_vars to a single variable, e.g.:
#   run_vars <- "feed_intake"
# To run all variables, set:
#   run_vars <- response_vars
run_vars <- response_vars

# Parallel setup: each brm gets 2 cores for its chains, remaining cores run models in parallel
total_cores <- parallel::detectCores()
brm_cores   <- 4L
n_workers   <- max(1L, floor(total_cores / brm_cores))

cat(sprintf(
  "\nLaunching %d parallel workers; each brm model will use %d core(s).\n",
  n_workers, brm_cores
))

# Variables that need more iterations to resolve Bulk ESS warnings
high_iter_vars <- c("feed_visits", "water_duration", "water_visits")

cl <- parallel::makeCluster(n_workers)
parallel::clusterExport(cl, varlist = c("master_data", "brm_cores", "high_iter_vars"), envir = environment())
parallel::clusterEvalQ(cl, {
  library(brms)
  library(coda)
  library(parallel)
})

run_repeatability_par <- function(response_var) {
  output_dir <- "results/11_repeatability"
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  rds_path <- file.path(output_dir, paste0("m1_brm_", response_var, ".rds"))

  formula_str <- paste0(
    response_var,
    " ~ DIM + parity + THI_mean + month + I(month^2) + (1 | cow)"
  )

  # Use more iterations for variables with ESS issues
  n_iter <- if (response_var %in% high_iter_vars) 10000L else 6000L

  # Capture all warnings during model fitting
  warnings_list <- list()
  m1_brm <- withCallingHandlers(
    brm(
      formula = as.formula(formula_str),
      data    = master_data,
      warmup  = 1000,
      iter    = n_iter,
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

  loo_warnings <- list()
  m1_brm <- withCallingHandlers(
    add_criterion(m1_brm, "loo"),
    warning = function(w) {
      loo_warnings[[length(loo_warnings) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  saveRDS(m1_brm, rds_path)

  list(
    model    = m1_brm,
    warnings = c(warnings_list, loo_warnings)
  )
}

results_list <- parallel::parLapply(cl, run_vars, run_repeatability_par)
parallel::stopCluster(cl)
names(results_list) <- run_vars

# Print all captured warnings per variable
cat("\n\n========== WARNINGS SUMMARY ==========\n")
any_warnings <- FALSE
for (rv in run_vars) {
  w <- results_list[[rv]]$warnings
  # Filter out package version warnings (not actionable)
  w <- w[!grepl("was built under R version", w)]
  if (length(w) > 0) {
    any_warnings <- TRUE
    cat("\n---", rv, "---\n")
    for (msg in w) cat("  WARNING:", msg, "\n")
  }
}
if (!any_warnings) cat("No warnings (excluding package version notices).\n")
cat("======================================\n\n")

# Extract models
models <- lapply(results_list, `[[`, "model")

partitions <- mapply(
  partition_variance,
  m1_brm       = models,
  response_var = run_vars,
  MoreArgs     = list(data = master_data),
  SIMPLIFY     = FALSE
)
names(partitions) <- run_vars

###################################################################################################
################################## Summary table #################################################
###################################################################################################
results_table <- do.call(rbind, lapply(partitions, function(p) {
  data.frame(
    variable    = p$response,
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
          "results/11_repeatability/repeatability_summary.csv",
          row.names = FALSE)

###################################################################################################
################################## Posterior BT plot #############################################
###################################################################################################
plot_posterior_bt <- function(m1_brm, response_var, data,
                              output_dir = "results/11_repeatability") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Extract individual-level posterior intercepts using as_draws_df()
  ps <- as_draws_df(m1_brm)
  cow_cols <- grep("^r_cow\\[", names(ps), value = TRUE)

  posteriorBT <- ps[, cow_cols] %>%
    as.data.frame() %>%
    gather(cow, value) %>%
    separate(cow,
             c(NA, NA, "cow", NA),
             sep = "([\\_\\[\\,])", fill = "right")

  # Adjust intercepts to the response scale using the population-level intercept
  posteriorBT$value <- posteriorBT$value + fixef(m1_brm, pars = "Intercept")[1]

  # Compute posterior means; highlight focal cows
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
    labs(y = "", x = response_var, fill = "ID", col = "ID") +
    theme_classic() +
    scale_fill_manual(values  = fill_values) +
    scale_color_manual(values = fill_values)

  ggsave(
    filename = file.path(output_dir, paste0("BT_plot_", response_var, ".png")),
    plot     = BT,
    width    = 8,
    height   = max(4, n_cows * 0.35)
  )

  return(BT)
}

bt_plots <- list()
for (rv in run_vars) {
  bt_plots[[rv]] <- plot_posterior_bt(models[[rv]], rv, master_data)
}

###################################################################################################
############################### Model diagnostics #################################################
###################################################################################################
output_dir   <- "results/11_repeatability"
diag_dir     <- file.path(output_dir, "diagnostics")
dir.create(diag_dir, showWarnings = TRUE, recursive = TRUE)

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
  print(rv)
  rds_path <- file.path(output_dir, paste0("m1_brm_", rv, ".rds"))
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

  # Collect Rhat and ESS from both fixed and random effects
  fe <- s$fixed
  re <- do.call(rbind, s$random)
  spec <- s$spec_pars
  all_params <- rbind(fe, re, spec)

  max_rhat     <- max(all_params[, "Rhat"],     na.rm = TRUE)
  min_bulk_ess <- min(all_params[, "Bulk_ESS"], na.rm = TRUE)
  min_tail_ess <- min(all_params[, "Tail_ESS"], na.rm = TRUE)

  # Expected sample size = (iter - warmup) / thin * chains
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
  np         <- nuts_params(m)
  n_div      <- sum(np$Value[np$Parameter == "divergent__"])
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
  loo_m     <- loo(m)
  n_high_k  <- sum(loo_m$diagnostics$pareto_k > 0.7)
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

  # ---------- 6. Variance partitioning ----------------------------------------
  partition_variance(m, rv, master_data)

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

