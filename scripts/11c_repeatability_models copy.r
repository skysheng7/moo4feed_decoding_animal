###################################################################################################
################################## 11b. Repeatability — Fit Models (Parallel) #####################
###################################################################################################
# Prerequisites: run 11a_repeatability_setup.r first (loads master_data, defines
# partition_variance).
#
# PARALLEL STRATEGY:
#   Models are fit concurrently via future_lapply(). Each brm() uses cores = 1
#   (chains run sequentially within a model) while multiple models run
#   simultaneously across separate R sessions. Adjust n_parallel below to
#   match your machine's available cores.
#
# FAMILY CHOICES (based on 11b distribution analysis + diagnostics):
#   Gaussian           – roughly symmetric, no floor/ceiling issues
#   lognormal()        – strictly positive, right-skewed (skew > ~1)
#   hurdle_lognormal() – positive + substantial zero-inflation

library(brms)
library(future)
library(future.apply)

output_dir <- "results/11_repeatability"
load("results/11_repeatability/master_data.rda")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

n_parallel <- 2
plan(multisession, workers = n_parallel)
cat("Fitting models in parallel with", n_parallel, "workers\n")

###################################################################################################
################################## Model specifications ###########################################
###################################################################################################
# Named by RDS file stem. Per-model family and iter can be tuned independently;
# all share: warmup = 1000, chains = 4, seed = 12345, adapt_delta = 0.99,
# max_treedepth = 15.

model_specs <- list(
  # skew=1.65, 0% zeros, low Bulk ESS + pareto_k with Gaussian — lognormal
  m1_brm_water_duration = list(
    var = "water_duration", family = lognormal(), iter = 15000
  ),
  # skew=1.87, 0% zeros, low Bulk ESS with Gaussian, pp_check clear misfit — lognormal
  m1_brm_number_of_non_nutritive_visits = list(
    var = "number_of_non_nutritive_visits", family = lognormal(), iter = 15000
  ),
  # skew=-0.61, bounded 0-100%, diagnostics OK except 1 pareto_k — Gaussian
  m1_brm_median_feeding_pct_per_meal = list(
    var = "median_feeding_pct_per_meal", family = gaussian(), iter = 6000
  ),
  # diagnostics OK — Gaussian
  m1_brm_water_visits = list(
    var = "water_visits", family = gaussian(), iter = 10000
  ),
  # skew=0.52, diagnostics OK — Gaussian
  m1_brm_total_meals = list(
    var = "total_meals", family = gaussian(), iter = 6000
  ),
  # skew=1.50, 0% zeros, pp_check shows Gaussian predicts negatives — Gaussian
  m1_brm_median_visit_per_meal = list(
    var = "median_visit_per_meal", family = gaussian(), iter = 6000
  ),
  # skew=-0.48, diagnostics OK — Gaussian
  m1_brm_unique_feed_bins_visited = list(
    var = "unique_feed_bins_visited", family = gaussian(), iter = 6000
  ),
  # skew=-0.36, diagnostics OK — Gaussian
  m1_brm_unique_water_bins_visited = list(
    var = "unique_water_bins_visited", family = gaussian(), iter = 6000
  ),
  # skew=-0.49, diagnostics OK — Gaussian
  m1_brm_total_bins_visited = list(
    var = "total_bins_visited", family = gaussian(), iter = 6000
  ),
  # skew=-0.36, diagnostics OK — Gaussian
  m1_brm_median_pct_feed_remaining = list(
    var = "median_pct_feed_remaining", family = gaussian(), iter = 6000
  )
)

###################################################################################################
################################## Fit helper #####################################################
###################################################################################################
fit_brm_model <- function(spec, data, output_dir, model_name) {
  warnings_collected <- list()
  fit <- withCallingHandlers(
    brm(
      formula = as.formula(paste(spec$var,
        "~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow)")),
      data    = data,
      family  = spec$family,
      warmup  = 1000,
      iter    = spec$iter,
      chains  = 4,
      init    = "random",
      cores   = 4,
      seed    = 12345,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    ),
    warning = function(w) {
      warnings_collected[[length(warnings_collected) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  fit <- add_criterion(fit, "loo")
  saveRDS(fit, file.path(output_dir, paste0(model_name, ".rds")))
  list(model = fit, warnings = warnings_collected)
}

###################################################################################################
################################## Run all models in parallel #####################################
###################################################################################################
cat("Starting parallel model fitting at", format(Sys.time()), "\n")

results <- future_lapply(names(model_specs), function(nm) {
  fit_brm_model(model_specs[[nm]], master_data, output_dir, nm)
}, future.seed = TRUE, future.packages = c("brms"))
names(results) <- names(model_specs)

plan(sequential)
cat("All models finished at", format(Sys.time()), "\n")

###################################################################################################
################################## Collect warnings ###############################################
###################################################################################################
all_warnings <- lapply(results, function(r) r$warnings)

cat("\n\n========== WARNINGS SUMMARY ==========\n")
any_warnings <- FALSE
for (rv in names(all_warnings)) {
  w <- all_warnings[[rv]]
  w <- w[!grepl("was built under R version", w)]
  if (length(w) > 0) {
    any_warnings <- TRUE
    cat("\n---", rv, "---\n")
    for (msg in w) cat("  WARNING:", msg, "\n")
  }
}
if (!any_warnings) cat("No warnings (excluding package version notices).\n")
cat("======================================\n\n")

###################################################################################################
################################## Assemble models list for downstream use ########################
###################################################################################################
response_vars <- c(
  "water_duration", "number_of_non_nutritive_visits",
  "median_feeding_pct_per_meal", "water_visits", "total_meals",
  "median_visit_per_meal", "unique_feed_bins_visited",
  "unique_water_bins_visited", "total_bins_visited",
  "median_pct_feed_remaining"
)

models <- setNames(
  lapply(paste0("m1_brm_", response_vars), function(nm) results[[nm]]$model),
  response_vars
)
