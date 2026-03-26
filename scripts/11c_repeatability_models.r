###################################################################################################
################################## 11b. Repeatability — Fit Models ################################
###################################################################################################
# Prerequisites: run 11a_repeatability_setup.r first (loads master_data, defines
# partition_variance).
#
# PARALLELISED: runs 2 models concurrently via future.apply, each brm() using
# 4 cores for its MCMC chains (2 × 4 = 8 cores total).
#
# Each variable gets its own specification so that formula, iterations, family,
# and control settings can be tuned independently.  Results are stored as
# individual .rds files under results/11_repeatability/.
#
# After all models are fitted the warnings summary is printed and the combined
# models list is assembled for downstream use by 11d.
#
# FAMILY CHOICES (based on 11b distribution analysis + diagnostics):
#   Gaussian                    – roughly symmetric, no floor/ceiling issues
#   lognormal()                 – strictly positive, right-skewed (skew > ~1)
#   hurdle_lognormal()          – positive + substantial zero-inflation
#   negbinomial()               – overdispersed counts (water_visits, total_meals, bin counts)
#   Beta()                      – proportions in (0,1)  (feeding %, feed remaining %)
#   zero_one_inflated_beta()    – proportions in [0,1] with point mass at 0 (actor %, reactor %)

library(brms)
library(future)
library(future.apply)

output_dir <- "results/11_repeatability"
load("results/11_repeatability/master_data.rda")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Model Specifications ###########################################
###################################################################################################
model_specs <- list(
  # skew=0.57, kurtosis=5.8, 0% zeros — mildly skewed, diagnostics OK → Gaussian
  list(name = "feed_intake",
       formula_var = "feed_intake",
       family = "gaussian", iter = 6000),

  # skew=0.09, near-normal → Gaussian
  list(name = "feed_duration",
       formula_var = "feed_duration",
       family = "gaussian", iter = 6000),

  # skew=1.25, 0% zeros, low Bulk ESS with Gaussian → lognormal
  list(name = "feed_visits",
       formula_var = "feed_visits",
       family = "lognormal", iter = 12000),

  # skew=0.18, near-normal but low Bulk ESS — keep Gaussian, increase iterations
  list(name = "water_intake",
       formula_var = "water_intake",
       family = "gaussian", iter = 10000),

  # skew=1.65, 0% zeros, low Bulk ESS + pareto_k with Gaussian → lognormal
  list(name = "water_duration",
       formula_var = "water_duration",
       family = "lognormal", iter = 15000),

  # Counts → negative binomial
  list(name = "water_visits",
       formula_var = "water_visits",
       family = "negbinomial", iter = 10000),

  # Counts → negative binomial
  list(name = "total_meals",
       formula_var = "total_meals",
       family = "negbinomial", iter = 6000),

  # skew=1.15, 0% zeros, right-skewed, pp_check shows Gaussian predicts negatives → lognormal
  list(name = "median_meal_duration",
       formula_var = "median_meal_duration",
       family = "lognormal", iter = 6000),

  # skew=1.50, 0% zeros, pp_check shows Gaussian predicts negatives → lognormal
  list(name = "median_visit_per_meal",
       formula_var = "median_visit_per_meal",
       family = "lognormal", iter = 6000),

  # skew=1.10, 0% zeros, strictly positive → lognormal
  list(name = "median_intake_per_meal",
       formula_var = "median_intake_per_meal",
       family = "lognormal", iter = 6000),

  # skew=0.86, diagnostics OK → negbinomial
  list(name = "median_unique_bins_per_meal",
       formula_var = "median_unique_bins_per_meal",
       family = "negbinomial", iter = 6000),

  # beta (rescaled to 0-1 in 11a)
  list(name = "median_feeding_pct_per_meal",
       formula_var = "median_feeding_pct_per_meal_prop",
       family = "Beta", iter = 6000),

  # Counts → negative binomial
  list(name = "unique_feed_bins_visited",
       formula_var = "unique_feed_bins_visited",
       family = "negbinomial", iter = 6000),

  # Counts → negative binomial
  list(name = "unique_water_bins_visited",
       formula_var = "unique_water_bins_visited",
       family = "negbinomial", iter = 6000),

  # Counts → negative binomial
  list(name = "total_bins_visited",
       formula_var = "total_bins_visited",
       family = "negbinomial", iter = 6000),

  # skew=1.87, 0% zeros, low Bulk ESS with Gaussian, pp_check shows clear misfit → lognormal
  list(name = "number_of_non_nutritive_visits",
       formula_var = "number_of_non_nutritive_visits",
       family = "lognormal", iter = 15000),

  # Percentage → Beta (rescaled to 0-1 in 11a)
  list(name = "median_pct_feed_remaining",
       formula_var = "median_pct_feed_remaining_prop",
       family = "Beta", iter = 6000),

  # skew=2.13, 0.56% zeros, high Rhat + low Bulk ESS with Gaussian → hurdle_lognormal
  list(name = "median_non_nutritive_per_meal",
       formula_var = "median_non_nutritive_per_meal",
       family = "hurdle_lognormal", iter = 10000),

  # Actor % — 25.8% zeros, skew=0.67 → zero_one_inflated_beta (rescaled to 0-1 in 11a)
  list(name = "median_pct_actor",
       formula_var = "median_pct_actor_prop",
       family = "zero_one_inflated_beta", iter = 10000),

  # Reactor % — 18.3% zeros, skew=0.79 → zero_one_inflated_beta (rescaled to 0-1 in 11a)
  list(name = "median_pct_reactor",
       formula_var = "median_pct_reactor_prop",
       family = "zero_one_inflated_beta", iter = 10000)
)

###################################################################################################
################################## Model-fitting function #########################################
###################################################################################################
fit_one_model <- function(spec, master_data, output_dir) {
  library(brms)

  rds_path <- file.path(output_dir, paste0("m1_brm_", spec$name, ".rds"))

  message("Fitting ", spec$name, " ...")

  f <- as.formula(paste(spec$formula_var,
                        "~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow)"))

  fam <- switch(spec$family,
    gaussian               = gaussian(),
    lognormal              = lognormal(),
    hurdle_lognormal       = hurdle_lognormal(),
    negbinomial            = negbinomial(),
    Beta                   = Beta(),
    zero_one_inflated_beta = zero_one_inflated_beta()
  )

  warns <- list()
  model <- withCallingHandlers(
    brm(
      formula = f,
      data    = master_data,
      family  = fam,
      warmup  = 1000,
      iter    = spec$iter,
      chains  = 4,
      init    = "random",
      cores   = 4,
      seed    = 12345,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    ),
    warning = function(w) {
      warns[[length(warns) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  model <- add_criterion(model, "loo")
  saveRDS(model, rds_path)
  message("Done: ", spec$name)

  list(model = model, warnings = warns)
}

###################################################################################################
################################## Run models in parallel (2 at a time × 4 cores each) ############
###################################################################################################
plan(multisession, workers = 2)

results <- future_lapply(
  model_specs,
  fit_one_model,
  master_data = master_data,
  output_dir  = output_dir,
  future.seed = TRUE,
  future.packages = "brms"
)

plan(sequential)

names(results) <- vapply(model_specs, \(s) s$name, character(1))

###################################################################################################
################################## Collect warnings ###############################################
###################################################################################################
all_warnings <- lapply(results, \(r) r$warnings)

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
  "feed_intake", "feed_duration", "feed_visits",
  "water_intake", "water_duration", "water_visits",
  "total_meals", "median_meal_duration", "median_visit_per_meal",
  "median_intake_per_meal", "median_unique_bins_per_meal", "median_feeding_pct_per_meal",
  "unique_feed_bins_visited", "unique_water_bins_visited", "total_bins_visited",
  "number_of_non_nutritive_visits", "median_pct_feed_remaining",
  "median_non_nutritive_per_meal", "median_pct_actor", "median_pct_reactor"
)

models <- lapply(results, \(r) r$model)
