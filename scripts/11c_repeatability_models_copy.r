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
#   Gaussian    – roughly symmetric, no floor/ceiling issues
#   lognormal() – strictly positive, right-skewed (skew > ~1)

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
  list(name = "feed_duration",
       formula_var = "feed_duration",
       family = "gaussian", iter = 12000),
  list(name = "median_visit_per_meal",
       formula_var = "median_visit_per_meal",
       family = "lognormal", iter = 12000),
  list(name = "median_feeding_pct_per_meal",
       formula_var = "median_feeding_pct_per_meal",
       family = "gaussian", iter = 12000),
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
    gaussian  = gaussian(),
    lognormal = lognormal()
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
  "median_intake_per_meal", "median_feeding_pct_per_meal",
  "number_of_non_nutritive_visits", "median_pct_feed_remaining",
  "median_non_nutritive_per_meal", "total_actor", "total_reactor"
)

models <- lapply(results, \(r) r$model)
