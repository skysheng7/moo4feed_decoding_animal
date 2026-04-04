###################################################################################################
################################## 12a. Predictability — Fit Models ##############################
###################################################################################################
# Predictability is quantified as within-individual variance (IIV) using a double hierarchical
# generalised linear model (DHGLM) in brms.  The sigma sub-model includes (1 | cow) so that each
# individual's intra-individual variability is estimated.
#
# Prerequisites: results/11_repeatability/master_data.rda must exist (created by 11a).
#
# PARALLELISED: runs 2 models concurrently via future.apply, each brm() using
# 4 cores for its MCMC chains (2 × 4 = 8 cores total).
#
# Each variable gets its own specification so that formula, iterations, family,
# and control settings can be tuned independently.  Results are stored as
# individual .rds files under results/12_predictability/.
#
# Models that already have an .rds file on disk are skipped automatically,
# so you can safely re-run the script if it was interrupted.
#
# FAMILY CHOICES (matching 11c repeatability models):
#   Gaussian    – roughly symmetric, no floor/ceiling issues
#   lognormal() – strictly positive, right-skewed (skew > ~1)

library(brms)
library(future)
library(future.apply)

load("results/11_repeatability/master_data.rda")

output_dir <- "results/12_predictability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Model Specifications ###########################################
###################################################################################################
model_specs <- list(
  list(name = "feed_intake",
       formula_var = "feed_intake",
       family = "gaussian", iter = 10000),

  list(name = "feed_duration",
       formula_var = "feed_duration",
       family = "gaussian", iter = 12000),

  list(name = "feed_visits",
       formula_var = "feed_visits",
       family = "lognormal", iter = 12000),

  list(name = "water_intake",
       formula_var = "water_intake",
       family = "gaussian", iter = 10000),

  list(name = "water_duration",
       formula_var = "water_duration",
       family = "lognormal", iter = 15000),

  list(name = "water_visits",
       formula_var = "water_visits",
       family = "lognormal", iter = 10000),

  list(name = "total_meals",
       formula_var = "total_meals",
       family = "lognormal", iter = 10000),

  list(name = "median_meal_duration",
       formula_var = "median_meal_duration",
       family = "lognormal", iter = 6000),

  list(name = "median_visit_per_meal",
       formula_var = "median_visit_per_meal",
       family = "lognormal", iter = 12000),

  list(name = "median_intake_per_meal",
       formula_var = "median_intake_per_meal",
       family = "lognormal", iter = 6000),

  list(name = "median_feeding_pct_per_meal",
       formula_var = "median_feeding_pct_per_meal",
       family = "gaussian", iter = 12000),

  list(name = "number_of_non_nutritive_visits",
       formula_var = "number_of_non_nutritive_visits",
       family = "lognormal", iter = 15000),

  list(name = "median_pct_feed_remaining",
       formula_var = "median_pct_feed_remaining",
       family = "gaussian", iter = 6000),

  list(name = "median_non_nutritive_per_meal",
       formula_var = "median_non_nutritive_per_meal",
       family = "lognormal", iter = 15000),

  list(name = "total_actor",
       formula_var = "total_actor",
       family = "lognormal", iter = 6000),

  list(name = "total_reactor",
       formula_var = "total_reactor",
       family = "lognormal", iter = 6000),

  list(name = "median_feed_rate",
       formula_var = "median_feed_rate",
       family = "gaussian", iter = 17000),

  list(name = "median_water_rate",
       formula_var = "median_water_rate",
       family = "gaussian", iter = 17000)
)

###################################################################################################
################################## Model-fitting function #########################################
###################################################################################################
fit_one_model <- function(spec, master_data, output_dir) {
  library(brms)

  rds_path <- file.path(output_dir, paste0("m2_brm_", spec$name, ".rds"))

  if (file.exists(rds_path)) {
    message("Skipping ", spec$name, " (already exists)")
    return(list(model = readRDS(rds_path), warnings = list(), error = NULL))
  }

  message("Fitting ", spec$name, " ...")

  f <- bf(
    as.formula(paste(spec$formula_var,
                     "~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow)")),
    sigma ~ (1 | cow)
  )

  fam <- switch(spec$family,
    gaussian  = gaussian(),
    lognormal = lognormal()
  )

  warns <- list()
  model <- NULL
  err   <- NULL

  tryCatch(
    {
      model <- withCallingHandlers(
        brm(
          formula = f,
          data    = master_data,
          family  = fam,
          warmup  = 1000,
          iter    = spec$iter,
          thin    = 1,
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
    },
    error = function(e) {
      err <<- conditionMessage(e)
    }
  )

  if (!is.null(model)) {
    saveRDS(model, rds_path)
    message("Done: ", spec$name)
  } else {
    message("FAILED: ", spec$name, " -- ", err)
  }

  list(model = model, warnings = warns, error = err)
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
################################## Collect warnings & errors ######################################
###################################################################################################
cat("\n\n========== WARNINGS SUMMARY ==========\n")
any_warnings <- FALSE
for (rv in names(results)) {
  w <- results[[rv]]$warnings
  w <- w[!grepl("was built under R version", w)]
  if (length(w) > 0) {
    any_warnings <- TRUE
    cat("\n---", rv, "---\n")
    for (msg in w) cat("  WARNING:", msg, "\n")
  }
}
if (!any_warnings) cat("No warnings (excluding package version notices).\n")
cat("======================================\n\n")

cat("\n========== ERRORS SUMMARY ==========\n")
any_errors <- FALSE
for (rv in names(results)) {
  e <- results[[rv]]$error
  if (!is.null(e)) {
    any_errors <- TRUE
    cat("\n---", rv, "---\n")
    cat("  ERROR:", e, "\n")
  }
}
if (!any_errors) cat("No errors.\n")
cat("====================================\n\n")

###################################################################################################
################################## Assemble models list for downstream use ########################
###################################################################################################
response_vars <- c(
  "feed_intake", "feed_duration", "feed_visits",
  "water_intake", "water_duration", "water_visits",
  "total_meals", "median_meal_duration", "median_visit_per_meal",
  "median_intake_per_meal", "median_feeding_pct_per_meal",
  "number_of_non_nutritive_visits", "median_pct_feed_remaining",
  "median_non_nutritive_per_meal", "total_actor", "total_reactor",
  "median_feed_rate", "median_water_rate"
)

models <- lapply(results, \(r) r$model)
