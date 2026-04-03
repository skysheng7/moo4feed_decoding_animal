###################################################################################################
################################## 12a. Predictability — Fit Models (HPC) ########################
###################################################################################################
# Predictability is quantified as within-individual variance (IIV) using a double hierarchical
# generalised linear model (DHGLM) in brms.  The sigma sub-model includes (1 | cow) so that each
# individual's intra-individual variability is estimated.
#
# Prerequisites: results/11_repeatability/master_data.rda must exist (created by 11a).
#
# USAGE (Slurm array job):
#   Rscript scripts/12a_predictability_models_hpc.r <MODEL_INDEX>
#   where MODEL_INDEX is 1..18 (passed automatically by Slurm via $SLURM_ARRAY_TASK_ID).
#
# Each model uses 4 cores for its 4 MCMC chains.  When submitted as a Slurm array
# (--array=1-18, --cpus-per-task=4), all 18 models run concurrently across the cluster.
#
# Models that already have an .rds file on disk are skipped automatically.
#
# FAMILY CHOICES (matching 11c repeatability models):
#   Gaussian    – roughly symmetric, no floor/ceiling issues
#   lognormal() – strictly positive, right-skewed (skew > ~1)
.libPaths("./my_r_lib")
library(brms)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/12a_predictability_models_hpc.r <MODEL_INDEX>")
model_index <- as.integer(args[1])

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
       family = "lognormal", iter = 12000),

  list(name = "median_water_rate",
       formula_var = "median_water_rate",
       family = "lognormal", iter = 12000)
)

if (model_index < 1 || model_index > length(model_specs)) {
  stop("MODEL_INDEX must be between 1 and ", length(model_specs),
       " (got ", model_index, ")")
}

spec <- model_specs[[model_index]]
cat("=== Task", model_index, "of", length(model_specs), ":", spec$name, "===\n")

###################################################################################################
################################## Fit the selected model #########################################
###################################################################################################
rds_path <- file.path(output_dir, paste0("m2_brm_", spec$name, ".rds"))

cat("Fitting", spec$name, "...\n")

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

###################################################################################################
################################## Save & report ##################################################
###################################################################################################
if (!is.null(model)) {
  saveRDS(model, rds_path)
  cat("SUCCESS:", spec$name, "saved to", rds_path, "\n")
} else {
  cat("FAILED:", spec$name, "--", err, "\n")
  quit(save = "no", status = 1)
}

if (length(warns) > 0) {
  warns <- warns[!grepl("was built under R version", warns)]
  if (length(warns) > 0) {
    cat("\nWarnings for", spec$name, ":\n")
    for (msg in warns) cat("  WARNING:", msg, "\n")
  }
}
