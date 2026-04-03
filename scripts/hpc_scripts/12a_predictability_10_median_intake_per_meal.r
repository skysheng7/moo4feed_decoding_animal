###################################################################################################
############### 12a-10. Predictability — median_intake_per_meal (interactive session) ##############
###################################################################################################
# DHGLM with sigma ~ (1 | cow) to estimate within-individual variance.
# Run inside an interactive Sockeye session (4 cores).
#
# Prerequisites: results/11_repeatability/master_data.rda

.libPaths("/scratch/st-nina-1/skysheng/moo4feed_decoding_animal/my_r_lib")
library(brms)

load("results/11_repeatability/master_data.rda")

output_dir <- "results/12_predictability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

rds_path <- file.path(output_dir, "m2_brm_median_intake_per_meal.rds")

cat("Fitting median_intake_per_meal ...\n")

f <- bf(
  median_intake_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
  sigma ~ (1 | cow)
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
        family  = lognormal(),
        warmup  = 1000,
        iter    = 6000,
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
  cat("SUCCESS: median_intake_per_meal saved to", rds_path, "\n")
} else {
  cat("FAILED: median_intake_per_meal --", err, "\n")
  quit(save = "no", status = 1)
}

if (length(warns) > 0) {
  warns <- warns[!grepl("was built under R version", warns)]
  if (length(warns) > 0) {
    cat("\nWarnings for median_intake_per_meal:\n")
    for (msg in warns) cat("  WARNING:", msg, "\n")
  }
}
