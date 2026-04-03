###################################################################################################
######################### 11c. Repeatability — Feed Rate (HPC) ###################################
###################################################################################################
# Prerequisites: run 11a_repeatability_setup.r first
#
# Single-model script for median_feed_rate, intended for HPC submission.
# Uses 4 cores for MCMC chains.
#
# FAMILY: lognormal – strictly positive, right-skewed

.libPaths("/scratch/st-nina-1/skysheng/moo4feed_decoding_animal/my_r_lib")
library(brms)

output_dir <- "results/11_repeatability"
load("results/11_repeatability/master_data.rda")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

rds_path <- file.path(output_dir, "m1_brm_median_feed_rate.rds")

###################################################################################################
################################## Fit model ######################################################
###################################################################################################
message("Fitting median_feed_rate ...")

warns <- list()
model <- withCallingHandlers(
  brm(
    formula = median_feed_rate ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 15000,
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
message("Done: median_feed_rate — saved to ", rds_path)

###################################################################################################
################################## Warnings summary ###############################################
###################################################################################################
warns <- warns[!grepl("was built under R version", warns)]

cat("\n\n========== WARNINGS SUMMARY ==========\n")
if (length(warns) > 0) {
  for (msg in warns) cat("  WARNING:", msg, "\n")
} else {
  cat("No warnings (excluding package version notices).\n")
}
cat("======================================\n\n")
