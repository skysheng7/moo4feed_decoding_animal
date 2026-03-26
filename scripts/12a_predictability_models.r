###################################################################################################
################################## 12a. Predictability — Fit Models ##############################
###################################################################################################
# Predictability is quantified as within-individual variance (IIV) using a double hierarchical
# generalised linear model (DHGLM) in brms.  The sigma sub-model includes (1 | cow) so that each
# individual's intra-individual variability is estimated.
#
# Prerequisites: results/11_repeatability/master_data.rda must exist (created by 11a).
#
# Each variable gets its own explicit brm() block so formula, iterations, priors,
# and control settings can be tuned independently.  Models are saved as .rds files
# under results/12_predictability/.
#
# FAMILY CHOICES (matching 11c repeatability models):
#   Gaussian           – roughly symmetric, no floor/ceiling issues
#   lognormal()        – strictly positive, right-skewed (skew > ~1)
#   hurdle_lognormal() – positive + substantial zero-inflation

library(brms)
library(coda)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggrepel)
library(parallel)

load("results/11_repeatability/master_data.rda")

output_dir <- "results/12_predictability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## feed_intake ####################################################
###################################################################################################
# Gaussian — pp_check OK but pareto_k(14), increase iter
warnings_feed_intake <- list()
m2_brm_feed_intake <- withCallingHandlers(
  brm(
    formula = bf(
      feed_intake ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_feed_intake[[length(warnings_feed_intake) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_feed_intake, file.path(output_dir, "m2_brm_feed_intake.rds"))

###################################################################################################
################################## feed_duration ##################################################
###################################################################################################
# Gaussian — low_Bulk_ESS + pareto_k(5), increase iter
warnings_feed_duration <- list()
m2_brm_feed_duration <- withCallingHandlers(
  brm(
    formula = bf(
      feed_duration ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_feed_duration[[length(warnings_feed_duration) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_feed_duration, file.path(output_dir, "m2_brm_feed_duration.rds"))

###################################################################################################
################################## feed_visits ####################################################
###################################################################################################
# skew=1.25, 0% zeros — switch to lognormal (matches 11c)
warnings_feed_visits <- list()
m2_brm_feed_visits <- withCallingHandlers(
  brm(
    formula = bf(
      feed_visits ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_feed_visits[[length(warnings_feed_visits) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_feed_visits, file.path(output_dir, "m2_brm_feed_visits.rds"))

###################################################################################################
################################## water_intake ###################################################
###################################################################################################
# Gaussian — pareto_k(3), increase iter
warnings_water_intake <- list()
m2_brm_water_intake <- withCallingHandlers(
  brm(
    formula = bf(
      water_intake ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_water_intake[[length(warnings_water_intake) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_water_intake, file.path(output_dir, "m2_brm_water_intake.rds"))

###################################################################################################
################################## water_duration #################################################
###################################################################################################
# skew=1.65, 0% zeros, low Bulk ESS + pareto_k(8) — switch to lognormal (matches 11c)
warnings_water_duration <- list()
m2_brm_water_duration <- withCallingHandlers(
  brm(
    formula = bf(
      water_duration ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_water_duration[[length(warnings_water_duration) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_water_duration, file.path(output_dir, "m2_brm_water_duration.rds"))

###################################################################################################
################################## water_visits ###################################################
###################################################################################################
# Gaussian (per 11c) — increase iter for pareto_k(6)
warnings_water_visits <- list()
m2_brm_water_visits <- withCallingHandlers(
  brm(
    formula = bf(
      water_visits ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_water_visits[[length(warnings_water_visits) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_water_visits, file.path(output_dir, "m2_brm_water_visits.rds"))

###################################################################################################
################################## total_meals ####################################################
###################################################################################################
# Gaussian — diagnostics OK
warnings_total_meals <- list()
m2_brm_total_meals <- withCallingHandlers(
  brm(
    formula = bf(
      total_meals ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_total_meals[[length(warnings_total_meals) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_total_meals, file.path(output_dir, "m2_brm_total_meals.rds"))

###################################################################################################
################################## median_meal_duration ###########################################
###################################################################################################
# Gaussian (per 11c) — pareto_k(3) but diagnostics otherwise OK
warnings_median_meal_duration <- list()
m2_brm_median_meal_duration <- withCallingHandlers(
  brm(
    formula = bf(
      median_meal_duration ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_median_meal_duration[[length(warnings_median_meal_duration) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_meal_duration, file.path(output_dir, "m2_brm_median_meal_duration.rds"))

###################################################################################################
################################## median_visit_per_meal ##########################################
###################################################################################################
# Gaussian (per 11c) — pareto_k(5) but diagnostics otherwise OK
warnings_median_visit_per_meal <- list()
m2_brm_median_visit_per_meal <- withCallingHandlers(
  brm(
    formula = bf(
      median_visit_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_median_visit_per_meal[[length(warnings_median_visit_per_meal) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_visit_per_meal, file.path(output_dir, "m2_brm_median_visit_per_meal.rds"))

###################################################################################################
################################## median_intake_per_meal #########################################
###################################################################################################
# Gaussian (per 11c) — pareto_k(3) but diagnostics otherwise OK
warnings_median_intake_per_meal <- list()
m2_brm_median_intake_per_meal <- withCallingHandlers(
  brm(
    formula = bf(
      median_intake_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_median_intake_per_meal[[length(warnings_median_intake_per_meal) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_intake_per_meal, file.path(output_dir, "m2_brm_median_intake_per_meal.rds"))

###################################################################################################
################################## median_unique_bins_per_meal ####################################
###################################################################################################
# Gaussian — pareto_k(2) but diagnostics otherwise OK
warnings_median_unique_bins_per_meal <- list()
m2_brm_median_unique_bins_per_meal <- withCallingHandlers(
  brm(
    formula = bf(
      median_unique_bins_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_median_unique_bins_per_meal[[length(warnings_median_unique_bins_per_meal) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_unique_bins_per_meal, file.path(output_dir, "m2_brm_median_unique_bins_per_meal.rds"))

###################################################################################################
################################## median_feeding_pct_per_meal ####################################
###################################################################################################
# Gaussian — low_Bulk_ESS + pareto_k(5), increase iter
warnings_median_feeding_pct_per_meal <- list()
m2_brm_median_feeding_pct_per_meal <- withCallingHandlers(
  brm(
    formula = bf(
      median_feeding_pct_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_median_feeding_pct_per_meal[[length(warnings_median_feeding_pct_per_meal) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_feeding_pct_per_meal, file.path(output_dir, "m2_brm_median_feeding_pct_per_meal.rds"))

###################################################################################################
################################## unique_feed_bins_visited #######################################
###################################################################################################
# Gaussian — diagnostics OK
warnings_unique_feed_bins_visited <- list()
m2_brm_unique_feed_bins_visited <- withCallingHandlers(
  brm(
    formula = bf(
      unique_feed_bins_visited ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_unique_feed_bins_visited[[length(warnings_unique_feed_bins_visited) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_unique_feed_bins_visited, file.path(output_dir, "m2_brm_unique_feed_bins_visited.rds"))

###################################################################################################
################################## unique_water_bins_visited ######################################
###################################################################################################
# Gaussian — diagnostics OK
warnings_unique_water_bins_visited <- list()
m2_brm_unique_water_bins_visited <- withCallingHandlers(
  brm(
    formula = bf(
      unique_water_bins_visited ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_unique_water_bins_visited[[length(warnings_unique_water_bins_visited) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_unique_water_bins_visited, file.path(output_dir, "m2_brm_unique_water_bins_visited.rds"))

###################################################################################################
################################## total_bins_visited #############################################
###################################################################################################
# Gaussian — diagnostics OK
warnings_total_bins_visited <- list()
m2_brm_total_bins_visited <- withCallingHandlers(
  brm(
    formula = bf(
      total_bins_visited ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_total_bins_visited[[length(warnings_total_bins_visited) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_total_bins_visited, file.path(output_dir, "m2_brm_total_bins_visited.rds"))

###################################################################################################
################################## number_of_non_nutritive_visits #################################
###################################################################################################
# skew=1.87, 0% zeros, low Bulk ESS + pareto_k(7) — switch to lognormal (matches 11c)
warnings_number_of_non_nutritive_visits <- list()
m2_brm_number_of_non_nutritive_visits <- withCallingHandlers(
  brm(
    formula = bf(
      number_of_non_nutritive_visits ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_number_of_non_nutritive_visits[[length(warnings_number_of_non_nutritive_visits) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_number_of_non_nutritive_visits, file.path(output_dir, "m2_brm_number_of_non_nutritive_visits.rds"))

###################################################################################################
################################## median_pct_feed_remaining ######################################
###################################################################################################
# Gaussian — diagnostics OK
warnings_median_pct_feed_remaining <- list()
m2_brm_median_pct_feed_remaining <- withCallingHandlers(
  brm(
    formula = bf(
      median_pct_feed_remaining ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
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
    warnings_median_pct_feed_remaining[[length(warnings_median_pct_feed_remaining) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_pct_feed_remaining, file.path(output_dir, "m2_brm_median_pct_feed_remaining.rds"))

###################################################################################################
################################## median_non_nutritive_per_meal ##################################
###################################################################################################
# skew=2.13, 0.56% zeros, low Bulk ESS + pareto_k(7) — hurdle_lognormal (matches 11c)
# sigma sub-model captures individual differences in day-to-day variability
# hu sub-model captures individual differences in probability of zero
warnings_median_non_nutritive_per_meal <- list()
m2_brm_median_non_nutritive_per_meal <- withCallingHandlers(
  brm(
    formula = bf(
      median_non_nutritive_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow),
      hu ~ (1 | cow)
    ),
    data    = master_data,
    family  = hurdle_lognormal(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_median_non_nutritive_per_meal[[length(warnings_median_non_nutritive_per_meal) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_non_nutritive_per_meal, file.path(output_dir, "m2_brm_median_non_nutritive_per_meal.rds"))

###################################################################################################
################################## median_pct_actor ###############################################
###################################################################################################
# Gaussian (per 11c) — pareto_k(6), increase iter
warnings_median_pct_actor <- list()
m2_brm_median_pct_actor <- withCallingHandlers(
  brm(
    formula = bf(
      median_pct_actor ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_median_pct_actor[[length(warnings_median_pct_actor) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_pct_actor, file.path(output_dir, "m2_brm_median_pct_actor.rds"))

###################################################################################################
################################## median_pct_reactor #############################################
###################################################################################################
# Gaussian (per 11c) — pareto_k(4), increase iter
warnings_median_pct_reactor <- list()
m2_brm_median_pct_reactor <- withCallingHandlers(
  brm(
    formula = bf(
      median_pct_reactor ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
      sigma ~ (1 | cow)
    ),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
    thin    = 1,
    chains  = 4,
    init    = "random",
    cores   = 4,
    seed    = 12345,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  ),
  warning = function(w) {
    warnings_median_pct_reactor[[length(warnings_median_pct_reactor) + 1]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
saveRDS(m2_brm_median_pct_reactor, file.path(output_dir, "m2_brm_median_pct_reactor.rds"))

###################################################################################################
################################## Collect warnings ##############################################
###################################################################################################
all_warnings <- list(
  feed_intake                    = warnings_feed_intake,
  feed_duration                  = warnings_feed_duration,
  feed_visits                    = warnings_feed_visits,
  water_intake                   = warnings_water_intake,
  water_duration                 = warnings_water_duration,
  water_visits                   = warnings_water_visits,
  total_meals                    = warnings_total_meals,
  median_meal_duration           = warnings_median_meal_duration,
  median_visit_per_meal          = warnings_median_visit_per_meal,
  median_intake_per_meal         = warnings_median_intake_per_meal,
  median_unique_bins_per_meal    = warnings_median_unique_bins_per_meal,
  median_feeding_pct_per_meal    = warnings_median_feeding_pct_per_meal,
  unique_feed_bins_visited       = warnings_unique_feed_bins_visited,
  unique_water_bins_visited      = warnings_unique_water_bins_visited,
  total_bins_visited             = warnings_total_bins_visited,
  number_of_non_nutritive_visits = warnings_number_of_non_nutritive_visits,
  median_pct_feed_remaining      = warnings_median_pct_feed_remaining,
  median_non_nutritive_per_meal  = warnings_median_non_nutritive_per_meal,
  median_pct_actor               = warnings_median_pct_actor,
  median_pct_reactor             = warnings_median_pct_reactor
)

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

models <- list(
  feed_intake                    = m2_brm_feed_intake,
  feed_duration                  = m2_brm_feed_duration,
  feed_visits                    = m2_brm_feed_visits,
  water_intake                   = m2_brm_water_intake,
  water_duration                 = m2_brm_water_duration,
  water_visits                   = m2_brm_water_visits,
  total_meals                    = m2_brm_total_meals,
  median_meal_duration           = m2_brm_median_meal_duration,
  median_visit_per_meal          = m2_brm_median_visit_per_meal,
  median_intake_per_meal         = m2_brm_median_intake_per_meal,
  median_unique_bins_per_meal    = m2_brm_median_unique_bins_per_meal,
  median_feeding_pct_per_meal    = m2_brm_median_feeding_pct_per_meal,
  unique_feed_bins_visited       = m2_brm_unique_feed_bins_visited,
  unique_water_bins_visited      = m2_brm_unique_water_bins_visited,
  total_bins_visited             = m2_brm_total_bins_visited,
  number_of_non_nutritive_visits = m2_brm_number_of_non_nutritive_visits,
  median_pct_feed_remaining      = m2_brm_median_pct_feed_remaining,
  median_non_nutritive_per_meal  = m2_brm_median_non_nutritive_per_meal,
  median_pct_actor               = m2_brm_median_pct_actor,
  median_pct_reactor             = m2_brm_median_pct_reactor
)
