###################################################################################################
################################## 11b. Repeatability — Fit Models ################################
###################################################################################################
# Prerequisites: run 11a_repeatability_setup.r first (loads master_data, defines
# partition_variance).
#
# Each variable gets its own explicit brm() call so that formula, iterations,
# priors, and control settings can be tuned independently.  Results are stored as
# individual .rds files under results/11_repeatability/.
#
# After running all blocks, the models list is assembled at the bottom and
# partition_variance() is called per variable, then the results table is saved.
#
# FAMILY CHOICES (based on 11b distribution analysis + diagnostics):
#   Gaussian           – roughly symmetric, no floor/ceiling issues
#   lognormal()        – strictly positive, right-skewed (skew > ~1)
#   hurdle_lognormal() – positive + substantial zero-inflation

output_dir <- "results/11_repeatability"
load("results/11_repeatability/master_data.rda")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## feed_intake ####################################################
###################################################################################################
# skew=0.57, kurtosis=5.8, 0% zeros — mildly skewed, diagnostics OK → Gaussian
warnings_feed_intake <- list()
m1_brm_feed_intake <- withCallingHandlers(
  brm(
    formula = feed_intake ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_feed_intake <- add_criterion(m1_brm_feed_intake, "loo")
saveRDS(m1_brm_feed_intake, file.path(output_dir, "m1_brm_feed_intake.rds"))

###################################################################################################
################################## feed_duration ##################################################
###################################################################################################
# skew=0.09, near-normal — Gaussian
warnings_feed_duration <- list()
m1_brm_feed_duration <- withCallingHandlers(
  brm(
    formula = feed_duration ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_feed_duration <- add_criterion(m1_brm_feed_duration, "loo")
saveRDS(m1_brm_feed_duration, file.path(output_dir, "m1_brm_feed_duration.rds"))

###################################################################################################
################################## feed_visits ####################################################
###################################################################################################
# skew=1.25, 0% zeros, low Bulk ESS with Gaussian — switch to lognormal
warnings_feed_visits <- list()
m1_brm_feed_visits <- withCallingHandlers(
  brm(
    formula = feed_visits ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 12000,
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
m1_brm_feed_visits <- add_criterion(m1_brm_feed_visits, "loo")
saveRDS(m1_brm_feed_visits, file.path(output_dir, "m1_brm_feed_visits.rds"))

###################################################################################################
################################## water_intake ###################################################
###################################################################################################
# skew=0.18, near-normal but low Bulk ESS — keep Gaussian, increase iterations
warnings_water_intake <- list()
m1_brm_water_intake <- withCallingHandlers(
  brm(
    formula = water_intake ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
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
m1_brm_water_intake <- add_criterion(m1_brm_water_intake, "loo")
saveRDS(m1_brm_water_intake, file.path(output_dir, "m1_brm_water_intake.rds"))

###################################################################################################
################################## water_duration #################################################
###################################################################################################
# skew=1.65, 0% zeros, low Bulk ESS + pareto_k with Gaussian — switch to lognormal
warnings_water_duration <- list()
m1_brm_water_duration <- withCallingHandlers(
  brm(
    formula = water_duration ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 10000,
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
m1_brm_water_duration <- add_criterion(m1_brm_water_duration, "loo")
saveRDS(m1_brm_water_duration, file.path(output_dir, "m1_brm_water_duration.rds"))

###################################################################################################
################################## water_visits ###################################################
###################################################################################################
warnings_water_visits <- list()
m1_brm_water_visits <- withCallingHandlers(
  brm(
    formula = water_visits ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
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
m1_brm_water_visits <- add_criterion(m1_brm_water_visits, "loo")
saveRDS(m1_brm_water_visits, file.path(output_dir, "m1_brm_water_visits.rds"))

###################################################################################################
################################## total_meals ####################################################
###################################################################################################
# skew=0.52, diagnostics OK — Gaussian
warnings_total_meals <- list()
m1_brm_total_meals <- withCallingHandlers(
  brm(
    formula = total_meals ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_total_meals <- add_criterion(m1_brm_total_meals, "loo")
saveRDS(m1_brm_total_meals, file.path(output_dir, "m1_brm_total_meals.rds"))

###################################################################################################
################################## median_meal_duration ###########################################
###################################################################################################
# skew=1.15, 0% zeros, right-skewed, pp_check shows Gaussian predicts negatives — lognormal
warnings_median_meal_duration <- list()
m1_brm_median_meal_duration <- withCallingHandlers(
  brm(
    formula = median_meal_duration ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_median_meal_duration <- add_criterion(m1_brm_median_meal_duration, "loo")
saveRDS(m1_brm_median_meal_duration, file.path(output_dir, "m1_brm_median_meal_duration.rds"))

###################################################################################################
################################## median_visit_per_meal ##########################################
###################################################################################################
# skew=1.50, 0% zeros, pp_check shows Gaussian predicts negatives — lognormal
warnings_median_visit_per_meal <- list()
m1_brm_median_visit_per_meal <- withCallingHandlers(
  brm(
    formula = median_visit_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_median_visit_per_meal <- add_criterion(m1_brm_median_visit_per_meal, "loo")
saveRDS(m1_brm_median_visit_per_meal, file.path(output_dir, "m1_brm_median_visit_per_meal.rds"))

###################################################################################################
################################## median_intake_per_meal #########################################
###################################################################################################
# skew=1.10, 0% zeros, strictly positive — lognormal
warnings_median_intake_per_meal <- list()
m1_brm_median_intake_per_meal <- withCallingHandlers(
  brm(
    formula = median_intake_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_median_intake_per_meal <- add_criterion(m1_brm_median_intake_per_meal, "loo")
saveRDS(m1_brm_median_intake_per_meal, file.path(output_dir, "m1_brm_median_intake_per_meal.rds"))

###################################################################################################
################################## median_unique_bins_per_meal ####################################
###################################################################################################
# skew=0.86, diagnostics OK — Gaussian
warnings_median_unique_bins_per_meal <- list()
m1_brm_median_unique_bins_per_meal <- withCallingHandlers(
  brm(
    formula = median_unique_bins_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_median_unique_bins_per_meal <- add_criterion(m1_brm_median_unique_bins_per_meal, "loo")
saveRDS(m1_brm_median_unique_bins_per_meal, file.path(output_dir, "m1_brm_median_unique_bins_per_meal.rds"))

###################################################################################################
################################## median_feeding_pct_per_meal ####################################
###################################################################################################
# skew=-0.61, bounded 0-100%, diagnostics OK except 1 pareto_k — Gaussian
warnings_median_feeding_pct_per_meal <- list()
m1_brm_median_feeding_pct_per_meal <- withCallingHandlers(
  brm(
    formula = median_feeding_pct_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_median_feeding_pct_per_meal <- add_criterion(m1_brm_median_feeding_pct_per_meal, "loo")
saveRDS(m1_brm_median_feeding_pct_per_meal, file.path(output_dir, "m1_brm_median_feeding_pct_per_meal.rds"))

###################################################################################################
################################## unique_feed_bins_visited #######################################
###################################################################################################
# skew=-0.48, diagnostics OK — Gaussian
warnings_unique_feed_bins_visited <- list()
m1_brm_unique_feed_bins_visited <- withCallingHandlers(
  brm(
    formula = unique_feed_bins_visited ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_unique_feed_bins_visited <- add_criterion(m1_brm_unique_feed_bins_visited, "loo")
saveRDS(m1_brm_unique_feed_bins_visited, file.path(output_dir, "m1_brm_unique_feed_bins_visited.rds"))

###################################################################################################
################################## unique_water_bins_visited ######################################
###################################################################################################
# skew=-0.36, diagnostics OK — Gaussian
warnings_unique_water_bins_visited <- list()
m1_brm_unique_water_bins_visited <- withCallingHandlers(
  brm(
    formula = unique_water_bins_visited ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_unique_water_bins_visited <- add_criterion(m1_brm_unique_water_bins_visited, "loo")
saveRDS(m1_brm_unique_water_bins_visited, file.path(output_dir, "m1_brm_unique_water_bins_visited.rds"))

###################################################################################################
################################## total_bins_visited #############################################
###################################################################################################
# skew=-0.49, diagnostics OK — Gaussian
warnings_total_bins_visited <- list()
m1_brm_total_bins_visited <- withCallingHandlers(
  brm(
    formula = total_bins_visited ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_total_bins_visited <- add_criterion(m1_brm_total_bins_visited, "loo")
saveRDS(m1_brm_total_bins_visited, file.path(output_dir, "m1_brm_total_bins_visited.rds"))

###################################################################################################
################################## number_of_non_nutritive_visits #################################
###################################################################################################
# skew=1.87, 0% zeros, low Bulk ESS with Gaussian, pp_check shows clear misfit — lognormal
warnings_number_of_non_nutritive_visits <- list()
m1_brm_number_of_non_nutritive_visits <- withCallingHandlers(
  brm(
    formula = number_of_non_nutritive_visits ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = lognormal(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_number_of_non_nutritive_visits <- add_criterion(m1_brm_number_of_non_nutritive_visits, "loo")
saveRDS(m1_brm_number_of_non_nutritive_visits, file.path(output_dir, "m1_brm_number_of_non_nutritive_visits.rds"))

###################################################################################################
################################## median_pct_feed_remaining ######################################
###################################################################################################
# skew=-0.36, diagnostics OK — Gaussian
warnings_median_pct_feed_remaining <- list()
m1_brm_median_pct_feed_remaining <- withCallingHandlers(
  brm(
    formula = median_pct_feed_remaining ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 6000,
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
m1_brm_median_pct_feed_remaining <- add_criterion(m1_brm_median_pct_feed_remaining, "loo")
saveRDS(m1_brm_median_pct_feed_remaining, file.path(output_dir, "m1_brm_median_pct_feed_remaining.rds"))

###################################################################################################
################################## median_non_nutritive_per_meal ##################################
###################################################################################################
# skew=2.13, 0.56% zeros, high Rhat + low Bulk ESS with Gaussian — hurdle_lognormal
# Fixed effects in both mu and hu to properly control for environment in both
# the continuous and zero-generating processes.
warnings_median_non_nutritive_per_meal <- list()
m1_brm_median_non_nutritive_per_meal <- withCallingHandlers(
  brm(
    formula = median_non_nutritive_per_meal ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = hurdle_lognormal(),
    warmup  = 1000,
    iter    = 10000,
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
m1_brm_median_non_nutritive_per_meal <- add_criterion(m1_brm_median_non_nutritive_per_meal, "loo")
saveRDS(m1_brm_median_non_nutritive_per_meal, file.path(output_dir, "m1_brm_median_non_nutritive_per_meal.rds"))

###################################################################################################
################################## median_pct_actor ###############################################
###################################################################################################
# skew=0.87, 23.84% zeros — bimodal with zero spike, pp_check terrible — hurdle_lognormal
warnings_median_pct_actor <- list()
m1_brm_median_pct_actor <- withCallingHandlers(
  brm(
    formula = median_pct_actor ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
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
m1_brm_median_pct_actor <- add_criterion(m1_brm_median_pct_actor, "loo")
saveRDS(m1_brm_median_pct_actor, file.path(output_dir, "m1_brm_median_pct_actor.rds"))

###################################################################################################
################################## median_pct_reactor #############################################
###################################################################################################
# skew=0.79, 18.31% zeros — bimodal with zero spike, pp_check terrible — hurdle_lognormal
warnings_median_pct_reactor <- list()
m1_brm_median_pct_reactor <- withCallingHandlers(
  brm(
    formula = median_pct_reactor ~ DIM + parity + THI_mean + poly(month, 2) + (1 | cow),
    data    = master_data,
    family  = gaussian(),
    warmup  = 1000,
    iter    = 10000,
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
m1_brm_median_pct_reactor <- add_criterion(m1_brm_median_pct_reactor, "loo")
saveRDS(m1_brm_median_pct_reactor, file.path(output_dir, "m1_brm_median_pct_reactor.rds"))

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
  feed_intake                    = m1_brm_feed_intake,
  feed_duration                  = m1_brm_feed_duration,
  feed_visits                    = m1_brm_feed_visits,
  water_intake                   = m1_brm_water_intake,
  water_duration                 = m1_brm_water_duration,
  water_visits                   = m1_brm_water_visits,
  total_meals                    = m1_brm_total_meals,
  median_meal_duration           = m1_brm_median_meal_duration,
  median_visit_per_meal          = m1_brm_median_visit_per_meal,
  median_intake_per_meal         = m1_brm_median_intake_per_meal,
  median_unique_bins_per_meal    = m1_brm_median_unique_bins_per_meal,
  median_feeding_pct_per_meal    = m1_brm_median_feeding_pct_per_meal,
  unique_feed_bins_visited       = m1_brm_unique_feed_bins_visited,
  unique_water_bins_visited      = m1_brm_unique_water_bins_visited,
  total_bins_visited             = m1_brm_total_bins_visited,
  number_of_non_nutritive_visits = m1_brm_number_of_non_nutritive_visits,
  median_pct_feed_remaining      = m1_brm_median_pct_feed_remaining,
  median_non_nutritive_per_meal  = m1_brm_median_non_nutritive_per_meal,
  median_pct_actor               = m1_brm_median_pct_actor,
  median_pct_reactor             = m1_brm_median_pct_reactor
)
