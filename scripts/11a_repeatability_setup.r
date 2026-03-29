###################################################################################################
################################## 11a. Repeatability — Data Setup ################################
###################################################################################################
# Run this script FIRST. It loads and assembles master_data and defines the variance-partitioning
# helper used by 11b and 11c.
#
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
load("results/9_filter_problematic_days/all_info_final.rda")

summary_df <- moo4feed::read_data_safely("results/1_data_cleaning/summary_df.csv",
                                         header = TRUE, sep = ",")

summary_df$date    <- ymd(summary_df$date, tz = "America/Los_Angeles")
all_info_final$date <- ymd(all_info_final$date, tz = "America/Los_Angeles")
all_info_final$cow  <- as.integer(all_info_final$cow)
summary_df$cow      <- as.integer(summary_df$cow)

data <- summary_df %>%
  semi_join(all_info_final, by = c("cow", "date"))

data <- data %>%
  left_join(
    all_info_final %>%
      select(cow, date, days_in_milk, Parity, milk_production, Elo, THI_mean) %>%
      rename(DIM = days_in_milk, parity = Parity),
    by = c("cow", "date")
  )

data$month <- lubridate::month(data$date)

stopifnot(all(!is.na(data$DIM)))

###################################################################################################
################################## Build master daily summary dataframe ##########################
###################################################################################################

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

bin_visits <- moo4feed::read_data_safely("results/3_bin_visit_analysis/bin_visits.csv",
                                         header = TRUE, sep = ",")
bin_visits$date <- ymd(bin_visits$date, tz = "America/Los_Angeles")
bin_visits$cow  <- as.integer(bin_visits$cow)

load("results/6_non_nutritive_visit_analysis/non_nutritive.rda")
non_nutritive_daily <- imap(non_nutritive, ~ mutate(.x, date = .y)) %>%
  bind_rows() %>%
  mutate(
    date = lubridate::ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  )

load("results/7_feed_availability_analysis/availability.rda")
avail_daily <- do.call(rbind, availability$daily_summary) %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(cow, date, median_pct_feed_remaining)

load("results/8_meal_level_behavior_analysis/all_meal_visits.rda")
all_meal_visits <- all_meal_visits %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(-total_meals) %>%
  select(cow, date, median_non_nutritive_per_meal)

load("results/8_meal_level_behavior_analysis/meal_roles.rda")
all_daily_roles <- do.call(rbind, meal_roles) %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  group_by(cow, date) %>%
  summarise(
    total_actor   = sum(actor_visits,   na.rm = TRUE),
    total_reactor = sum(reactor_visits, na.rm = TRUE),
    .groups = "drop"
  )

master_data <- data %>%
  left_join(meal_daily,          by = c("cow", "date")) %>%
  left_join(bin_visits,          by = c("cow", "date")) %>%
  left_join(non_nutritive_daily, by = c("cow", "date")) %>%
  left_join(avail_daily,         by = c("cow", "date")) %>%
  left_join(all_meal_visits,     by = c("cow", "date")) %>%
  left_join(all_daily_roles,     by = c("cow", "date")) %>%
  mutate(
    # +1 offset applied to variables modelled with lognormal family that can
    # legitimately be zero on a given day (e.g. a cow that had no displacement
    # events, or no non-nutritive visits).  The lognormal
    # distribution requires strictly positive values, so shifting by 1 avoids
    # a hard brms error while preserving the relative ordering and shape of
    # the distribution.  Interpret model estimates on the (original + 1) scale.
    median_non_nutritive_per_meal  = median_non_nutritive_per_meal  + 1,
    total_actor                  = total_actor                  + 1,
    total_reactor                = total_reactor                + 1
  )

dir.create("results/11_repeatability", showWarnings = FALSE, recursive = TRUE)
save(master_data, file = "results/11_repeatability/master_data.rda")

###################################################################################################
################################## Variance partitioning helper ###################################
###################################################################################################
# Repeatability and CVi for Bayesian GLMMs with (1 | cow) as the sole
# random effect.  Variance is partitioned into:
#   var.cow  = between-individual (random intercept) variance
#   var.res  = residual (within-individual) variance = sigma^2
# Repeatability R = var.cow / (var.cow + var.res)
#
# CVi (coefficient of individual variation) — both on the original data scale
# so that gaussian and lognormal models are directly comparable:
#   gaussian  (identity link): CVi = sigma_cow / mu_bar
#   lognormal (log link):      CVi = sqrt(exp(var.cow) - 1)
#
# References
# ----------
# Nakagawa, S., Johnson, P. C. D. & Schielzeth, H. (2017). The coefficient
#   of determination R^2 and intra-class correlation coefficient from
#   generalized linear mixed-effects models revisited and expanded.
#   J. R. Soc. Interface 14: 20170213.
#   https://doi.org/10.1098/rsif.2017.0213
#
# Nakagawa, S. & Schielzeth, H. (2010). Repeatability for Gaussian and
#   non-Gaussian data: a practical guide for biologists.  Biological Reviews
#   85: 935-956.
#   https://doi.org/10.1111/j.1469-185X.2010.00141.x
partition_variance <- function(m1_brm, response_var, data, n_sim = 10000) {
  ps  <- as_draws_df(m1_brm)
  fam <- family(m1_brm)$family

  var.cow <- ps$"sd_cow__Intercept"^2

  fe      <- fixef(m1_brm, summary = FALSE)   # n_draws x n_fixef
  X       <- standata(m1_brm)$X               # n_obs   x n_fixef
  eta_bar <- as.vector(fe %*% colMeans(X))    # n_draws

  if (fam %in% c("gaussian", "lognormal")) {
    var.res   <- ps$"sigma"^2
    var.total <- var.cow + var.res
  } else {
    stop("Unsupported family: ", fam)
  }

  R_cow <- var.cow / var.total
  R_res <- var.res / var.total

  if (fam == "gaussian") {
    CVi <- sqrt(var.cow) / eta_bar
  } else if (fam == "lognormal") {
    CVi <- sqrt(exp(var.cow) - 1)
  }

  cat("\n===", response_var, "(family:", fam, ") ===\n")
  cat("Repeatability (R_cow):   ", round(mean(R_cow), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(R_cow), 0.95), 4), "\n")
  cat("R_residual:              ", round(mean(R_res), 4), "\n")
  cat("CVi:                     ", round(mean(CVi), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVi), 0.95), 4), "\n")

  list(
    response = response_var,
    family   = fam,
    R_cow    = R_cow,
    R_res    = R_res,
    CVi      = CVi
  )
}
