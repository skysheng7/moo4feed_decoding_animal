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

load("results/8_meal_level_behavior_analysis/all_daily_roles.rda")
all_daily_roles <- all_daily_roles %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(-total_meals) %>%
  select(cow, date,
         median_pct_actor,
         median_pct_reactor)

master_data <- data %>%
  left_join(meal_daily,          by = c("cow", "date")) %>%
  left_join(bin_visits,          by = c("cow", "date")) %>%
  left_join(non_nutritive_daily, by = c("cow", "date")) %>%
  left_join(avail_daily,         by = c("cow", "date")) %>%
  left_join(all_meal_visits,     by = c("cow", "date")) %>%
  left_join(all_daily_roles,     by = c("cow", "date"))

###################################################################################################
################################## Rescale percentage variables for Beta / ZOIB models ############
###################################################################################################
# Beta requires (0, 1); clamp away from exact 0/1 with small epsilon.
# ZOIB allows [0, 1] so just divide by 100.
eps <- 1e-6
master_data$median_feeding_pct_per_meal_prop <- pmin(pmax(
  master_data$median_feeding_pct_per_meal / 100, eps), 1 - eps)
master_data$median_pct_feed_remaining_prop <- pmin(pmax(
  master_data$median_pct_feed_remaining / 100, eps), 1 - eps)
master_data$median_pct_actor_prop   <- master_data$median_pct_actor / 100
master_data$median_pct_reactor_prop <- master_data$median_pct_reactor / 100

dir.create("results/11_repeatability", showWarnings = FALSE, recursive = TRUE)
save(master_data, file = "results/11_repeatability/master_data.rda")

###################################################################################################
################################## Variance partitioning helper ###################################
###################################################################################################
# Repeatability and CVi for Bayesian GLMMs with (1 | cow) as the sole
# random effect.  Variance is partitioned into:
#   var.cow  = between-individual (random intercept) variance
#   var.res  = residual (within-individual + distribution-specific) variance
# Repeatability R = var.cow / (var.cow + var.res)
#
# Family-specific residual variance (Nakagawa et al. 2017, Table 2):
#   gaussian / lognormal / hurdle_lognormal — sigma^2
#   negbinomial (log link)                  — log(1 + 1/lambda + 1/shape)
#   beta / zero_one_inflated_beta (logit)   — pi^2 / 3
#
# CVi (coefficient of individual variation) — all on the original data scale:
#   gaussian (identity link):
#       CVi = sigma_cow / mu_bar                           (closed-form)
#   lognormal / hurdle_lognormal / negbinomial (log link):
#       CVi = sqrt(exp(var.cow) - 1)                       (closed-form)
#   beta / zero_one_inflated_beta (logit link):
#       No closed-form for logistic-normal moments; CVi is obtained by
#       simulation (de Villemereuil et al. 2016, Appendix A).
#
# For families with non-linear link functions (logit, log for negbinomial
# residual variance), quantities on the data scale depend on the location
# of the linear predictor.  We therefore evaluate at the population-average
# linear predictor (fixed effects averaged over the observed covariate
# distribution) rather than at the intercept alone, following the approach
# implemented in QGglmm (de Villemereuil 2018).
#
# References
# ----------
# de Villemereuil, P., Schielzeth, H., Nakagawa, S. & Morrissey, M. B.
#   (2016). General methods for evolutionary quantitative genetic inference
#   from generalised mixed models.  Genetics 204: 1281-1294.
#   https://doi.org/10.1534/genetics.115.186536
#
# de Villemereuil, P. (2018). QGglmm: Estimate Quantitative Genetics
#   Parameters from Generalised Linear Mixed Models.  R package.
#   https://CRAN.R-project.org/package=QGglmm
#
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

  data_var <- as.character(m1_brm$formula$formula[[2]])

  var.cow <- ps$"sd_cow__Intercept"^2

  # Population-average fixed-effect linear predictor per posterior draw.
  # By linearity of eta, rowMeans(X %*% t(beta)) == beta %*% colMeans(X),
  # so evaluating at the column-means of the design matrix is equivalent to
  # averaging the per-observation linear predictors over the observed
  # covariate distribution, without creating a large n_draws x n_obs matrix.
  fe      <- fixef(m1_brm, summary = FALSE)   # n_draws x n_fixef
  X       <- standata(m1_brm)$X               # n_obs   x n_fixef
  eta_bar <- as.vector(fe %*% colMeans(X))    # n_draws

  if (fam %in% c("gaussian", "lognormal", "hurdle_lognormal")) {
    var.res   <- ps$"sigma"^2
    var.total <- var.cow + var.res

  } else if (fam == "negbinomial") {
    shape   <- ps$"shape"
    lambda  <- exp(eta_bar + 0.5 * var.cow)
    var.res   <- log(1 + 1 / lambda + 1 / shape)
    var.total <- var.cow + var.res

  } else if (fam %in% c("beta", "zero_one_inflated_beta")) {
    var.res   <- pi^2 / 3
    var.total <- var.cow + var.res

  } else {
    stop("Unsupported family: ", fam)
  }

  R_cow <- var.cow / var.total
  R_res <- var.res / var.total

  # --- CVi on the original data scale ---
  if (fam == "gaussian") {
    # Identity link: sigma_cow / E[Y].  Using eta_bar propagates uncertainty
    # in the population mean into the CVi posterior.
    CVi <- sqrt(var.cow) / eta_bar

  } else if (fam %in% c("lognormal", "hurdle_lognormal", "negbinomial")) {
    # Log link: closed-form; the location on the log scale cancels because
    # exp() is multiplicative — CVi depends only on the random-effect variance.
    CVi <- sqrt(exp(var.cow) - 1)

  } else if (fam %in% c("beta", "zero_one_inflated_beta")) {
    # Logit link: no closed-form for logistic-normal moments.
    # Simulate n_sim latent individual values per posterior draw and
    # transform through plogis() to the (0,1) scale, then compute
    # CVi = SD(mu_j) / mean(mu_j)  (de Villemereuil et al. 2016).
    #
    # A single shared vector of standard-normal draws (common random numbers)
    # is reused across all posterior samples to reduce Monte Carlo noise.
    sd_cow <- ps$"sd_cow__Intercept"
    set.seed(42)
    u <- rnorm(n_sim)
    CVi <- vapply(seq_along(eta_bar), function(s) {
      mu_j <- plogis(eta_bar[s] + sd_cow[s] * u)
      sd(mu_j) / mean(mu_j)
    }, numeric(1))
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
