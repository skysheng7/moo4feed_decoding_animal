###################################################################################################
################################## 11. Repeatability Analysis ####################################
###################################################################################################
# External packages
library(brms)        # for brm(), add_criterion(), fixef()
library(coda)        # for as.mcmc(), HPDinterval()
library(tidybayes)   # for posterior_samples()
library(tidyverse)   # for %>%, gather(), separate(), left_join(), group_by(), mutate(), ungroup()
library(ggplot2)     # for ggplot(), geom_point(), labs(), theme_classic(), scale_fill_manual()
library(parallel)    # for detectCores()
library(lubridate)   # for ymd()
library(moo4feed)    # for read_data_safely()

###################################################################################################
################################## Load and prepare data ##########################################
###################################################################################################
# Load the filtered cow-date combinations (only clean days from selected stable groups)
load("results/9_filter_problematic_days/all_info_final.rda")

# Read daily summary variables
summary_df <- moo4feed::read_data_safely("results/1_data_cleaning/summary_df.csv",
                                         header = TRUE, sep = ",")

# Ensure date types match for joining
summary_df$date <- ymd(summary_df$date, tz = "America/Los_Angeles")
all_info_final$date <- ymd(all_info_final$date, tz = "America/Los_Angeles")
all_info_final$cow  <- as.integer(all_info_final$cow)
summary_df$cow               <- as.integer(summary_df$cow)

# Filter summary_df to only the cow-date combinations present in all_info_final
data <- summary_df %>%
  semi_join(all_info_final, by = c("cow", "date"))

# Merge in covariates needed for the model (DIM, parity, THI_mean, group_number)
data <- data %>%
  left_join(
    all_info_final %>%
      select(cow, date, days_in_milk, Parity, milk_production, Elo, THI_mean, group_number) %>%
      rename(DIM = days_in_milk, parity = Parity),
    by = c("cow", "date")
  )

# Confirm no missing covariate rows
stopifnot(all(!is.na(data$DIM)))
stopifnot(all(!is.na(data$group_number)))

###################################################################################################
################################## Build master daily summary dataframe ##########################
###################################################################################################

# ---- meal_summaries.csv: median meal-level stats per cow per day --------------------------------
meal_summaries <- moo4feed::read_data_safely("results/2_meal_clustering/meal_summaries.csv",
                                             header = TRUE, sep = ",")
meal_summaries$date <- ymd(meal_summaries$date, tz = "America/Los_Angeles")
meal_summaries$cow  <- as.integer(meal_summaries$cow)

meal_daily <- meal_summaries %>%
  group_by(cow, date) %>%
  summarise(
    total_meals              = n(),
    median_meal_duration     = median(meal_duration,       na.rm = TRUE),
    median_visit_per_meal    = median(visit_count,         na.rm = TRUE),
    median_intake_per_meal   = median(total_intake,        na.rm = TRUE),
    median_unique_bins_per_meal = median(unique_bins_count,   na.rm = TRUE),
    median_feeding_pct_per_meal = median(feeding_percentage,  na.rm = TRUE),
    .groups = "drop"
  )

# ---- bin_visits.csv: unique bins visited per cow per day ----------------------------------------
bin_visits <- moo4feed::read_data_safely("results/3_bin_visit_analysis/bin_visits.csv",
                                         header = TRUE, sep = ",")
bin_visits$date <- ymd(bin_visits$date, tz = "America/Los_Angeles")
bin_visits$cow  <- as.integer(bin_visits$cow)

# ---- non_nutritive.rda: total non-nutritive visits per cow per day ------------------------------
load("results/6_non_nutritive_visit_analysis/non_nutritive.rda")
non_nutritive_daily <- imap(non_nutritive, ~ mutate(.x, date = .y)) %>%
  bind_rows() %>%
  mutate(
    date = lubridate::ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  )

# ---- availability.rda: median feed availability per cow per day ---------------------------------
load("results/7_feed_availability_analysis/availability.rda")
avail_daily <- do.call(rbind, availability$daily_summary) %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(cow, date, median_pct_feed_remaining)

# ---- all_meal_visits.rda: median non-nutritive & empty-bin visits per meal ----------------------
load("results/8_meal_level_behavior_analysis/all_meal_visits.rda")
all_meal_visits <- all_meal_visits %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(-total_meals) %>%
  select(cow, date,
         median_non_nutritive_per_meal)

# ---- all_daily_roles.rda: median actor/reactor percentages per cow per day ----------------------
load("results/8_meal_level_behavior_analysis/all_daily_roles.rda")
all_daily_roles <- all_daily_roles %>%
  mutate(
    date = ymd(date, tz = "America/Los_Angeles"),
    cow  = as.integer(cow)
  ) %>%
  select(-total_meals) %>%
  select(cow, date,
         median_pct_actor,
         median_pct_reactor,
         median_pct_actor_reactor)

# ---- Assemble master dataframe ------------------------------------------------------------------
master_data <- data %>%
  left_join(meal_daily,         by = c("cow", "date")) %>%
  left_join(bin_visits,         by = c("cow", "date")) %>%
  left_join(non_nutritive_daily, by = c("cow", "date")) %>%
  left_join(avail_daily,        by = c("cow", "date")) %>%
  left_join(all_meal_visits,    by = c("cow", "date")) %>%
  left_join(all_daily_roles,    by = c("cow", "date"))

save(master_data, file = "results/11_repeatability/master_data.rda")

###################################################################################################
################################## Helper: run one repeatability model ############################
###################################################################################################
run_repeatability <- function(response_var, data, output_dir = "results/11_repeatability") {
  dir.create(output_dir, showWarnings = TRUE, recursive = TRUE)
  rds_path <- file.path(output_dir, paste0("m1_brm_", response_var, ".rds"))

  formula_str <- paste0(
    response_var,
    " ~ DIM + parity + THI_mean + (1 | cow) + (1 | group_number)"
  )

  my.cores <- detectCores()

  m1_brm <- brm(
    formula  = as.formula(formula_str),
    data     = data,
    warmup   = 500,
    iter     = 3000,
    thin     = 2,
    chains   = 2,
    init     = "random",
    cores    = my.cores,
    seed     = 12345
  )
  saveRDS(m1_brm, rds_path)

  return(m1_brm)
}

###################################################################################################
################################## Variance partitioning helper ###################################
###################################################################################################
partition_variance <- function(m1_brm, response_var, data) {
  ps <- as_draws_df(m1_brm)

  var.cow         <- ps$"sd_cow__Intercept"^2
  var.group       <- ps$"sd_group_number__Intercept"^2
  var.res         <- ps$"sigma"^2
  var.total       <- var.cow + var.group + var.res

  R_cow   <- var.cow   / var.total
  R_group <- var.group / var.total
  R_res   <- var.res   / var.total

  CVi <- sqrt(var.cow) / mean(data[[response_var]], na.rm = TRUE)

  cat("\n===", response_var, "===\n")
  cat("Repeatability (R_cow):   ", round(mean(R_cow), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(R_cow), 0.95), 4), "\n")
  cat("R_group:                 ", round(mean(R_group), 4), "\n")
  cat("R_residual:              ", round(mean(R_res), 4), "\n")
  cat("CVi:                     ", round(mean(CVi), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVi), 0.95), 4), "\n")

  list(
    response    = response_var,
    R_cow       = R_cow,
    R_group     = R_group,
    R_res       = R_res,
    CVi         = CVi
  )
}


###################################################################################################
################################## Run repeatability for all master_data variables ################
###################################################################################################
response_vars <- c(
  # Basic feeding / watering behaviour
  "feed_intake",
  "feed_duration",
  "feed_visits",
  "water_intake",
  "water_duration",
  "water_visits",

  # Meal-level summaries
  "total_meals",
  "median_meal_duration",
  "median_visit_per_meal",
  "median_intake_per_meal",
  "median_unique_bins_per_meal",
  "median_feeding_pct_per_meal",
  # Bin-visit summaries
  "unique_feed_bins_visited",
  "unique_water_bins_visited",
  "total_bins_visited",
  # Non-nutritive / availability / social behaviour
  "number_of_non_nutritive_visits",
  "median_pct_feed_remaining",
  "median_non_nutritive_per_meal",
  "median_pct_actor",
  "median_pct_reactor",
  "median_pct_actor_reactor"
)

# Run models in parallel across response variables using a socket cluster (Windows-compatible).
# Each worker fits one brm model independently; within each model brms still uses all cores
# for its chains, so cap the per-model core count to avoid over-subscription.
n_vars        <- length(response_vars)
total_cores   <- parallel::detectCores()
# Use one worker per variable but no more than half the available cores so each
# brm model can still use 2 chains in parallel on the remaining cores.
n_workers     <- min(n_vars, max(1L, floor(total_cores / 2)))
brm_cores     <- max(1L, floor(total_cores / n_workers))

cat(sprintf(
  "\nLaunching %d parallel workers; each brm model will use %d core(s).\n",
  n_workers, brm_cores
))

cl <- parallel::makeCluster(n_workers)
parallel::clusterExport(cl, varlist = c("master_data", "brm_cores"), envir = environment())
parallel::clusterEvalQ(cl, {
  library(brms)
  library(coda)
  library(parallel)
})

run_repeatability_par <- function(response_var) {
  output_dir <- "results/11_repeatability"
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  rds_path <- file.path(output_dir, paste0("m1_brm_", response_var, ".rds"))

  formula_str <- paste0(
    response_var,
    " ~ DIM + parity + THI_mean + (1 | cow) + (1 | group_number)"
  )

  m1_brm <- brm(
    formula  = as.formula(formula_str),
    data     = master_data,
    warmup   = 500,
    iter     = 3000,
    thin     = 2,
    chains   = 2,
    init     = "random",
    cores    = brm_cores,
    seed     = 12345
  )
  saveRDS(m1_brm, rds_path)
  m1_brm
}

models <- parallel::parLapply(cl, response_vars, run_repeatability_par)
parallel::stopCluster(cl)
names(models) <- response_vars

partitions <- mapply(
  partition_variance,
  m1_brm       = models,
  response_var = response_vars,
  MoreArgs     = list(data = master_data),
  SIMPLIFY     = FALSE
)
names(partitions) <- response_vars

###################################################################################################
################################## Summary table #################################################
###################################################################################################
results_table <- do.call(rbind, lapply(partitions, function(p) {
  data.frame(
    variable    = p$response,
    R_cow_mean  = round(mean(p$R_cow), 4),
    R_cow_lower = round(HPDinterval(as.mcmc(p$R_cow), 0.95)[1], 4),
    R_cow_upper = round(HPDinterval(as.mcmc(p$R_cow), 0.95)[2], 4),
    R_group     = round(mean(p$R_group), 4),
    R_residual  = round(mean(p$R_res), 4),
    CVi_mean    = round(mean(p$CVi), 4),
    CVi_lower   = round(HPDinterval(as.mcmc(p$CVi), 0.95)[1], 4),
    CVi_upper   = round(HPDinterval(as.mcmc(p$CVi), 0.95)[2], 4)
  )
}))

print(results_table)
write.csv(results_table,
          "results/11_repeatability/repeatability_summary.csv",
          row.names = FALSE)

###################################################################################################
################################## Posterior BT plot #############################################
###################################################################################################
plot_posterior_bt <- function(m1_brm, response_var, data,
                              output_dir = "results/11_repeatability") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Extract individual-level posterior intercepts using posterior_samples()
  ps <- posterior_samples(m1_brm)
  cow_cols <- grep("^r_cow\\[", names(ps), value = TRUE)

  posteriorBT <- ps[, cow_cols] %>%
    gather(cow, value) %>%
    separate(cow,
             c(NA, NA, "cow", NA),
             sep = "([\\_\\[\\,])", fill = "right")

  # Adjust intercepts to the response scale using the population-level intercept
  posteriorBT$value <- posteriorBT$value + fixef(m1_brm, pars = "Intercept")[1]

  # Compute posterior means; highlight focal cows, all others labelled "Other individuals"
  posteriorBT <- posteriorBT %>%
    dplyr::group_by(cow) %>%
    dplyr::mutate(meanBT = mean(value)) %>%
    dplyr::ungroup()

  focal_cows   <- c("5042", "5120", "6022", "7169", "3067")
  focal_colors <- c("#C77CFF", "#F8766D", "#7CAE00", "#FFCC00", "#00BFC4")
  names(focal_colors) <- focal_cows

  posteriorBT$col <- ifelse(
    posteriorBT$cow %in% focal_cows,
    posteriorBT$cow,
    "Other individuals"
  )

  fill_values <- c(focal_colors, "Other individuals" = "gray")

  n_cows <- n_distinct(posteriorBT$cow)

  BT <- ggplot() +
    ggridges::geom_density_ridges(data = posteriorBT,
                                  aes(x      = value,
                                      y      = reorder(as.factor(cow), meanBT),
                                      height = ..density..,
                                      fill   = col,
                                      scale  = 3),
                                  alpha = 0.6) +
    geom_point(data = posteriorBT[!duplicated(posteriorBT$cow), ],
               aes(x = meanBT, y = as.factor(cow), col = col),
               size = 1) +
    labs(y = "", x = response_var, fill = "ID", col = "ID") +
    theme_classic() +
    scale_fill_manual(values  = fill_values) +
    scale_color_manual(values = fill_values)

  ggsave(
    filename = file.path(output_dir, paste0("BT_plot_", response_var, ".png")),
    plot     = BT,
    width    = 8,
    height   = max(4, n_cows * 0.35)
  )

  return(BT)
}

bt_plots <- list()
for (rv in response_vars) {
  bt_plots[[rv]] <- plot_posterior_bt(models[[rv]], rv, master_data)
}
