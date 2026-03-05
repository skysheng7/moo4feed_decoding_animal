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
load("results/9_filter_problematic_days/all_info_final_selected.rdata")

# Read daily summary variables
summary_df <- moo4feed::read_data_safely("results/1_data_cleaning/summary_df.csv",
                                         header = TRUE, sep = ",")

# Ensure date types match for joining
summary_df$date <- ymd(summary_df$date, tz = "America/Los_Angeles")
all_info_final_selected$date <- ymd(all_info_final_selected$date, tz = "America/Los_Angeles")
all_info_final_selected$cow  <- as.integer(all_info_final_selected$cow)
summary_df$cow               <- as.integer(summary_df$cow)

# Filter summary_df to only the cow-date combinations present in all_info_final_selected
data <- summary_df %>%
  semi_join(all_info_final_selected, by = c("cow", "date"))

# Merge in covariates needed for the model (DIM, parity, THI_mean, group_number)
data <- data %>%
  left_join(
    all_info_final_selected %>%
      select(cow, date, days_in_milk, Parity, milk_production, Elo, THI_mean, group_number) %>%
      rename(DIM = days_in_milk, parity = Parity),
    by = c("cow", "date")
  )

# Confirm no missing covariate rows
stopifnot(all(!is.na(data$DIM)))
stopifnot(all(!is.na(data$group_number)))

###################################################################################################
################################## Helper: run one repeatability model ############################
###################################################################################################
run_repeatability <- function(response_var, data, output_dir = "results/11_repeatability") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  rds_path <- file.path(output_dir, paste0("m1_brm_", response_var, ".rds"))

  formula_str <- paste0(
    response_var,
    " ~ DIM + I(DIM^2) + parity + THI_mean + (1 | cow) + (1 | group_number)"
  )

  my.cores <- detectCores()

  m1_brm <- brm(
    formula  = as.formula(formula_str),
    data     = data,
    warmup   = 500,
    iter     = 3000,
    thin     = 2,
    chains   = 2,
    inits    = "random",
    cores    = my.cores,
    seed     = 12345
  )
  m1_brm <- add_criterion(m1_brm, "waic")
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
################################## Run repeatability for summary_df variables #####################
###################################################################################################
response_vars <- c(
  "feed_intake",
  "feed_duration",
  "feed_visits",
  "water_intake",
  "water_duration",
  "water_visits"
)

models      <- list()
partitions  <- list()

for (rv in response_vars) {
  cat("\nFitting model for:", rv, "\n")
  models[[rv]]     <- run_repeatability(rv, data)
  partitions[[rv]] <- partition_variance(models[[rv]], rv, data)
}

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

  # Identify top and bottom cow by posterior mean; all others labelled "Other individuals"
  posteriorBT <- posteriorBT %>%
    dplyr::group_by(cow) %>%
    dplyr::mutate(meanBT = mean(value)) %>%
    dplyr::ungroup()

  cow_means  <- posteriorBT %>% distinct(cow, meanBT)
  top_cow    <- cow_means$cow[which.max(cow_means$meanBT)]
  bottom_cow <- cow_means$cow[which.min(cow_means$meanBT)]

  posteriorBT$col <- ifelse(posteriorBT$cow == top_cow,    paste0("Highest (", top_cow, ")"),
                     ifelse(posteriorBT$cow == bottom_cow, paste0("Lowest (", bottom_cow, ")"),
                            "Other individuals"))

  fill_values <- c(
    setNames("#F8766D", paste0("Highest (", top_cow, ")")),
    setNames("#00BFC4", paste0("Lowest (", bottom_cow, ")")),
    "Other individuals" = "gray"
  )

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
    filename = file.path(output_dir, paste0("BT_plot_", response_var, ".pdf")),
    plot     = BT,
    width    = 8,
    height   = max(4, n_cows * 0.35)
  )

  return(BT)
}

bt_plots <- list()
for (rv in response_vars) {
  bt_plots[[rv]] <- plot_posterior_bt(models[[rv]], rv, data)
}
