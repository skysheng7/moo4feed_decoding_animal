###################################################################################################
################################## 12. Predictability Analysis ###################################
###################################################################################################
# Predictability is quantified as within-individual variance (IIV) using a double hierarchical
# generalised linear model (DHGLM) in brms.  A sub-model for the residual SD (sigma) includes a
# random intercept per cow, so that each individual's intra-individual variability can be
# estimated.  A low IIV means the individual is highly predictable (consistent day-to-day).
#
# External packages
library(brms)        # for brm(), fixef(), as_draws_df()
library(coda)        # for as.mcmc(), HPDinterval()
library(tidyverse)   # for %>%, gather(), separate(), left_join(), group_by(), mutate(), ungroup()
library(ggplot2)     # for ggplot(), geom_point(), labs(), theme_classic(), scale_fill_manual()
library(ggridges)    # for geom_density_ridges()
library(ggrepel)     # for geom_label_repel()
library(parallel)    # for detectCores()

###################################################################################################
################################## Load master data ###############################################
###################################################################################################
load("results/11_repeatability/master_data.rda")

###################################################################################################
################################## Helper: extract IIV summaries ##################################
###################################################################################################
# rIIV  = variance of the sigma random effect (on the log scale)
# CVP   = coefficient of variation of predictability = sqrt(exp(sd^2) - 1)
extract_iiv <- function(m2_brm, response_var) {
  ps <- as_draws_df(m2_brm)

  sd_sigma <- ps$"sd_cow__sigma_Intercept"

  var_res  <- exp(sd_sigma)^2
  log_norm <- exp(sd_sigma^2)
  CVP      <- sqrt(log_norm - 1)

  cat("\n===", response_var, "===\n")
  cat("rIIV (mean):  ", round(mean(var_res), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(var_res), 0.95), 4), "\n")
  cat("CVP  (mean):  ", round(mean(CVP), 4),
      "  95% HPD:", round(HPDinterval(as.mcmc(CVP), 0.95), 4), "\n")

  list(
    response = response_var,
    var_res  = var_res,
    CVP      = CVP
  )
}

###################################################################################################
################################## Response variables #############################################
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

###################################################################################################
################################## Run DHGLM models ###############################################
###################################################################################################
# ---- Configuration: set which variable(s) to run ------------------------------------------------
# To debug one variable at a time, set run_vars to a single variable, e.g.:
#   run_vars <- "feed_intake"
# To run all variables, set:
#   run_vars <- response_vars
# To debug one variable at a time, set run_vars to a single variable, e.g.:
#   run_vars <- "feed_intake"
# To run all variables, set:
#   run_vars <- response_vars
run_vars <- response_vars

# Parallel setup: each brm gets 2 cores for its chains, remaining cores run models in parallel
total_cores <- parallel::detectCores()
brm_cores   <- 4L
n_workers   <- max(1L, floor(total_cores / brm_cores))

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

run_predictability_par <- function(response_var) {
  output_dir <- "results/12_predictability"
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  rds_path <- file.path(output_dir, paste0("m2_brm_", response_var, ".rds"))

  # Mean sub-model mirrors the repeatability formula (script 11)
  # Sigma sub-model adds (1 | cow) to let each cow have its own residual SD
  double_model <- brms::bf(
    as.formula(paste0(
      response_var,
      " ~ DIM + parity + THI_mean + month + I(month^2) + (1 | cow)"
    )),
    sigma ~ (1 | cow)
  )

  # Capture all warnings during model fitting
  warnings_list <- list()
  m2_brm <- withCallingHandlers(
    brms::brm(
      formula = double_model,
      data    = master_data,
      warmup  = 1000,
      iter    = 6000,
      thin    = 1,
      chains  = 4,
      init    = "random",
      cores   = brm_cores,
      seed    = 12345,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
    ),
    warning = function(w) {
      warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  saveRDS(m2_brm, rds_path)

  list(
    model    = m2_brm,
    warnings = warnings_list
  )
}

results_list <- parallel::parLapply(cl, run_vars, run_predictability_par)
parallel::stopCluster(cl)
names(results_list) <- run_vars

# Print all captured warnings per variable
cat("\n\n========== WARNINGS SUMMARY ==========\n")
any_warnings <- FALSE
for (rv in run_vars) {
  w <- results_list[[rv]]$warnings
  # Filter out package version warnings (not actionable)
  w <- w[!grepl("was built under R version", w)]
  if (length(w) > 0) {
    any_warnings <- TRUE
    cat("\n---", rv, "---\n")
    for (msg in w) cat("  WARNING:", msg, "\n")
  }
}
if (!any_warnings) cat("No warnings (excluding package version notices).\n")
cat("======================================\n\n")

# Extract models
models <- lapply(results_list, `[[`, "model")

###################################################################################################
################################## Extract IIV for all variables ##################################
###################################################################################################
iiv_results <- mapply(
  extract_iiv,
  m2_brm       = models,
  response_var = run_vars,
  SIMPLIFY     = FALSE
)
names(iiv_results) <- run_vars

###################################################################################################
################################## Summary table #################################################
###################################################################################################
results_table <- do.call(rbind, lapply(iiv_results, function(p) {
  data.frame(
    variable       = p$response,
    rIIV_mean      = round(mean(p$var_res), 4),
    rIIV_lower     = round(HPDinterval(as.mcmc(p$var_res), 0.95)[1], 4),
    rIIV_upper     = round(HPDinterval(as.mcmc(p$var_res), 0.95)[2], 4),
    CVP_mean       = round(mean(p$CVP), 4),
    CVP_lower      = round(HPDinterval(as.mcmc(p$CVP), 0.95)[1], 4),
    CVP_upper      = round(HPDinterval(as.mcmc(p$CVP), 0.95)[2], 4)
  )
}))

print(results_table)
write.csv(results_table,
          "results/12_predictability/predictability_summary.csv",
          row.names = FALSE)

###################################################################################################
################################## Posterior IIV ridge plot #######################################
###################################################################################################
plot_posterior_iiv <- function(m2_brm, response_var,
                               output_dir = "results/12_predictability") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ps       <- as_draws_df(m2_brm)
  iiv_cols <- grep("^r_cow__sigma\\[", names(ps), value = TRUE)

  # Pivot to long format; column names look like r_cow__sigma[5042,Intercept]
  posteriorIIV <- ps[, iiv_cols] %>%
    tidyr::pivot_longer(
      cols      = everything(),
      names_to  = "raw_col",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      cow = sub("^r_cow__sigma\\[(.+),Intercept\\]$", "\\1", raw_col)
    ) %>%
    dplyr::select(-raw_col)

  # Shift by the population-level sigma intercept to get absolute IIV on the log scale
  posteriorIIV$value <- posteriorIIV$value +
    fixef(m2_brm, pars = "sigma_Intercept")[1]

  posteriorIIV <- posteriorIIV %>%
    dplyr::group_by(cow) %>%
    dplyr::mutate(meanIIV = mean(value)) %>%
    dplyr::ungroup()

  focal_cows   <- c("5042", "5120", "6022", "7169", "3067")
  focal_colors <- c("#C77CFF", "#F8766D", "#7CAE00", "#FFCC00", "#00BFC4")
  names(focal_colors) <- focal_cows

  posteriorIIV$col <- ifelse(
    posteriorIIV$cow %in% focal_cows,
    posteriorIIV$cow,
    "Other individuals"
  )

  fill_values <- c(focal_colors, "Other individuals" = "gray")
  n_cows      <- dplyr::n_distinct(posteriorIIV$cow)

  # Build a summary for the mean-dot layer (one row per cow, ordered by meanIIV)
  cow_summary <- posteriorIIV %>%
    dplyr::distinct(cow, meanIIV, col) %>%
    dplyr::mutate(cow_ordered = reorder(as.factor(cow), meanIIV))

  IIV_plot <- ggplot() +
    ggridges::geom_density_ridges(
      data  = posteriorIIV,
      aes(x    = value,
          y    = reorder(as.factor(cow), meanIIV),
          fill = col),
      scale = 3,
      alpha = 0.6
    ) +
    geom_point(
      data = cow_summary,
      aes(x = meanIIV, y = cow_ordered, col = col),
      size = 1
    ) +
    labs(y = "", x = paste0("rIIV \u2014 ", response_var), fill = "ID", col = "ID") +
    theme_classic() +
    scale_fill_manual(values  = fill_values) +
    scale_color_manual(values = fill_values)

  ggsave(
    filename = file.path(output_dir, paste0("IIV_plot_", response_var, ".png")),
    plot     = IIV_plot,
    width    = 8,
    height   = max(4, n_cows * 0.35)
  )

  return(IIV_plot)
}

iiv_plots <- list()
for (rv in names(models)) {
  iiv_plots[[rv]] <- plot_posterior_iiv(models[[rv]], rv)
}

###################################################################################################
################################## Repeatability vs Predictability ################################
###################################################################################################
# Combine CVi (repeatability) and CVP (predictability) summaries and plot them against each other.
# High CVi  = high between-individual variation (animals differ a lot on average)
# High CVP  = high within-individual variation  (animals are unpredictable day-to-day)

rep_summary  <- moo4feed::read_data_safely("results/11_repeatability/repeatability_summary.csv",
                                           header = TRUE, sep = ",")
pred_summary <- moo4feed::read_data_safely("results/12_predictability/predictability_summary.csv",
                                           header = TRUE, sep = ",")

combined_summary <- rep_summary  %>%
  dplyr::select(variable, CVi_mean) %>%
  dplyr::inner_join(
    pred_summary %>% dplyr::select(variable, CVP_mean, rIIV_mean),
    by = "variable"
  ) %>%
  dplyr::filter(variable != "median_pct_actor_reactor")

# ---- shared label layer helper -----------------------------------------------
repred_label_layer <- function() {
  ggrepel::geom_label_repel(
    aes(fill = variable),
    colour          = "black",
    fontface        = "bold",
    size            = 3,
    alpha           = 0.7,
    label.padding   = unit(0.2, "lines"),
    box.padding     = unit(0.4, "lines"),
    point.padding   = unit(0.3, "lines"),
    direction       = "both",
    max.overlaps    = Inf,
    show.legend     = FALSE
  )
}

# ---- Plot 1: CVP_mean (y) vs CVi_mean (x) ------------------------------------
scatter_CVP_CVi <- ggplot(combined_summary,
                          aes(x = CVi_mean, y = CVP_mean,
                              colour = variable, label = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  repred_label_layer() +
  labs(
    x      = "CVi mean (Repeatability \u2014 between-individual variation)",
    y      = "CVP mean (Predictability \u2014 within-individual variation)",
    colour = "Variable"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(
  filename = "results/12_predictability/scatter_CVP_vs_CVi.png",
  plot     = scatter_CVP_CVi,
  width    = 10,
  height   = 8
)

# ---- Plot 2: rIIV_mean (y) vs CVi_mean (x) -----------------------------------
scatter_rIIV_CVi <- ggplot(combined_summary,
                           aes(x = CVi_mean, y = rIIV_mean,
                               colour = variable, label = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  repred_label_layer() +
  labs(
    x      = "CVi mean (Repeatability \u2014 between-individual variation)",
    y      = "rIIV mean (Predictability \u2014 residual intra-individual variance)",
    colour = "Variable"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(
  filename = "results/12_predictability/scatter_rIIV_vs_CVi.png",
  plot     = scatter_rIIV_CVi,
  width    = 10,
  height   = 8
)
