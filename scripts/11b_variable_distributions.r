###################################################################################################
###################### 11b. Behaviour Variable Distribution Exploration ###########################
###################################################################################################
# PURPOSE: Visualise the raw distribution of every response variable in master_data via a
# grid of histograms.  Overlays a normal-density curve (scaled to counts) so you can
# judge at a glance whether Gaussian is plausible, or whether you need a different
# likelihood family (Poisson, negative-binomial, gamma, beta, zero-inflated, etc.).
#
# PREREQUISITES: results/11_repeatability/master_data.rda must exist.
#   Run 11a_repeatability_setup.r first if it does not.
#
# OUTPUT: results/11_repeatability/variable_distributions.png
#         results/11_repeatability/variable_distributions_log.png  (log-scale x-axis)
#         results/11_repeatability/normality_tests.csv

library(tidyverse)
library(scales)
library(moments)     # for skewness() and kurtosis()
library(gridExtra)   # for arrangeGrob()
library(grid)        # for textGrob(), gpar()

###################################################################################################
################################## Load data ######################################################
###################################################################################################
if (!exists("master_data")) {
  load("results/11_repeatability/master_data.rda")
}

output_dir <- "results/11_repeatability"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Variable list (18 response vars) ###############################
###################################################################################################
response_vars <- c(
  "feed_intake",                   "feed_duration",
  "feed_visits",                   "water_intake",
  "water_duration",                "water_visits",
  "total_meals",                   "median_meal_duration",
  "median_visit_per_meal",         "median_intake_per_meal",
  "median_feeding_pct_per_meal",   "number_of_non_nutritive_visits",
  "median_pct_feed_remaining",     "median_non_nutritive_per_meal",
  "total_actor",                   "total_reactor",
  "median_feed_rate",              "median_water_rate"
)

# Human-readable labels
var_labels <- c(
  feed_intake                   = "Feed intake (kg/d)",
  feed_duration                 = "Feed duration (min/d)",
  feed_visits                   = "Feed visits (n/d)",
  water_intake                  = "Water intake (L/d)",
  water_duration                = "Water duration (min/d)",
  water_visits                  = "Water visits (n/d)",
  total_meals                   = "Total meals (n/d)",
  median_meal_duration          = "Meal duration (min, median)",
  median_visit_per_meal         = "Visits per meal (median)",
  median_intake_per_meal        = "Intake per meal (kg, median)",
  median_feeding_pct_per_meal   = "Feeding % per meal (median)",
  number_of_non_nutritive_visits= "Non-nutritive visits (n/d)",
  median_pct_feed_remaining     = "Feed remaining % (median)",
  median_non_nutritive_per_meal = "Non-nutritive/meal (median)",
  total_actor                   = "Actor displacements (n/d)",
  total_reactor                 = "Reactor displacements (n/d)",
  median_feed_rate              = "Feed intake rate (kg/s, median)",
  median_water_rate             = "Water intake rate (kg/s, median)"
)

###################################################################################################
################################## Normality summary table ########################################
###################################################################################################
set.seed(42)
normality_stats <- lapply(response_vars, function(rv) {
  x <- master_data[[rv]]
  x <- x[!is.na(x)]
  n <- length(x)

  sw <- tryCatch(shapiro.test(sample(x, min(n, 5000))), error = function(e) NULL)
  sw_p  <- if (!is.null(sw)) sw$p.value else NA
  sw_W  <- if (!is.null(sw)) sw$statistic else NA

  data.frame(
    variable    = rv,
    n           = n,
    mean        = round(mean(x), 4),
    sd          = round(sd(x), 4),
    median      = round(median(x), 4),
    min         = round(min(x), 4),
    max         = round(max(x), 4),
    skewness    = round(moments::skewness(x), 4),
    kurtosis    = round(moments::kurtosis(x), 4),
    pct_zero    = round(100 * mean(x == 0), 2),
    SW_W        = round(sw_W, 4),
    SW_p        = signif(sw_p, 4),
    normal_flag = ifelse(!is.na(sw_p) & sw_p > 0.05, "possibly normal", "non-normal"),
    stringsAsFactors = FALSE
  )
})
normality_df <- do.call(rbind, normality_stats)

print(normality_df[, c("variable", "n", "mean", "sd", "skewness", "kurtosis",
                        "pct_zero", "SW_W", "SW_p", "normal_flag")],
      row.names = FALSE)

write.csv(normality_df,
          file.path(output_dir, "normality_tests.csv"),
          row.names = FALSE)
cat("Saved: results/11_repeatability/normality_tests.csv\n")

###################################################################################################
################################## Helper: single histogram panel #################################
###################################################################################################
make_hist_panel <- function(rv, data, log_x = FALSE) {
  x_raw  <- data[[rv]]
  x_raw  <- x_raw[!is.na(x_raw)]
  label  <- var_labels[rv]

  # For log-scale, drop zeros/negatives
  if (log_x) {
    x_plot <- x_raw[x_raw > 0]
    if (length(x_plot) < 10) return(NULL)   # nothing to plot
    x_axis <- log(x_plot)
  } else {
    x_plot <- x_raw
    if (length(x_plot) < 10) return(NULL)
    x_axis <- x_plot
  }

  n_obs   <- length(x_plot)
  mu      <- mean(x_axis)
  sigma   <- sd(x_axis)
  n_bins  <- max(20, min(60, round(sqrt(n_obs))))

  # Shapiro-Wilk p (sample up to 5000, seeded per variable for reproducibility)
  set.seed(match(rv, names(var_labels)))
  sw_res <- tryCatch(shapiro.test(sample(x_axis, min(n_obs, 5000))),
                     error = function(e) NULL)
  sw_p   <- if (!is.null(sw_res)) sw_res$p.value else NA
  skew   <- round(moments::skewness(x_axis), 2)
  pct_z  <- round(100 * mean(x_raw == 0), 1)

  # Normal overlay: scale density to match count histogram
  bin_width <- diff(range(x_axis)) / n_bins
  scale_fac <- n_obs * bin_width

  subtitle_parts <- c(
    paste0("skew=", skew),
    paste0("zeros=", pct_z, "%")
  )
  if (!is.na(sw_p)) {
    sw_label <- if (sw_p < 0.001) "SW p<0.001" else paste0("SW p=", round(sw_p, 3))
    subtitle_parts <- c(subtitle_parts, sw_label)
  }

  fill_col  <- if (!is.na(sw_p) && sw_p > 0.05) "#4CAF50" else "#E57373"
  alpha_col <- 0.65

  df_plot <- data.frame(x = x_axis)

  p <- ggplot(df_plot, aes(x = x)) +
    geom_histogram(bins = n_bins, fill = fill_col, colour = "white",
                   alpha = alpha_col, linewidth = 0.2) +
    stat_function(
      fun    = function(z) dnorm(z, mean = mu, sd = sigma) * scale_fac,
      colour = "black", linewidth = 0.8, linetype = "dashed",
      na.rm  = TRUE
    ) +
    labs(
      title    = label,
      subtitle = paste(subtitle_parts, collapse = "  |  "),
      x        = if (log_x) paste0("log(", rv, ")") else rv,
      y        = "Count"
    ) +
    theme_classic(base_size = 9) +
    theme(
      plot.title    = element_text(size = 9,  face = "bold"),
      plot.subtitle = element_text(size = 7,  colour = "grey30"),
      axis.title    = element_text(size = 7.5),
      axis.text     = element_text(size = 6.5),
      plot.margin   = margin(4, 6, 4, 6)
    )

  return(p)
}

###################################################################################################
################################## Build grid — raw scale #########################################
###################################################################################################
cat("Building raw-scale histogram grid...\n")

panels_raw <- lapply(response_vars, function(rv) {
  tryCatch(make_hist_panel(rv, data = master_data, log_x = FALSE),
           error = function(e) { message("Raw panel failed for ", rv, ": ", e$message); NULL })
})
names(panels_raw) <- response_vars

panels_raw <- lapply(panels_raw, function(p) if (is.null(p)) grid::rectGrob(gp = grid::gpar(col = NA)) else p)
n_valid   <- length(panels_raw)
valid_raw <- panels_raw
n_cols    <- 4
n_rows    <- ceiling(n_valid / n_cols)

title_raw <- tryCatch(
  grid::textGrob(
    "Behaviour Variable Distributions — Raw Scale\n(green = Shapiro-Wilk p > 0.05, dashed = Normal curve)",
    gp = grid::gpar(fontsize = 13, fontface = "bold")),
  error = function(e) grid::textGrob(
    "Behaviour Variable Distributions — Raw Scale\n(green = Shapiro-Wilk p > 0.05, dashed = Normal curve)",
    gp = grid::gpar(fontsize = 14))
)

grid_raw <- do.call(
  gridExtra::arrangeGrob,
  c(valid_raw, list(ncol = n_cols, top = title_raw))
)

ggsave(
  filename = file.path(output_dir, "variable_distributions.png"),
  plot     = grid_raw,
  width    = n_cols * 4,
  height   = n_rows * 3.2,
  dpi      = 150
)
cat("Saved: results/11_repeatability/variable_distributions.png\n")

###################################################################################################
################################## Build grid — log scale #########################################
###################################################################################################
cat("Building log-scale histogram grid...\n")

panels_log <- lapply(response_vars, function(rv) {
  tryCatch(make_hist_panel(rv, data = master_data, log_x = TRUE),
           error = function(e) { message("Log panel failed for ", rv, ": ", e$message); NULL })
})
names(panels_log) <- response_vars

valid_log <- Filter(Negate(is.null), panels_log)
n_valid_l <- length(valid_log)
n_rows_l  <- ceiling(n_valid_l / n_cols)

title_log <- tryCatch(
  grid::textGrob(
    "Behaviour Variable Distributions — Log Scale\n(green = Shapiro-Wilk p > 0.05 on log values, dashed = Normal curve)",
    gp = grid::gpar(fontsize = 13, fontface = "bold")),
  error = function(e) grid::textGrob(
    "Behaviour Variable Distributions — Log Scale\n(green = Shapiro-Wilk p > 0.05 on log values, dashed = Normal curve)",
    gp = grid::gpar(fontsize = 14))
)

grid_log <- do.call(
  gridExtra::arrangeGrob,
  c(valid_log, list(ncol = n_cols, top = title_log))
)

ggsave(
  filename = file.path(output_dir, "variable_distributions_log.png"),
  plot     = grid_log,
  width    = n_cols * 4,
  height   = n_rows_l * 3.2,
  dpi      = 150
)
cat("Saved: results/11_repeatability/variable_distributions_log.png\n")

###################################################################################################
################################## Print distribution guidance ####################################
###################################################################################################
cat("\n", strrep("=", 70), "\n")
cat("DISTRIBUTION SUMMARY & SUGGESTED LIKELIHOOD FAMILIES\n")
cat(strrep("=", 70), "\n\n")

for (i in seq_len(nrow(normality_df))) {
  rv   <- normality_df$variable[i]
  sk   <- normality_df$skewness[i]
  pz   <- normality_df$pct_zero[i]
  swp  <- normality_df$SW_p[i]
  mn   <- normality_df$mean[i]
  suggestion <- dplyr::case_when(
    pz > 20                        ~ "Zero-inflated (hurdle_lognormal / hurdle_gamma / zero_inflated_poisson)",
    pz > 5 & abs(sk) > 1          ~ "Zero-inflated or hurdle model",
    !is.na(swp) & swp > 0.05      ~ "Gaussian (normal) — current default is fine",
    abs(sk) < 0.5                  ~ "Gaussian — mild skew, likely OK",
    sk > 1 & mn > 0 & pz == 0     ~ "Log-normal or Gamma",
    sk > 1 & pz > 0               ~ "Hurdle-Gamma or Zero-inflated Poisson",
    sk > 0.5                       ~ "Log-normal or Gamma",
    sk < -0.5                      ~ "Consider Beta (if bounded 0-1) or reflected distribution",
    TRUE                           ~ "Inspect histogram manually"
  )
  cat(sprintf("%-40s  skew=%+5.2f  zeros=%4.1f%%  SW_p=%s\n  => %s\n\n",
              rv, sk, pz,
              ifelse(is.na(swp), "  NA", formatC(swp, digits = 3, format = "g")),
              suggestion))
}
