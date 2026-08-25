###################################################################################################
################################## 14. Extract Individual Traits & Cluster Behaviours #############
###################################################################################################
# Prerequisites:
#   - results/11_repeatability/m1_brm_*.rds  (from 11c)
#   - results/12_predictability/m2_brm_*.rds  (from 12a)
#   - results/11_repeatability/master_data.rda (from 11a)
#   - results/11_repeatability/repeatability_summary.csv (from 11d)
#
# Outputs:
#   - results/14_individual_traits/cow_intercepts.csv
#   - results/14_individual_traits/cow_slopes.csv
#   - results/14_individual_traits/cow_traits_wide.csv  (intercepts + slopes merged)
#   - results/14_individual_traits/behaviour_clusters.csv
#   - results/14_individual_traits/behaviour_cluster_plot.png

library(brms)
library(tidyverse)
library(ggplot2)
library(ggrepel)

output_dir <- "results/14_individual_traits"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Configuration ##################################################
###################################################################################################
response_vars <- c(
  "feed_intake", "feed_duration", "feed_visits",
  "water_intake", "water_duration", "water_visits",
  "total_meals", "median_meal_duration", "median_visit_per_meal",
  "median_intake_per_meal", "median_feeding_pct_per_meal",
  "number_of_non_nutritive_visits", "median_pct_feed_remaining",
  "median_non_nutritive_per_meal", "total_actor", "total_reactor",
  "median_feed_rate", "median_water_rate"
)

# Variables that had a +1 offset applied in 11a (lognormal, can be zero)
offset_vars <- c("median_non_nutritive_per_meal", "total_actor", "total_reactor",
                  "median_feed_rate", "median_water_rate")

# Load master_data (needed for gaussian back-transformation)
load("results/11_repeatability/master_data.rda")

###################################################################################################
################################## [1] Extract individual intercepts (behaviour type) #############
###################################################################################################
# For each cow, the intercept = population intercept + cow random effect (posterior mean).
# For lognormal models: exponentiate to get the original scale.
# For offset_vars: subtract 1 to undo the +1 shift applied in 11a.
# This mirrors the x-axis of the BT ridge plots in 11d.

cat("\n========== Extracting individual intercepts ==========\n")

intercept_list <- list()

for (rv in response_vars) {
  rds_path <- file.path("results/11_repeatability", paste0("m1_brm_", rv, ".rds"))
  if (!file.exists(rds_path)) {
    message("Skipping ", rv, " (m1 not found)")
    next
  }
  cat("  Intercept:", rv, "\n")
  m <- readRDS(rds_path)

  fam <- family(m)$family
  ps  <- as_draws_df(m)

  # Select only the mu-part cow random effects (exclude hu, zoi, coi parts)
  cow_cols <- grep("^r_cow\\[", names(ps), value = TRUE)
  cow_cols <- cow_cols[!grepl("__(hu|zoi|coi)", cow_cols)]

  # Population intercept (posterior mean)
  pop_intercept <- mean(ps$b_Intercept)

  # Per-cow posterior means of the random effect
  cow_re <- colMeans(ps[, cow_cols, drop = FALSE])

  # Extract cow IDs from column names like r_cow[5042,Intercept]
  cow_ids <- sub("^r_cow\\[(.+),Intercept\\]$", "\\1", names(cow_re))

  # Individual intercept on the link scale
  ind_intercept <- pop_intercept + cow_re

  # Back-transform to original scale
  if (fam == "lognormal") {
    ind_intercept <- exp(ind_intercept)
  }

  # Undo the +1 offset for zero-capable lognormal variables
  if (rv %in% offset_vars) {
    ind_intercept <- ind_intercept - 1
  }

  intercept_list[[rv]] <- data.frame(
    cow       = cow_ids,
    variable  = rv,
    intercept = as.numeric(ind_intercept),
    stringsAsFactors = FALSE
  )
}

intercepts_long <- bind_rows(intercept_list)
intercepts_wide <- intercepts_long %>%
  pivot_wider(names_from = variable, values_from = intercept,
              names_prefix = "int_")

write.csv(intercepts_long,
          file.path(output_dir, "cow_intercepts.csv"),
          row.names = FALSE)
write.csv(intercepts_wide,
          file.path(output_dir, "cow_intercepts_wide.csv"),
          row.names = FALSE)

cat("  Saved", nrow(intercepts_wide), "cows x",
    length(response_vars), "intercept variables\n")

###################################################################################################
################################## [2] Extract individual slopes (within-ind variation) ############
###################################################################################################
# For each cow, the slope = exp(population sigma intercept + cow sigma random effect).
# This is the individual's residual SD — mirrors the x-axis of IIV ridge plots in 12b.

cat("\n========== Extracting individual slopes (IIV) ==========\n")

slope_list <- list()

for (rv in response_vars) {
  rds_path <- file.path("results/12_predictability", paste0("m2_brm_", rv, ".rds"))
  if (!file.exists(rds_path)) {
    message("Skipping ", rv, " (m2 not found)")
    next
  }
  cat("  Slope:", rv, "\n")
  m <- readRDS(rds_path)

  ps       <- as_draws_df(m)
  iiv_cols <- grep("^r_cow__sigma\\[", names(ps), value = TRUE)

  # Population sigma intercept (posterior mean)
  pop_sigma_int <- mean(ps$b_sigma_Intercept)

  # Per-cow posterior means of the sigma random effect
  cow_re <- colMeans(ps[, iiv_cols, drop = FALSE])

  cow_ids <- sub("^r_cow__sigma\\[(.+),Intercept\\]$", "\\1", names(cow_re))

  # Individual residual SD on original scale = exp(link-scale value)
  ind_sigma <- exp(pop_sigma_int + cow_re)

  slope_list[[rv]] <- data.frame(
    cow      = cow_ids,
    variable = rv,
    slope    = as.numeric(ind_sigma),
    stringsAsFactors = FALSE
  )
}

slopes_long <- bind_rows(slope_list)
slopes_wide <- slopes_long %>%
  pivot_wider(names_from = variable, values_from = slope,
              names_prefix = "slp_")

write.csv(slopes_long,
          file.path(output_dir, "cow_slopes.csv"),
          row.names = FALSE)
write.csv(slopes_wide,
          file.path(output_dir, "cow_slopes_wide.csv"),
          row.names = FALSE)

cat("  Saved", nrow(slopes_wide), "cows x",
    length(response_vars), "slope variables\n")

###################################################################################################
################################## Merge intercepts + slopes into one wide table ##################
###################################################################################################
cow_traits <- intercepts_wide %>%
  inner_join(slopes_wide, by = "cow")

write.csv(cow_traits,
          file.path(output_dir, "cow_traits_wide.csv"),
          row.names = FALSE)

cat("\n  Combined traits table:", nrow(cow_traits), "cows x",
    ncol(cow_traits) - 1, "trait columns\n")

###################################################################################################
################################## [3] K-means clustering of behaviours ###########################
###################################################################################################
# Cluster the 16 behaviours into 3 groups based on herd-level CVi and R (repeatability).
#   Cluster 1: high CVi + high R
#   Cluster 2: low CVi  + high R
#   Cluster 3: low CVi  + low R

rep_summary <- read.csv("results/11_repeatability/repeatability_summary.csv",
                        stringsAsFactors = FALSE)

# Use CVi_mean and R_cow_mean for clustering
clust_data <- rep_summary %>%
  select(variable, R_cow_mean, CVi_mean)

# Scale features before K-means
clust_scaled <- scale(clust_data[, c("R_cow_mean", "CVi_mean")])

set.seed(42)
km <- kmeans(clust_scaled, centers = 3, nstart = 50, iter.max = 100)

clust_data$cluster_raw <- km$cluster

# Relabel clusters so that:
#   Cluster 1 = highest CVi + highest R  (high CVi, high R)
#   Cluster 2 = high R + low CVi
#   Cluster 3 = low R + low CVi
# Strategy: rank clusters by R mean (descending), then by CVi mean (descending).
# Cluster 1 (high R, high CVi) naturally ranks first; among the low-CVi pair,
# cluster 2 gets high R and cluster 3 gets low R.
cluster_profiles <- clust_data %>%
  group_by(cluster_raw) %>%
  summarise(mean_R   = mean(R_cow_mean),
            mean_CVi = mean(CVi_mean),
            .groups  = "drop") %>%
  # Primary sort: R descending, secondary: CVi descending
  arrange(desc(mean_R), desc(mean_CVi)) %>%
  mutate(new_label = 1:3)

relabel_map <- setNames(cluster_profiles$new_label, cluster_profiles$cluster_raw)
clust_data$cluster <- relabel_map[as.character(clust_data$cluster_raw)]

# Verify cluster assignment logic
cat("\n========== Behaviour Clusters ==========\n")
for (cl in 1:3) {
  vars_in <- clust_data$variable[clust_data$cluster == cl]
  profile <- clust_data %>% filter(cluster == cl) %>%
    summarise(mean_R = round(mean(R_cow_mean), 3),
              mean_CVi = round(mean(CVi_mean), 3))
  cat(sprintf("Cluster %d (mean R=%.3f, mean CVi=%.3f): %s\n",
              cl, profile$mean_R, profile$mean_CVi,
              paste(vars_in, collapse = ", ")))
}

behaviour_clusters <- clust_data %>%
  select(variable, R_cow_mean, CVi_mean, cluster) %>%
  arrange(cluster, desc(CVi_mean))

write.csv(behaviour_clusters,
          file.path(output_dir, "behaviour_clusters.csv"),
          row.names = FALSE)

###################################################################################################
################################## Canonical variable labels (shared with paper) #################
###################################################################################################
var_labels <- c(
  "feed_intake"                    = "Daily feed intake",
  "feed_duration"                  = "Daily feeding duration",
  "feed_visits"                    = "Daily feeding visits",
  "water_intake"                   = "Daily water intake",
  "water_duration"                 = "Daily drinking duration",
  "water_visits"                   = "Daily drinking visits",
  "total_actor"                    = "Daily actor events",
  "total_reactor"                  = "Daily reactor events",
  "number_of_non_nutritive_visits" = "Daily non-nutritive visits",
  "median_pct_feed_remaining"      = "Median % of feed remaining",
  "total_meals"                    = "Daily meals",
  "median_meal_duration"           = "Median meal duration",
  "median_visit_per_meal"          = "Median visits per meal",
  "median_intake_per_meal"         = "Median intake per meal",
  "median_feeding_pct_per_meal"    = "Median % of time spent feeding per meal",
  "median_non_nutritive_per_meal"  = "Median non-nutritive per meal"
)
label_var <- function(v) {
  ifelse(v %in% names(var_labels), var_labels[v], gsub("_", " ", v))
}

###################################################################################################
################################## Visualise behaviour clusters ###################################
###################################################################################################
behaviour_clusters$label <- label_var(behaviour_clusters$variable)
behaviour_clusters$cluster_label <- paste("Cluster", behaviour_clusters$cluster)

cluster_colours <- c("Cluster 1" = "#F28E2B",
                     "Cluster 2" = "#909E03",
                     "Cluster 3" = "#D4A6C8")

cluster_plot <- ggplot(behaviour_clusters,
                       aes(x = R_cow_mean, y = CVi_mean,
                           colour = cluster_label, fill = cluster_label,
                           shape = cluster_label)) +
  geom_point(size = 3.5) +
  ggrepel::geom_label_repel(
    aes(label = label, fill = cluster_label),
    colour             = "black",
    size               = 6,
    alpha              = 0.5,
    label.padding      = unit(0.2, "lines"),
    box.padding        = unit(0.8, "lines"),
    point.padding      = unit(0.5, "lines"),
    force              = 20,
    force_pull         = 0.5,
    direction          = "both",
    min.segment.length = 0,
    segment.size       = 0.3,
    max.overlaps       = Inf,
    show.legend        = FALSE
  ) +
  scale_colour_manual(values = cluster_colours) +
  scale_fill_manual(values = cluster_colours) +
  scale_shape_manual(values = c("Cluster 1" = 16,
                                "Cluster 2" = 17,
                                "Cluster 3" = 15)) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  labs(x      = "Repeatability (R)",
       y      = "CVi (between-individual variation)",
       colour = "Cluster",
       shape  = "Cluster") +
  theme_classic(base_size = 25) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    axis.title       = element_text(size = 24),
    axis.text        = element_text(size = 20)
  ) +
  guides(fill = "none")

ggsave(file.path(output_dir, "behaviour_cluster_plot.png"),
       cluster_plot, width = 9, height = 7)

###################################################################################################
################################## Cluster-coloured ellipse scatters ##############################
###################################################################################################
# Re-draw the R-vs-CVi ellipse scatter (from 11e) and the CVP-vs-CVi ellipse scatter
# (from 12b) using the 3-cluster colour scheme defined above so the clustering result
# can be read directly off the same figures. Saved to this step's output directory
# rather than overwriting 11e / 12b output, so the original per-variable colour
# versions remain available.

library(ggforce)

# Helper: map each variable to its cluster label.
cluster_map <- behaviour_clusters %>%
  dplyr::select(variable, cluster_label)

# ---- R vs CVi ellipse scatter, coloured by cluster ------------------------------------------
rep_summary_c <- rep_summary %>%
  dplyr::left_join(cluster_map, by = "variable") %>%
  dplyr::mutate(
    a     = (R_cow_upper - R_cow_lower) / 2,
    b     = (CVi_upper   - CVi_lower)   / 2,
    label = label_var(variable)
  )

axis_lim <- c(0, 0.8)

rep_scatter_c <- ggplot(rep_summary_c,
                        aes(x0 = R_cow_mean, y0 = CVi_mean,
                            a = a, b = b, angle = 0,
                            fill = cluster_label, colour = cluster_label)) +
  ggforce::geom_ellipse(alpha = 0.35, linewidth = 0.4) +
  geom_point(aes(x = R_cow_mean, y = CVi_mean,
                 colour = cluster_label, shape = family),
             size = 2.5, show.legend = TRUE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = R_cow_mean, y = CVi_mean, label = label, fill = cluster_label),
    colour             = "black",
    size               = 8,
    alpha              = 0.5,
    label.padding      = unit(0.15, "lines"),
    box.padding        = unit(0.5, "lines"),
    point.padding      = unit(0.3, "lines"),
    force              = 50,
    force_pull         = 0.3,
    max.iter           = 20000,
    direction          = "both",
    min.segment.length = 0,
    segment.size       = 0.3,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    inherit.aes        = FALSE
  ) +
  scale_x_continuous(limits = axis_lim, expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = axis_lim, expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_manual(values   = cluster_colours, name = "Cluster") +
  scale_colour_manual(values = cluster_colours, name = "Cluster") +
  scale_shape_manual(values = c("gaussian" = 16, "lognormal" = 17)) +
  labs(
    x     = "Repeatability (R) \n proportion of variance due to individual variation",
    y     = "Coefficient of variation (CVi) \n relative magnitude of individual variation",
    shape = "Likelihood family"
  ) +
  theme_classic(base_size = 30) +
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.text      = element_text(size = 25),
    legend.title     = element_text(size = 28),
    axis.title       = element_text(size = 29),
    axis.text        = element_text(size = 25)
  ) +
  guides(fill = "none")

ggsave(file.path(output_dir, "repeatability_ellipse_scatter_by_cluster.png"),
       rep_scatter_c, width = 12, height = 9)

# ---- CVP vs CVi ellipse scatter, coloured by cluster -----------------------------------------
pred_summary <- read.csv("results/12_predictability/predictability_summary.csv",
                         stringsAsFactors = FALSE)

combined_c <- rep_summary %>%
  dplyr::select(variable, family, CVi_mean, CVi_lower, CVi_upper) %>%
  dplyr::inner_join(
    pred_summary %>% dplyr::select(variable, CVP_mean, CVP_lower, CVP_upper),
    by = "variable"
  ) %>%
  dplyr::left_join(cluster_map, by = "variable") %>%
  dplyr::mutate(
    a     = (CVP_upper - CVP_lower) / 2,
    b     = (CVi_upper - CVi_lower) / 2,
    label = label_var(variable)
  )

cvp_cvi_c <- ggplot(combined_c,
                    aes(x0 = CVP_mean, y0 = CVi_mean,
                        a = a, b = b, angle = 0,
                        fill = cluster_label, colour = cluster_label)) +
  ggforce::geom_ellipse(alpha = 0.35, linewidth = 0.4) +
  geom_point(aes(x = CVP_mean, y = CVi_mean,
                 colour = cluster_label, shape = family),
             size = 2.5, show.legend = TRUE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = CVP_mean, y = CVi_mean, label = label, fill = cluster_label),
    colour             = "black",
    size               = 8,
    alpha              = 0.5,
    label.padding      = unit(0.15, "lines"),
    box.padding        = unit(0.5, "lines"),
    point.padding      = unit(0.3, "lines"),
    force              = 50,
    force_pull         = 0.3,
    max.iter           = 20000,
    direction          = "both",
    min.segment.length = 0,
    segment.size       = 0.3,
    max.overlaps       = Inf,
    show.legend        = FALSE,
    inherit.aes        = FALSE
  ) +
  scale_x_continuous(limits = axis_lim, expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = axis_lim, expand = expansion(mult = c(0.02, 0.02))) +
  scale_fill_manual(values   = cluster_colours, name = "Cluster") +
  scale_colour_manual(values = cluster_colours, name = "Cluster") +
  scale_shape_manual(values = c("gaussian" = 16, "lognormal" = 17)) +
  labs(
    x     = "Coefficient of variation in predictability (CVP) \n within-individual variation",
    y     = "Coefficient of variation (CVi) \n relative magnitude of among-individual variation",
    shape = "Likelihood family"
  ) +
  theme_classic(base_size = 30) +
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.text      = element_text(size = 25),
    legend.title     = element_text(size = 28),
    axis.title       = element_text(size = 29),
    axis.text        = element_text(size = 25)
  ) +
  guides(fill = "none")

ggsave(file.path(output_dir, "scatter_CVP_vs_CVi_by_cluster.png"),
       cvp_cvi_c, width = 12, height = 9)

###################################################################################################
################################## Combined A/B grid plot #########################################
###################################################################################################
library(patchwork)

rep_scatter_A <- rep_scatter_c +
  theme(legend.position = "none") +
  labs(tag = "A")

cvp_cvi_B <- cvp_cvi_c +
  theme(
    legend.position  = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box       = "vertical",
    legend.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.3)
  ) +
  labs(tag = "B")

grid_plot <- rep_scatter_A / cvp_cvi_B +
  plot_layout(ncol = 1)

ggsave(file.path(output_dir, "ellipse_scatter_grid_AB.png"),
       grid_plot, width = 14, height = 24)

cat("\nDone. Outputs saved to:", output_dir, "\n")
