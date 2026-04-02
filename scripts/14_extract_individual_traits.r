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
  "median_non_nutritive_per_meal", "total_actor", "total_reactor"
)

# Variables that had a +1 offset applied in 11a (lognormal, can be zero)
offset_vars <- c("median_non_nutritive_per_meal", "total_actor", "total_reactor")

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
# Strategy: rank clusters by CVi mean (descending), then by R mean (descending)
cluster_profiles <- clust_data %>%
  group_by(cluster_raw) %>%
  summarise(mean_R   = mean(R_cow_mean),
            mean_CVi = mean(CVi_mean),
            .groups  = "drop") %>%
  # Primary sort: CVi descending, secondary: R descending
  arrange(desc(mean_CVi), desc(mean_R)) %>%
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
################################## Visualise behaviour clusters ###################################
###################################################################################################
behaviour_clusters$label <- gsub("_", " ", behaviour_clusters$variable)
behaviour_clusters$cluster_label <- paste("Cluster", behaviour_clusters$cluster)

cluster_colours <- c("Cluster 1" = "#E41A1C",
                     "Cluster 2" = "#377EB8",
                     "Cluster 3" = "#4DAF4A")

cluster_plot <- ggplot(behaviour_clusters,
                       aes(x = R_cow_mean, y = CVi_mean,
                           colour = cluster_label, fill = cluster_label,
                           shape = cluster_label)) +
  geom_point(size = 3.5) +
  ggrepel::geom_label_repel(
    aes(label = label, fill = cluster_label),
    colour             = "black",
    fontface           = "bold",
    size               = 4,
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
  theme_classic(base_size = 16) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    axis.title       = element_text(size = 18),
    axis.text        = element_text(size = 14)
  ) +
  guides(fill = "none")

ggsave(file.path(output_dir, "behaviour_cluster_plot.png"),
       cluster_plot, width = 9, height = 7)

cat("\nDone. Outputs saved to:", output_dir, "\n")
