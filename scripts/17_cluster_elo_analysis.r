###################################################################################################
################################## 17. Cluster × Elo Analysis ####################################
###################################################################################################
# Prerequisites: run script 15 first (or ensure its output CSVs exist).
#                run script 11a first (or ensure master_data.rda exists).
#
# Merges M3 PCA cluster assignments (one row per cow) with each cow's median Elo score
# derived from master_data (cow × day panel). Produces:
#   - results/17_cluster_elo/cluster_elo.csv        (merged table)
#   - results/17_cluster_elo/elo_by_cluster.csv     (cluster-level summary statistics)
#   - results/17_cluster_elo/elo_by_cluster_box.png (boxplot: Elo distribution per cluster)
#   - results/17_cluster_elo/elo_by_cluster_dot.png (dot plot: individual cow Elo per cluster)

library(tidyverse)
library(ggplot2)
library(ggrepel)

output_dir <- "results/17_cluster_elo"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

sunset_palette <- c("#F8A07E", "#EB6770", "#A059A0", "#3D4D8A")

###################################################################################################
################################## Load data ######################################################
###################################################################################################

# M3 cluster assignments (one row per cow)
cow_clusters <- read.csv(
  "results/15_pca_clustering/pca_M3_c1_int_cow_clusters.csv",
  stringsAsFactors = FALSE
)
cow_clusters$cow <- as.integer(cow_clusters$cow)

# master_data: daily panel with Elo scores
load("results/11_repeatability/master_data.rda")

###################################################################################################
################################## Compute per-cow Elo summary ####################################
###################################################################################################
# Elo scores vary day-to-day; summarise to a single value per cow.
cow_elo <- master_data %>%
  group_by(cow) %>%
  summarise(
    elo_median = median(Elo, na.rm = TRUE),
    elo_mean   = mean(Elo,   na.rm = TRUE),
    elo_sd     = sd(Elo,     na.rm = TRUE),
    n_days     = sum(!is.na(Elo)),
    .groups    = "drop"
  )

###################################################################################################
################################## Merge clusters + Elo ##########################################
###################################################################################################
cluster_elo <- cow_clusters %>%
  left_join(cow_elo, by = "cow")

n_missing <- sum(is.na(cluster_elo$elo_median))
if (n_missing > 0) {
  warning(n_missing, " cow(s) in cluster file had no Elo data in master_data and will be excluded.")
  cluster_elo <- cluster_elo %>% filter(!is.na(elo_median))
}

cluster_elo$cluster <- factor(cluster_elo$cluster)

write.csv(cluster_elo,
          file.path(output_dir, "cluster_elo.csv"),
          row.names = FALSE)

cat("Merged dataset: ", nrow(cluster_elo), "cows across",
    nlevels(cluster_elo$cluster), "clusters\n")
print(cluster_elo)

###################################################################################################
################################## Cluster-level summary ##########################################
###################################################################################################
cluster_summary <- cluster_elo %>%
  group_by(cluster) %>%
  summarise(
    n            = n(),
    elo_mean     = round(mean(elo_median),   2),
    elo_sd       = round(sd(elo_median),     2),
    elo_median   = round(median(elo_median), 2),
    elo_min      = round(min(elo_median),    2),
    elo_max      = round(max(elo_median),    2),
    .groups      = "drop"
  )

cat("\n--- Cluster-level Elo summary (based on per-cow median Elo) ---\n")
print(cluster_summary)

write.csv(cluster_summary,
          file.path(output_dir, "elo_by_cluster.csv"),
          row.names = FALSE)

###################################################################################################
################################## Boxplot ########################################################
###################################################################################################
n_clusters <- nlevels(cluster_elo$cluster)
clust_cols <- colorRampPalette(sunset_palette)(n_clusters)

p_box <- ggplot(cluster_elo, aes(x = cluster, y = elo_median, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(colour = cluster), width = 0.15, size = 2.5, alpha = 0.8) +
  scale_fill_manual(values = clust_cols)   +
  scale_colour_manual(values = clust_cols) +
  labs(
    x    = "PCA Cluster (M3)",
    y    = "Median Elo score",
    fill = "Cluster", colour = "Cluster"
  ) +
  theme_classic(base_size = 20) +
  theme(
    legend.position = "none",
    axis.title      = element_text(size = 22),
    axis.text       = element_text(size = 18)
  )

ggsave(file.path(output_dir, "elo_by_cluster_box.png"),
       p_box, width = 8, height = 6)

###################################################################################################
################################## Dot plot with cow labels #######################################
###################################################################################################
p_dot <- ggplot(cluster_elo, aes(x = cluster, y = elo_median, colour = cluster)) +
  geom_point(size = 3, alpha = 0.8,
             position = position_jitter(width = 0.15, seed = 42)) +
  geom_text_repel(
    aes(label = cow),
    size            = 4,
    box.padding     = 0.3,
    point.padding   = 0.2,
    max.overlaps    = Inf,
    segment.size    = 0.3,
    position        = position_jitter(width = 0.15, seed = 42)
  ) +
  stat_summary(fun = median, geom = "crossbar",
               width = 0.4, colour = "grey30", linewidth = 0.6) +
  scale_colour_manual(values = clust_cols) +
  labs(
    x      = "PCA Cluster (M3)",
    y      = "Median Elo score",
    colour = "Cluster"
  ) +
  theme_classic(base_size = 20) +
  theme(
    legend.position = "none",
    axis.title      = element_text(size = 22),
    axis.text       = element_text(size = 18)
  )

ggsave(file.path(output_dir, "elo_by_cluster_dot.png"),
       p_dot, width = 9, height = 7)

cat("\nDone. Outputs saved to:", output_dir, "\n")
