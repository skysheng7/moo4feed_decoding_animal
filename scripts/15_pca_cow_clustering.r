###################################################################################################
################################## 15. PCA & Cow Clustering ########################################
###################################################################################################
# Prerequisites: run script 14 first (or ensure its output CSVs exist).
#
# Six PCA approaches:
#   Method 1: All 16 intercepts + 16 slopes  (32 variables)
#   Method 2: All 16 intercepts only          (16 variables)
#   Method 3: Cluster-1 intercepts only
#   Method 4: Cluster-1 + Cluster-2 intercepts only
#   Method 5: Cluster-1 intercepts + slopes
#   Method 6: Cluster-1 + Cluster-2 intercepts + slopes
#
# For each method: PCA, interpret loadings, cluster cows (K-means), compare.
#
# Outputs under results/15_pca_clustering/:
#   - pca_method_<N>_summary.csv, pca_method_<N>_loadings.csv
#   - pca_method_<N>_screeplot.png, pca_method_<N>_biplot.png
#   - pca_method_<N>_cow_clusters.csv, pca_method_<N>_cluster_plot.png
#   - method_comparison.csv

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(factoextra)   # fviz_screeplot, fviz_pca_biplot, fviz_cluster, fviz_nbclust

output_dir <- "results/15_pca_clustering"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Load data from script 14 #######################################
###################################################################################################
cow_traits   <- read.csv("results/14_individual_traits/cow_traits_wide.csv",
                         stringsAsFactors = FALSE)
behav_clust  <- read.csv("results/14_individual_traits/behaviour_clusters.csv",
                         stringsAsFactors = FALSE)

# Identify variables per cluster
vars_c1  <- behav_clust$variable[behav_clust$cluster == 1]
vars_c2  <- behav_clust$variable[behav_clust$cluster == 2]
vars_c12 <- c(vars_c1, vars_c2)

cat("Cluster 1 variables:", paste(vars_c1, collapse = ", "), "\n")
cat("Cluster 2 variables:", paste(vars_c2, collapse = ", "), "\n")
cat("Cluster 3 variables:",
    paste(behav_clust$variable[behav_clust$cluster == 3], collapse = ", "), "\n\n")

###################################################################################################
################################## Define the 6 methods ###########################################
###################################################################################################
all_vars <- behav_clust$variable  # all 16

build_method <- function(name, vars, use_intercept, use_slope) {
  cols <- character()
  if (use_intercept) cols <- c(cols, paste0("int_", vars))
  if (use_slope)     cols <- c(cols, paste0("slp_", vars))
  list(name = name, cols = cols, vars = vars,
       use_intercept = use_intercept, use_slope = use_slope)
}

methods <- list(
  build_method("M1_all_int_slp",     all_vars,  TRUE, TRUE),
  build_method("M2_all_int",         all_vars,  TRUE, FALSE),
  build_method("M3_c1_int",          vars_c1,   TRUE, FALSE),
  build_method("M4_c12_int",         vars_c12,  TRUE, FALSE),
  build_method("M5_c1_int_slp",      vars_c1,   TRUE, TRUE),
  build_method("M6_c12_int_slp",     vars_c12,  TRUE, TRUE)
)

###################################################################################################
################################## Helper: run PCA + clustering for one method #####################
###################################################################################################
run_pca_cluster <- function(method, cow_traits, output_dir) {
  cat("\n", strrep("=", 70), "\n")
  cat("Method:", method$name, " (", length(method$cols), "variables )\n")
  cat(strrep("=", 70), "\n")

  # Subset and remove rows with any NA
  mat <- cow_traits[, c("cow", method$cols)]
  mat <- mat[complete.cases(mat), ]
  cow_ids <- mat$cow
  X <- mat[, -1, drop = FALSE]

  # Scale (center + unit variance) — standard for PCA
  X_scaled <- scale(X)

  # ---- PCA ----
  pca <- prcomp(X_scaled, center = FALSE, scale. = FALSE)  # already scaled

  # Variance explained
  var_exp   <- pca$sdev^2 / sum(pca$sdev^2)
  cum_var   <- cumsum(var_exp)
  n_pc_70   <- which(cum_var >= 0.70)[1]
  n_pc_80   <- which(cum_var >= 0.80)[1]

  pca_summary <- data.frame(
    PC              = paste0("PC", seq_along(var_exp)),
    variance_pct    = round(var_exp * 100, 2),
    cumulative_pct  = round(cum_var * 100, 2)
  )

  cat("  PCs to reach 70%:", n_pc_70, "\n")
  cat("  PCs to reach 80%:", n_pc_80, "\n")

  # Loadings for first min(6, ncol) PCs
  n_show <- min(6, ncol(pca$rotation))
  loadings_df <- as.data.frame(pca$rotation[, 1:n_show]) %>%
    rownames_to_column("variable")

  cat("\n  Top loadings (PC1):\n")
  top_pc1 <- loadings_df %>% arrange(desc(abs(PC1))) %>% head(8)
  for (i in seq_len(nrow(top_pc1))) {
    cat(sprintf("    %-40s  %+.3f\n", top_pc1$variable[i], top_pc1$PC1[i]))
  }

  if (n_show >= 2) {
    cat("\n  Top loadings (PC2):\n")
    top_pc2 <- loadings_df %>% arrange(desc(abs(PC2))) %>% head(8)
    for (i in seq_len(nrow(top_pc2))) {
      cat(sprintf("    %-40s  %+.3f\n", top_pc2$variable[i], top_pc2$PC2[i]))
    }
  }

  # Save PCA outputs
  write.csv(pca_summary,
            file.path(output_dir, paste0("pca_", method$name, "_summary.csv")),
            row.names = FALSE)
  write.csv(loadings_df,
            file.path(output_dir, paste0("pca_", method$name, "_loadings.csv")),
            row.names = FALSE)

  # Scree plot
  scree <- fviz_screeplot(pca, ncp = min(15, length(var_exp)),
                          addlabels = TRUE, barfill = "steelblue") +
    ggtitle(paste("Scree plot —", method$name))
  ggsave(file.path(output_dir, paste0("pca_", method$name, "_screeplot.png")),
         scree, width = 8, height = 5)

  # Biplot (PC1 vs PC2)
  biplot <- fviz_pca_biplot(pca, repel = TRUE, col.var = "contrib",
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            col.ind = "gray60", alpha.ind = 0.5,
                            label = "var", labelsize = 3) +
    ggtitle(paste("Biplot —", method$name))
  ggsave(file.path(output_dir, paste0("pca_", method$name, "_biplot.png")),
         biplot, width = 10, height = 8)

  # ---- Determine optimal K ----
  # Use PCs that explain >= 80% variance for clustering
  scores <- pca$x[, 1:n_pc_80, drop = FALSE]

  # Silhouette method for optimal K
  sil_plot <- fviz_nbclust(scores, kmeans, method = "silhouette",
                           k.max = min(10, nrow(scores) - 1)) +
    ggtitle(paste("Optimal K (silhouette) —", method$name))
  ggsave(file.path(output_dir, paste0("pca_", method$name, "_silhouette.png")),
         sil_plot, width = 7, height = 5)

  # Extract optimal K from silhouette
  sil_data <- sil_plot$data
  optimal_k <- as.integer(as.character(sil_data$clusters[which.max(sil_data$y)]))
  cat("  Optimal K (silhouette):", optimal_k, "\n")

  # ---- K-means clustering ----
  set.seed(42)
  km <- kmeans(scores, centers = optimal_k, nstart = 50, iter.max = 200)

  cow_clusters <- data.frame(
    cow     = cow_ids,
    cluster = km$cluster,
    stringsAsFactors = FALSE
  )

  # Attach PC scores
  pc_scores_df <- as.data.frame(scores)
  pc_scores_df$cow <- cow_ids
  cow_clusters <- cow_clusters %>% left_join(pc_scores_df, by = "cow")

  write.csv(cow_clusters,
            file.path(output_dir, paste0("pca_", method$name, "_cow_clusters.csv")),
            row.names = FALSE)

  # Cluster visualisation on PC1-PC2
  cluster_df <- data.frame(scores[, 1:min(2, ncol(scores))], cluster = factor(km$cluster))
  clust_plot <- fviz_cluster(km, data = scores[, 1:min(2, ncol(scores))],
                             geom = "point", ellipse.type = "convex",
                             palette = "jco", ggtheme = theme_classic()) +
    ggtitle(paste("Cow clusters —", method$name))
  ggsave(file.path(output_dir, paste0("pca_", method$name, "_cluster_plot.png")),
         clust_plot, width = 8, height = 6)

  # ---- Cluster quality metrics ----
  avg_sil <- cluster::silhouette(km$cluster, dist(scores))
  mean_sil <- mean(avg_sil[, "sil_width"])
  bss_tss  <- km$betweenss / km$totss

  cat(sprintf("  Mean silhouette: %.3f\n", mean_sil))
  cat(sprintf("  BSS/TSS ratio:   %.3f\n", bss_tss))
  cat(sprintf("  Cluster sizes:   %s\n", paste(km$size, collapse = ", ")))

  list(
    method       = method$name,
    n_vars       = length(method$cols),
    n_pc_80      = n_pc_80,
    optimal_k    = optimal_k,
    mean_sil     = round(mean_sil, 4),
    bss_tss      = round(bss_tss, 4),
    cluster_sizes = paste(sort(km$size), collapse = "/"),
    var_pc1_pct  = round(var_exp[1] * 100, 1),
    var_pc2_pct  = round(var_exp[2] * 100, 1)
  )
}

###################################################################################################
################################## Run all 6 methods ##############################################
###################################################################################################
comparison <- list()
for (i in seq_along(methods)) {
  comparison[[i]] <- run_pca_cluster(methods[[i]], cow_traits, output_dir)
}

###################################################################################################
################################## Method comparison table #########################################
###################################################################################################
comp_df <- bind_rows(comparison)

cat("\n\n", strrep("=", 90), "\n")
cat("METHOD COMPARISON\n")
cat(strrep("=", 90), "\n\n")
print(as.data.frame(comp_df), right = FALSE, row.names = FALSE)

cat("\n--- Interpretation Guide ---\n")
cat("  mean_sil:  Average silhouette width (higher = better-defined clusters, >0.5 strong)\n")
cat("  bss_tss:   Between-SS / Total-SS ratio (higher = more separation, >0.5 good)\n")
cat("  var_pc1/2: Variance explained by PC1 and PC2 (higher = more structure captured)\n")
cat("  n_pc_80:   Number of PCs to reach 80% variance (fewer = more parsimonious)\n")
cat("  optimal_k: Optimal number of clusters via silhouette method\n")

# Rank methods
comp_df <- comp_df %>%
  mutate(
    rank_sil    = rank(-mean_sil),
    rank_bss    = rank(-bss_tss),
    rank_pars   = rank(n_pc_80),     # fewer PCs = more parsimonious
    avg_rank    = (rank_sil + rank_bss + rank_pars) / 3
  ) %>%
  arrange(avg_rank)

cat("\n--- Ranking (lower avg_rank = better) ---\n")
print(as.data.frame(comp_df %>% select(method, mean_sil, bss_tss, n_pc_80,
                                        rank_sil, rank_bss, rank_pars, avg_rank)),
      right = FALSE, row.names = FALSE)

best <- comp_df$method[1]
cat("\n  Best method:", best, "\n")
cat("    Criteria: highest silhouette width (cluster definition),\n")
cat("              highest BSS/TSS (cluster separation),\n")
cat("              fewest PCs to 80% (parsimony / interpretability).\n")

write.csv(comp_df,
          file.path(output_dir, "method_comparison.csv"),
          row.names = FALSE)

cat("\nDone. Outputs saved to:", output_dir, "\n")
