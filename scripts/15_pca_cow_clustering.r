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
# Outputs under results/15_pcfa_clustering/:
#   - pca_method_<N>_summary.csv, pca_method_<N>_loadings.csv
#   - pca_method_<N>_screeplot.png, pca_method_<N>_biplot.png
#   - pca_method_<N>_cow_clusters.csv, pca_method_<N>_cluster_plot.png
#   - method_comparison.csv

set.seed(234)

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(psych)        # principal, fa.parallel (Horn's parallel analysis)
library(factoextra)   # fviz_nbclust, fviz_cluster

output_dir <- "results/15_pca_clustering"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

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
  # Strip int_/slp_ prefix produced by PCA column names, then look up canonical label
  v_clean <- sub("^(int_|slp_)", "", v)
  ifelse(v_clean %in% names(var_labels), var_labels[v_clean], gsub("_", " ", v_clean))
}

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

  # ---- Determine number of components (>= 80% cumulative variance) ----
  # First run unrotated PCA to get eigenvalues
  pca_all <- principal(X, nfactors = ncol(X), rotate = "none", scores = FALSE)
  all_var  <- pca_all$values / sum(pca_all$values)
  cum_all  <- cumsum(all_var)
  n_comp   <- which(cum_all >= 0.80)[1]
  if (is.na(n_comp) || n_comp < 1) n_comp <- 1
  cat("  Components to reach 80% variance:", n_comp, "\n")

  # ---- PCA via psych::principal with varimax rotation ----
  pca <- principal(X, nfactors = n_comp, rotate = "varimax", scores = TRUE)

  # Variance explained per component (from original eigenvalues)
  var_exp  <- pca$values[1:n_comp] / sum(pca_all$values)
  cum_var  <- cumsum(var_exp)

  pca_summary <- data.frame(
    RC              = paste0("RC", seq_len(n_comp)),
    eigenvalue      = round(pca$values[1:n_comp], 4),
    variance_pct    = round(var_exp * 100, 2),
    cumulative_pct  = round(cum_var * 100, 2)
  )

  cat("  Variance explained by retained components:", round(cum_var[n_comp] * 100, 1), "%\n")

  # Loadings matrix (varimax-rotated)
  load_mat <- as.data.frame(unclass(pca$loadings))
  loadings_df <- load_mat %>% rownames_to_column("variable")

  # Print top loadings per component
  for (rc in seq_len(min(n_comp, 6))) {
    rc_name <- colnames(load_mat)[rc]
    cat(sprintf("\n  Top loadings (%s):\n", rc_name))
    top <- loadings_df %>% arrange(desc(abs(.data[[rc_name]]))) %>% head(8)
    for (i in seq_len(nrow(top))) {
      cat(sprintf("    %-40s  %+.3f\n", top$variable[i], top[[rc_name]][i]))
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
  all_eig <- data.frame(
    component  = seq_along(pca_all$values),
    eigenvalue = pca_all$values
  )
  scree <- ggplot(all_eig, aes(x = component, y = eigenvalue)) +
    geom_line(colour = "steelblue", linewidth = 1) +
    geom_point(colour = "steelblue", size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept = n_comp + 0.5, linetype = "dotted", colour = "grey40") +
    labs(title = paste("Scree plot —", method$name),
         x = "Component", y = "Eigenvalue") +
    theme_classic()
  ggsave(file.path(output_dir, paste0("pca_", method$name, "_screeplot.png")),
         scree, width = 8, height = 5)

  # ---- Determine optimal K ----
  scores <- pca$scores
  scores_mat <- as.data.frame(scores)
  k_max <- min(10, nrow(scores) - 1)

  # Silhouette method for optimal K (with nstart/iter.max for stable results)
  sil_plot <- fviz_nbclust(scores_mat, kmeans, method = "silhouette",
                           k.max = k_max, nstart = 100, iter.max = 200) +
    ggtitle(paste("Optimal K (silhouette) —", method$name))
  ggsave(file.path(output_dir, paste0("pca_", method$name, "_silhouette.png")),
         sil_plot, width = 7, height = 5)

  # Extract optimal K from single-seed silhouette
  sil_data <- sil_plot$data
  sil_k <- as.integer(as.character(sil_data$clusters[which.max(sil_data$y)]))
  cat("  Optimal K (single-seed silhouette):", sil_k, "\n")

  # Multi-seed stability check: run silhouette across 100 seeds to find most stable K
  n_seeds <- 100
  k_range <- 2:k_max
  best_k_per_seed <- integer(n_seeds)
  d <- dist(scores_mat)  # compute once

  for (s in seq_len(n_seeds)) {
    set.seed(s)
    avg_sil <- sapply(k_range, function(k) {
      km_tmp <- kmeans(scores_mat, centers = k, nstart = 100, iter.max = 200)
      mean(cluster::silhouette(km_tmp$cluster, d)[, "sil_width"])
    })
    best_k_per_seed[s] <- k_range[which.max(avg_sil)]
  }

  freq <- table(best_k_per_seed)
  cat("  K stability across", n_seeds, "seeds:\n")
  print(freq)
  optimal_k <- as.integer(names(which.max(freq)))
  cat("  Most stable optimal K:", optimal_k, "\n")

  # Save stability results
  stability_df <- data.frame(k = as.integer(names(freq)),
                             wins = as.integer(freq),
                             pct  = round(as.integer(freq) / n_seeds * 100, 1))
  write.csv(stability_df,
            file.path(output_dir, paste0("pca_", method$name, "_k_stability.csv")),
            row.names = FALSE)

  # ---- K-means clustering ----
  set.seed(100)  # restore seed for reproducible final clustering
  km <- kmeans(scores_mat, centers = optimal_k, nstart = 100, iter.max = 200)

  cow_clusters <- data.frame(
    cow     = cow_ids,
    cluster = km$cluster,
    stringsAsFactors = FALSE
  )

  # Attach component scores
  scores_mat$cow <- cow_ids
  cow_clusters <- cow_clusters %>% left_join(scores_mat, by = "cow")

  write.csv(cow_clusters,
            file.path(output_dir, paste0("pca_", method$name, "_cow_clusters.csv")),
            row.names = FALSE)

  # ---- Cluster quality metrics ----
  avg_sil <- cluster::silhouette(km$cluster, dist(scores))
  mean_sil <- mean(avg_sil[, "sil_width"])
  bss_tss  <- km$betweenss / km$totss

  cat(sprintf("  Mean silhouette: %.3f\n", mean_sil))
  cat(sprintf("  BSS/TSS ratio:   %.3f\n", bss_tss))
  cat(sprintf("  Cluster sizes:   %s\n", paste(km$size, collapse = ", ")))

  list(
    method        = method$name,
    n_vars        = length(method$cols),
    n_comp        = n_comp,
    cum_var_pct   = round(cum_var[n_comp] * 100, 1),
    optimal_k     = optimal_k,
    mean_sil      = round(mean_sil, 4),
    bss_tss       = round(bss_tss, 4),
    cluster_sizes = paste(sort(km$size), collapse = "/"),
    var_rc1_pct   = round(var_exp[1] * 100, 1),
    var_rc2_pct   = if (n_comp >= 2) round(var_exp[2] * 100, 1) else NA,
    pca_obj       = pca,
    km_obj        = km
  )
}

###################################################################################################
################################## Run all 6 methods ##############################################
###################################################################################################
results <- list()
for (i in seq_along(methods)) {
  results[[i]] <- run_pca_cluster(methods[[i]], cow_traits, output_dir)
}

###################################################################################################
################################## Method comparison table #########################################
###################################################################################################
metric_fields <- c("method", "n_vars", "n_comp", "cum_var_pct", "optimal_k",
                    "mean_sil", "bss_tss", "cluster_sizes", "var_rc1_pct", "var_rc2_pct")
comp_df <- bind_rows(lapply(results, function(x) x[metric_fields]))

cat("\n\n", strrep("=", 90), "\n")
cat("METHOD COMPARISON\n")
cat(strrep("=", 90), "\n\n")
print(as.data.frame(comp_df), right = FALSE, row.names = FALSE)

cat("\n--- Interpretation Guide ---\n")
cat("  mean_sil:    Average silhouette width (higher = better-defined clusters, >0.5 strong)\n")
cat("  bss_tss:     Between-SS / Total-SS ratio (higher = more separation, >0.5 good)\n")
cat("  var_rc1/2:   Variance explained by RC1 and RC2 (higher = more structure captured)\n")
cat("  n_comp:      Number of components retained (>= 80% cumulative variance)\n")
cat("  cum_var_pct: Cumulative variance explained by retained components\n")
cat("  optimal_k:   Optimal number of clusters via silhouette method\n")

# Rank methods
comp_df <- comp_df %>%
  mutate(
    rank_sil    = rank(-mean_sil),
    rank_bss    = rank(-bss_tss),
    rank_pars   = rank(n_comp),      # fewer components = more parsimonious
    avg_rank    = (rank_sil + rank_bss + rank_pars) / 3
  ) %>%
  arrange(avg_rank)

cat("\n--- Ranking (lower avg_rank = better) ---\n")
print(as.data.frame(comp_df %>% select(method, mean_sil, bss_tss, n_comp, cum_var_pct,
                                        rank_sil, rank_bss, rank_pars, avg_rank)),
      right = FALSE, row.names = FALSE)

best <- comp_df$method[1]
cat("\n  Best method:", best, "\n")
cat("    Criteria: highest silhouette width (cluster definition),\n")
cat("              highest BSS/TSS (cluster separation),\n")
cat("              fewest components from parallel analysis (parsimony).\n")

write.csv(comp_df,
          file.path(output_dir, "method_comparison.csv"),
          row.names = FALSE)

###################################################################################################
################################## Biplots for best method only ####################################
###################################################################################################
best_idx <- which(sapply(results, function(x) x$method) == best)
best_res <- results[[best_idx]]

cat("\nDrawing biplots for best method:", best, "\n")

sunset_palette <- c("#EE8866", "#AAAA00", "#EBAF02", "#77AADD")

# Compute a single global axis limit and scaling factor across ALL retained RCs
# so that every biplot panel shares the same coordinate space.
{
  sc_all      <- best_res$pca_obj$scores[, seq_len(best_res$n_comp)]
  global_lim  <- c(-1, 1) * (max(abs(sc_all)) * 1.12)
  global_sf   <- 0.8 * max(abs(sc_all))
}

draw_biplot <- function(pca_obj, km_obj, optimal_k, method_name, idx_x, idx_y,
                        suffix, global_lim, global_sf) {
  sc_mat <- pca_obj$scores
  ld_mat <- unclass(pca_obj$loadings)
  rc_x   <- colnames(sc_mat)[idx_x]
  rc_y   <- colnames(sc_mat)[idx_y]

  plot_df <- data.frame(
    dim1    = sc_mat[, idx_x],
    dim2    = sc_mat[, idx_y],
    cluster = factor(km_obj$cluster)
  )

  load2 <- data.frame(
    ld1      = ld_mat[, idx_x],
    ld2      = ld_mat[, idx_y],
    variable = label_var(rownames(ld_mat))
  )
  load2$xend <- load2$ld1 * global_sf
  load2$yend <- load2$ld2 * global_sf

  clust_cols <- colorRampPalette(sunset_palette)(optimal_k)

  p <- ggplot(plot_df, aes(x = dim1, y = dim2, colour = cluster, fill = cluster)) +
    stat_ellipse(geom = "polygon", level = 0.95, alpha = 0.10,
                 linewidth = 0.4, linetype = "solid") +
    geom_point(size = 3, alpha = 0.7) +
    geom_segment(data = load2, inherit.aes = FALSE,
                 aes(x = 0, y = 0, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.25, "cm")),
                 colour = "sienna", linewidth = 0.6) +
    ggrepel::geom_text_repel(
      data            = load2,
      inherit.aes     = FALSE,
      aes(x = xend, y = yend, label = variable),
      colour          = "sienna",
      fontface        = "bold",
      size            = 10,
      box.padding     = unit(0.5, "lines"),
      point.padding   = unit(0.3, "lines"),
      force           = 50,
      force_pull      = 0.3,
      max.iter        = 20000,
      direction       = "both",
      min.segment.length = 0,
      segment.size    = 0.3,
      segment.colour  = "sienna",
      max.overlaps    = Inf
    ) +
    scale_colour_manual(values = clust_cols) +
    scale_fill_manual(values   = clust_cols) +
    coord_cartesian(xlim = global_lim, ylim = global_lim) +
    labs(
      x      = rc_x,
      y      = rc_y,
      colour = "Cluster",
      fill   = "Cluster"
    ) +
    theme_classic(base_size = 32) +
    theme(
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.text      = element_text(size = 27),
      legend.title     = element_text(size = 30),
      axis.title       = element_text(size = 31),
      axis.text        = element_text(size = 27)
    )
  p
}

library(patchwork)

if (best_res$n_comp >= 2) {
  p_rc1_rc2 <- draw_biplot(best_res$pca_obj, best_res$km_obj, best_res$optimal_k, best,
                            1, 2, "rc1_rc2", global_lim, global_sf)
}
if (best_res$n_comp >= 3) {
  p_rc1_rc3 <- draw_biplot(best_res$pca_obj, best_res$km_obj, best_res$optimal_k, best,
                            1, 3, "rc1_rc3", global_lim, global_sf)
  p_rc2_rc3 <- draw_biplot(best_res$pca_obj, best_res$km_obj, best_res$optimal_k, best,
                            2, 3, "rc2_rc3", global_lim, global_sf)
}

# Save individual panels (for reference / supplementary use)
ggsave(file.path(output_dir, paste0("pca_", best, "_biplot_clusters_rc1_rc2.png")),
       p_rc1_rc2, width = 10, height = 9)
ggsave(file.path(output_dir, paste0("pca_", best, "_biplot_clusters_rc1_rc3.png")),
       p_rc1_rc3, width = 10, height = 9)
ggsave(file.path(output_dir, paste0("pca_", best, "_biplot_clusters_rc2_rc3.png")),
       p_rc2_rc3, width = 10, height = 9)

# Combined A/B panel (RC1v2 and RC1v3) with shared legend
biplot_A <- p_rc1_rc2 +
  theme(legend.position = "none") +
  labs(tag = "A")

biplot_B <- p_rc1_rc3 +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal"
  ) +
  labs(tag = "B")

biplot_AB <- biplot_A / biplot_B +
  plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, paste0("pca_", best, "_biplot_clusters_AB.png")),
       biplot_AB, width = 12, height = 22)

cat("\nDone. Outputs saved to:", output_dir, "\n")
