###################################################################################################
################################## 16. Interactive 3D Cluster Plot ################################
###################################################################################################
# Prerequisites: run script 15 first (or ensure its output CSVs exist).
#
# Creates an interactive 3D scatter plot of cow clusters from PCA Method 3
# (Cluster-1 intercepts only) using PC1, PC2, PC3.
# Each cow is labelled by name, and clusters are shown in distinct colours.
#
# Output: results/16_3d_cluster_plot/M3_3d_cluster_plot.html

library(plotly)
library(dplyr)

output_dir <- "results/16_3d_cluster_plot"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Load M3 clustering results #####################################
###################################################################################################
cow_clusters <- read.csv("results/15_pca_clustering/pca_M3_c1_int_cow_clusters.csv",
                         stringsAsFactors = FALSE)
pca_summary  <- read.csv("results/15_pca_clustering/pca_M3_c1_int_summary.csv",
                         stringsAsFactors = FALSE)

# Variance explained for axis labels
var_pc1 <- pca_summary$variance_pct[1]
var_pc2 <- pca_summary$variance_pct[2]
var_pc3 <- pca_summary$variance_pct[3]

cow_clusters$cluster <- factor(cow_clusters$cluster)

cat("Number of cows:", nrow(cow_clusters), "\n")
cat("Clusters found:", nlevels(cow_clusters$cluster), "\n")
cat("Cluster sizes:\n")
print(table(cow_clusters$cluster))

###################################################################################################
################################## Interactive 3D plot ############################################
###################################################################################################
# Distinct colour palette
cluster_colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                     "#FF7F00", "#A65628", "#F781BF", "#999999")

n_clust <- nlevels(cow_clusters$cluster)
colours_used <- cluster_colours[seq_len(n_clust)]

fig <- plot_ly(
  data   = cow_clusters,
  x      = ~PC1,
  y      = ~PC2,
  z      = ~PC3,
  color  = ~cluster,
  colors = colours_used,
  type   = "scatter3d",
  mode   = "markers+text",
  marker = list(size = 7, opacity = 0.85,
                line = list(width = 1, color = "black")),
  text   = ~cow,
  textposition = "top center",
  textfont     = list(size = 10),
  hoverinfo    = "text",
  hovertext    = ~paste0("Cow: ", cow,
                         "<br>Cluster: ", cluster,
                         "<br>PC1: ", round(PC1, 2),
                         "<br>PC2: ", round(PC2, 2),
                         "<br>PC3: ", round(PC3, 2))
) %>%
  layout(
    title = list(
      text = "Cow Clustering (Method 3: Cluster-1 Intercepts)",
      font = list(size = 16)
    ),
    scene = list(
      xaxis = list(title = paste0("PC1 (", var_pc1, "%)")),
      yaxis = list(title = paste0("PC2 (", var_pc2, "%)")),
      zaxis = list(title = paste0("PC3 (", var_pc3, "%)"))
    ),
    legend = list(title = list(text = "Cluster"))
  )

# Save as interactive HTML
html_path <- normalizePath(file.path(output_dir, "M3_3d_cluster_plot.html"), mustWork = FALSE)
htmlwidgets::saveWidget(fig, file = html_path, selfcontained = FALSE)

cat("\nInteractive 3D plot saved to:", html_path, "\n")
cat("Open the HTML file in a browser to explore the plot.\n")
