###################################################################################################
################################## 16. Interactive 3D Cluster Plot ################################
###################################################################################################
# Prerequisites: run script 15 first (or ensure its output CSVs exist).
#
# Creates an interactive 3D scatter plot of cow clusters from PCA Method 3
# (Cluster-1 intercepts only) using RC1, RC2, RC3 (varimax-rotated components).
# Each cow is labelled by ID. Clusters use the sunset palette.
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
var_rc1 <- pca_summary$variance_pct[1]
var_rc2 <- pca_summary$variance_pct[2]
var_rc3 <- pca_summary$variance_pct[3]

# Component column names (may vary after varimax reordering)
rc_cols <- setdiff(names(cow_clusters), c("cow", "cluster"))
rc1_col <- rc_cols[1]
rc2_col <- rc_cols[2]
rc3_col <- rc_cols[3]

rc_to_full <- function(x) gsub("RC(\\d+)", "RC \\1", x)
rc1_label <- rc_to_full(rc1_col)
rc2_label <- rc_to_full(rc2_col)
rc3_label <- rc_to_full(rc3_col)

cow_clusters$cluster <- factor(cow_clusters$cluster)

cat("Number of cows:", nrow(cow_clusters), "\n")
cat("Clusters found:", nlevels(cow_clusters$cluster), "\n")
cat("Cluster sizes:\n")
print(table(cow_clusters$cluster))

###################################################################################################
################################## Sunset palette #################################################
###################################################################################################
sunset_palette <- c("#EE8866", "#AAAA00", "#EBAF02", "#77AADD")

n_clust <- nlevels(cow_clusters$cluster)
colours_used <- colorRampPalette(sunset_palette)(n_clust)

###################################################################################################
################################## Interactive 3D plot ############################################
###################################################################################################
fig <- plot_ly(
  data   = cow_clusters,
  x      = ~get(rc1_col),
  y      = ~get(rc2_col),
  z      = ~get(rc3_col),
  color  = ~cluster,
  colors = colours_used,
  type   = "scatter3d",
  mode   = "markers+text",
  marker = list(size = 5, opacity = 0.85,
                line = list(width = 1, color = "black")),
  text   = ~cow,
  textposition = "top center",
  textfont     = list(size = 10),
  hoverinfo    = "text",
  hovertext    = ~paste0("Cow: ", cow,
                         "<br>Cluster: ", cluster,
                         "<br>", rc1_label, ": ", round(get(rc1_col), 2),
                         "<br>", rc2_label, ": ", round(get(rc2_col), 2),
                         "<br>", rc3_label, ": ", round(get(rc3_col), 2))
) %>%
  layout(
    scene = list(
      xaxis = list(title = paste0(rc1_label, " (", var_rc1, "%)"),
                   titlefont = list(size = 18), tickfont = list(size = 14)),
      yaxis = list(title = paste0(rc2_label, " (", var_rc2, "%)"),
                   titlefont = list(size = 18), tickfont = list(size = 14)),
      zaxis = list(title = paste0(rc3_label, " (", var_rc3, "%)"),
                   titlefont = list(size = 18), tickfont = list(size = 14))
    ),
    legend = list(
      title    = list(text = "Cluster", font = list(size = 22)),
      font     = list(size = 20),
      itemsizing = "constant"
    )
  )

# Save as interactive HTML
html_path <- normalizePath(file.path(output_dir, "M3_3d_cluster_plot.html"), mustWork = FALSE)
htmlwidgets::saveWidget(fig, file = html_path, selfcontained = FALSE)

cat("\nInteractive 3D plot saved to:", html_path, "\n")

# Copy to docs/ for GitHub Pages
docs_dir <- "docs"
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)
file.copy(html_path, file.path(docs_dir, "index.html"), overwrite = TRUE)
lib_dir <- file.path(output_dir, "M3_3d_cluster_plot_files")
if (dir.exists(lib_dir)) {
  file.copy(lib_dir, docs_dir, recursive = TRUE, overwrite = TRUE)
}
cat("Copied to docs/ for GitHub Pages.\n")
cat("Open the HTML file in a browser to explore the plot.\n")
