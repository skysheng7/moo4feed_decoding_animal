###################################################################################################
################# 13. Interactive 3D visualisation: CVi vs R vs CVP ###############################
###################################################################################################
# Merges repeatability (CVi, R) and predictability (CVP) summaries and produces
# an interactive 3D scatter plot with plotly. The output is saved as a
# self-contained HTML file that can be opened in any browser.

library(dplyr)
library(plotly)
library(htmlwidgets)

###################################################################################################
################################## Load & merge summaries #########################################
###################################################################################################
rep_summary  <- read.csv("results/11_repeatability/repeatability_summary.csv",
                         header = TRUE, stringsAsFactors = FALSE)
pred_summary <- read.csv("results/12_predictability/predictability_summary.csv",
                         header = TRUE, stringsAsFactors = FALSE)

combined <- inner_join(rep_summary, pred_summary, by = c("variable", "family"))

combined <- combined %>%
  mutate(label = gsub("_", " ", variable))

###################################################################################################
################################## Build 3D scatter plot ##########################################
###################################################################################################
shape_map <- c("gaussian" = "circle", "lognormal" = "diamond")

fig <- plot_ly(
  data        = combined,
  x           = ~CVi_mean,
  y           = ~R_cow_mean,
  z           = ~CVP_mean,
  type        = "scatter3d",
  mode        = "markers+text",
  marker      = list(
    size        = 10,
    opacity     = 0.85,
    symbol      = ~ifelse(family == "gaussian", "circle", "diamond"),
    color       = ~CVP_mean,
    colorscale  = "Viridis",
    colorbar    = list(title = "CVP"),
    line        = list(color = "black", width = 0.5)
  ),
  text        = ~label,
  textposition = "top center",
  textfont    = list(size = 10),
  hovertemplate = paste0(
    "<b>%{text}</b><br>",
    "CVi: %{x:.4f}<br>",
    "R: %{y:.4f}<br>",
    "CVP: %{z:.4f}",
    "<extra></extra>"
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "CVi (individual variation)"),
      yaxis = list(title = "R (repeatability)"),
      zaxis = list(title = "CVP (predictability)"),
      camera = list(
        eye = list(x = 1.6, y = -1.6, z = 0.9)
      )
    ),
    title = list(
      text = "Relationship between CVi, Repeatability (R), and Predictability (CVP)",
      font = list(size = 16)
    ),
    margin = list(t = 60)
  )

###################################################################################################
################################## Add error bars as lines ########################################
###################################################################################################
# Plotly scatter3d doesn't support native error bars on all three axes, so we
# draw thin line segments for the 95% credible intervals on each axis.

for (i in seq_len(nrow(combined))) {
  row <- combined[i, ]

  # CVi interval (horizontal, along x)
  fig <- fig %>% add_trace(
    x = c(row$CVi_lower, row$CVi_upper),
    y = c(row$R_cow_mean, row$R_cow_mean),
    z = c(row$CVP_mean, row$CVP_mean),
    type = "scatter3d", mode = "lines",
    line = list(color = "grey", width = 3),
    showlegend = FALSE, hoverinfo = "skip"
  )

  # R interval (along y)
  fig <- fig %>% add_trace(
    x = c(row$CVi_mean, row$CVi_mean),
    y = c(row$R_cow_lower, row$R_cow_upper),
    z = c(row$CVP_mean, row$CVP_mean),
    type = "scatter3d", mode = "lines",
    line = list(color = "grey", width = 3),
    showlegend = FALSE, hoverinfo = "skip"
  )

  # CVP interval (along z)
  fig <- fig %>% add_trace(
    x = c(row$CVi_mean, row$CVi_mean),
    y = c(row$R_cow_mean, row$R_cow_mean),
    z = c(row$CVP_lower, row$CVP_upper),
    type = "scatter3d", mode = "lines",
    line = list(color = "grey", width = 3),
    showlegend = FALSE, hoverinfo = "skip"
  )
}

###################################################################################################
################################## Save interactive HTML ##########################################
###################################################################################################
out_dir <- "results/13_3d_visualization"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_path <- file.path(out_dir, "3d_CVi_R_CVP.html")
saveWidget(fig, file = normalizePath(out_path, mustWork = FALSE), selfcontained = TRUE)

cat("Saved interactive 3D plot:", out_path, "\n")
