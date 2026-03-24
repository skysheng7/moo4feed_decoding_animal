###################################################################################################
######################### 11b. Repeatability Visualisation ########################################
###################################################################################################
# Ellipse scatter plot of repeatability (R) vs coefficient of individual variation (CVi).
# Each variable is drawn as an ellipse whose horizontal extent spans the 95% credible interval
# for R and whose vertical extent spans the 95% credible interval for CVi.

library(ggplot2)
library(ggforce)
library(ggrepel)
library(dplyr)

###################################################################################################
################################## Load summary data ##############################################
###################################################################################################
rep_summary <- read.csv("results/11_repeatability/repeatability_summary.csv",
                        header = TRUE, stringsAsFactors = FALSE)

###################################################################################################
################################## Compute ellipse geometry #######################################
###################################################################################################
rep_summary <- rep_summary %>%
  mutate(
    a = (R_cow_upper - R_cow_lower) / 2,
    b = (CVi_upper   - CVi_lower)   / 2
  )

###################################################################################################
################################## Build colour palette ###########################################
###################################################################################################
n_vars <- nrow(rep_summary)
colour_pal <- scales::hue_pal()(n_vars)
names(colour_pal) <- rep_summary$variable

###################################################################################################
################################## Plot ############################################################
###################################################################################################
p <- ggplot(rep_summary, aes(x0 = R_cow_mean, y0 = CVi_mean,
                             a = a, b = b, angle = 0,
                             fill = variable, colour = variable)) +
  geom_ellipse(alpha = 0.7, linewidth = 0.4) +
  geom_point(aes(x = R_cow_mean, y = CVi_mean, colour = variable),
             size = 1.5, show.legend = FALSE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = R_cow_mean, y = CVi_mean, label = variable, fill = variable),
    colour          = "black",
    fontface        = "bold",
    size            = 3,
    alpha           = 0.7,
    label.padding   = unit(0.2, "lines"),
    box.padding     = unit(0.4, "lines"),
    point.padding   = unit(0.3, "lines"),
    direction       = "both",
    max.overlaps    = Inf,
    show.legend     = FALSE,
    inherit.aes     = FALSE
  ) +
  scale_fill_manual(values   = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  labs(
    x = "Repeatability (R) \u2014 proportion of variance due to individual differences",
    y = "Coefficient of variation (CVi) \u2014 relative magnitude of individual differences"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(
  filename = "results/11_repeatability/repeatability_ellipse_scatter.png",
  plot     = p,
  width    = 10,
  height   = 8
)

cat("Saved: results/11_repeatability/repeatability_ellipse_scatter.png\n")
