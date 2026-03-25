###################################################################################################
######################### 11e. Repeatability Visualisation ########################################
###################################################################################################
# Ellipse scatter plot of repeatability (R) vs coefficient of individual variation (CVi).
# Each variable is drawn as an ellipse whose horizontal extent spans the 95% credible interval
# for R and whose vertical extent spans the 95% credible interval for CVi.
# Shape indicates the likelihood family used (Gaussian vs lognormal vs hurdle_lognormal).

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
    b = (CVi_upper   - CVi_lower)   / 2,
    label = gsub("_", " ", variable)
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
# Dynamic axis limits with padding
x_max <- max(rep_summary$R_cow_upper, na.rm = TRUE) * 1.1
y_max <- max(rep_summary$CVi_upper,   na.rm = TRUE) * 1.1

p <- ggplot(rep_summary, aes(x0 = R_cow_mean, y0 = CVi_mean,
                             a = a, b = b, angle = 0,
                             fill = variable, colour = variable)) +
  geom_ellipse(alpha = 0.7, linewidth = 0.4) +
  geom_point(aes(x = R_cow_mean, y = CVi_mean, colour = variable,
                 shape = family),
             size = 2.5, show.legend = TRUE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = R_cow_mean, y = CVi_mean, label = label, fill = variable),
    colour             = "black",
    fontface           = "bold",
    size               = 4,
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
  scale_x_continuous(limits = c(0, x_max), expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0.02, 0.05))) +
  scale_fill_manual(values   = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  scale_shape_manual(values = c("gaussian" = 16, "lognormal" = 17,
                                "hurdle_lognormal" = 15)) +
  labs(
    x     = "Repeatability (R) \n proportion of variance due to individual differences",
    y     = "Coefficient of variation (CVi) \n relative magnitude of individual differences",
    shape = "Likelihood family"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    axis.title       = element_text(size = 18),
    axis.text        = element_text(size = 14)
  ) +
  guides(fill = "none", colour = "none")

ggsave(
  filename = "results/11_repeatability/repeatability_ellipse_scatter.png",
  plot     = p,
  width    = 10,
  height   = 9
)

cat("Saved: results/11_repeatability/repeatability_ellipse_scatter.png\n")
