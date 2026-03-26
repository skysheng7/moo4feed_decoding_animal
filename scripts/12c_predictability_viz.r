###################################################################################################
######################### 12c. Predictability Visualisation ########################################
###################################################################################################
# Ellipse scatter plot of relative intra-individual variability (rIIV) vs
# coefficient of predictability (CVP).
# Each variable is drawn as an ellipse whose horizontal extent spans the 95%
# credible interval for rIIV and whose vertical extent spans the 95% credible
# interval for CVP.
# Shape indicates the likelihood family used.

library(ggplot2)
library(ggforce)
library(ggrepel)
library(dplyr)

###################################################################################################
################################## Load summary data ##############################################
###################################################################################################
pred_summary <- read.csv("results/12_predictability/predictability_summary.csv",
                         header = TRUE, stringsAsFactors = FALSE)

###################################################################################################
################################## Compute ellipse geometry #######################################
###################################################################################################
pred_summary <- pred_summary %>%
  mutate(
    a     = (rIIV_upper - rIIV_lower) / 2,
    b     = (CVP_upper  - CVP_lower)  / 2,
    label = gsub("_", " ", variable)
  )

###################################################################################################
################################## Build colour palette ###########################################
###################################################################################################
n_vars <- nrow(pred_summary)
colour_pal <- scales::hue_pal()(n_vars)
names(colour_pal) <- pred_summary$variable

###################################################################################################
################################## Plot ############################################################
###################################################################################################
p <- ggplot(pred_summary, aes(x0 = rIIV_mean, y0 = CVP_mean,
                              a = a, b = b, angle = 0,
                              fill = variable, colour = variable)) +
  geom_ellipse(alpha = 0.7, linewidth = 0.4) +
  geom_point(aes(x = rIIV_mean, y = CVP_mean, colour = variable,
                 shape = family),
             size = 2.5, show.legend = TRUE,
             inherit.aes = FALSE) +
  ggrepel::geom_label_repel(
    aes(x = rIIV_mean, y = CVP_mean, label = label, fill = variable),
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
    show.legend        = FALSE,
    inherit.aes        = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.25, 0.25))) +
  scale_fill_manual(values   = colour_pal) +
  scale_colour_manual(values = colour_pal) +
  scale_shape_manual(values = c("gaussian" = 16, "lognormal" = 17,
                                "hurdle_lognormal" = 15)) +
  labs(
    x     = "Relative intra-individual variability (rIIV)",
    y     = "Coefficient of predictability (CVP)",
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
  filename = "results/12_predictability/predictability_ellipse_scatter.png",
  plot     = p,
  width    = 10,
  height   = 9
)

cat("Saved: results/12_predictability/predictability_ellipse_scatter.png\n")
