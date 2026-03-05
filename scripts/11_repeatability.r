###################################################################################################
################################## 11. Repeatability Analysis ####################################
###################################################################################################
# External packages
library(brms)        # for brm(), add_criterion(), fixef()
library(coda)        # for as.mcmc(), HPDinterval()
library(tidybayes)   # for posterior_samples()
library(tidyverse)   # for %>%, gather(), separate(), left_join(), group_by(), mutate(), ungroup()
library(ggplot2)     # for ggplot(), geom_point(), labs(), theme_classic(), scale_fill_manual()
library(parallel)    # for detectCores()

###################################################################################################
################################## Bayesian repeatability model ###################################
###################################################################################################
# Fit Bayesian mixed model with animal ID and year/month as random effects
my.cores <- detectCores()
m1_brm <- brm(meanDailyDisplacement ~ month + I(month^2) + Sex +
                (1 | animal_id) + (1 | year/month),
              data = data,
              warmup = 500,
              iter = 3000,
              thin = 2,
              chains = 2,
              inits = "random",
              cores = my.cores,
              seed = 12345)
m1_brm <- add_criterion(m1_brm, "waic")

m1_brm <- readRDS("m1_brm.rds")
summary(m1_brm)

###################################################################################################
################################## Variance partitioning #########################################
###################################################################################################
# Extract posterior variance components
var.animal_id  <- posterior_samples(m1_brm)$"sd_animal_id__Intercept"^2
var.year       <- posterior_samples(m1_brm)$"sd_year__Intercept"^2
var.year.month <- posterior_samples(m1_brm)$"sd_year:month__Intercept"^2
var.res        <- posterior_samples(m1_brm)$"sigma"^2

# Total variance (denominator shared across all repeatability calculations)
var.total <- var.animal_id + var.year.month + var.year + var.res

# Repeatability: proportion of variance attributable to among-individual differences
RDist <- var.animal_id / var.total
mean(RDist); HPDinterval(as.mcmc(RDist), 0.95)

# Proportion of variance attributable to year
RYear <- var.year / var.total
mean(RYear)

# Proportion of variance attributable to year:month interaction
RYearMonth <- var.year.month / var.total
mean(RYearMonth)

# Proportion of variance attributable to residual
RRes <- var.res / var.total
mean(RRes)

# Individual coefficient of variation (CVi): among-individual SD relative to population mean
CVi <- sqrt(var.animal_id) / mean(data$meanDailyDisplacement)
mean(CVi); HPDinterval(as.mcmc(CVi), 0.95)

###################################################################################################
################################## Posterior BT plot #############################################
###################################################################################################
# Extract individual-level posterior intercepts and adjust for fixed effects by sex
posteriorBT <- posterior_samples(m1_brm)[, 9:43] %>%
  gather(animal_id, value,
         "r_animal_id[elephant1,Intercept]":"r_animal_id[elephant9,Intercept]") %>%
  separate(animal_id,
           c(NA, NA, NA, "animal_id", NA),
           sep = "([\\_\\[\\,])", fill = "right") %>%
  left_join(select(data[!duplicated(data$animal_id), ], animal_id, Sex))

posteriorBT[posteriorBT$Sex == "F", ]$value <-
  posteriorBT[posteriorBT$Sex == "F", ]$value + fixef(m1_brm, pars = "Intercept")[1]
posteriorBT[posteriorBT$Sex == "M", ]$value <-
  posteriorBT[posteriorBT$Sex == "M", ]$value + fixef(m1_brm, pars = "Intercept")[1] + fixef(m1_brm, pars = "SexM")[1]

# Highlight specific individuals of interest; all others grouped as "Other individuals"
posteriorBT$col <- ifelse(posteriorBT$animal_id %in%
                            c("elephant17", "elephant4", "elephant8",
                              "elephant36", "elephant20"),
                          posteriorBT$animal_id, "Other individuals")

# Compute per-individual posterior mean for ordering the y-axis
posteriorBT <- posteriorBT %>%
  dplyr::group_by(animal_id) %>%
  dplyr::mutate(meanBT = mean(value)) %>%
  dplyr::ungroup()

# Ridge density plot of individual posterior distributions, ordered by posterior mean
BT <- ggplot() +
  ggridges::geom_density_ridges(data = posteriorBT,
                                aes(x = value,
                                    y = reorder(as.factor(animal_id), meanBT),
                                    height = ..density..,
                                    fill = col, scale = 3),
                                alpha = 0.6) +
  geom_point(data = posteriorBT[!duplicated(posteriorBT$animal_id), ],
             aes(x = meanBT, y = as.factor(animal_id), col = Sex),
             size = 1) +
  labs(y = "", x = "BT mean daily distance (km)", fill = "ID") +
  theme_classic() +
  scale_fill_manual(values = c("#F8766D", "#C77CFF", "#7CAE00",
                               "#FFCC00", "#00BFC4", "gray"))
