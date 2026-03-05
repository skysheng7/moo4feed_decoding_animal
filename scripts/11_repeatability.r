library(lme4)
library(arm)
library(MuMIn)
library(tidyverse)
library(plyr)
library(broom)
library(coda)
library(grid)
library(gridExtra)
library(brms)
library(broom.mixed)
library(merTools)
library(tidybayes)
library(parallel)


my.cores <- detectCores()
m1_brm <- brm(meanDailyDisplacement ~ month + I(month^2) + Sex +
(1 | animal_id) + (1 | year/month),
              data = data,
              warmup = 500,
              iter = 3000,
              thin=2,
              chains = 2,
              inits  = "random",
              cores  = my.cores,
              seed = 12345)
m1_brm <- add_criterion(m1_brm, "waic")


m1_brm <- readRDS("m1_brm.rds") 
summary(m1_brm)
