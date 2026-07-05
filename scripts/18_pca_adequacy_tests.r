###################################################################################################
################## 18. PCA Data Adequacy: KMO & Bartlett's test of sphericity #####################
###################################################################################################
# Reviewer request: report the Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy and
# Bartlett's test of sphericity for the data BEFORE running PCA.
#
# This is computed for METHOD M3 (cluster-1 intercepts) -- the PCA reported in the paper.
#
# What these two tests check (both assess whether a correlation matrix is suitable for PCA/FA):
#
#   * Bartlett's test of sphericity  -- H0: the correlation matrix is an identity matrix, i.e.
#     the variables are mutually uncorrelated. A significant result (p < 0.05) rejects H0 and
#     indicates the variables are correlated enough for PCA to be worthwhile.
#     (Bartlett 1951; implemented in psych::cortest.bartlett)
#
#   * KMO measure of sampling adequacy -- the proportion of variance among the variables that
#     is common (shared) variance. Ranges 0-1. Kaiser (1974) guideline thresholds:
#         >= 0.90 marvelous | 0.80 meritorious | 0.70 middling |
#            0.60 mediocre  | 0.50 miserable   | < 0.50 unacceptable.
#     KMO also returns a per-variable MSA; variables with MSA < 0.50 are candidates for removal.
#     (Kaiser 1970, 1974; implemented in psych::KMO)
#
# The data matrix X is built IDENTICALLY to script 15 (15_pca_cow_clustering.r) for method M3.
#
# Prerequisites: run script 14 first (or ensure results/14_individual_traits/ CSVs exist).
#
# Outputs under results/18_pca_adequacy_tests/:
#   - pca_adequacy_kmo_bartlett.csv        (overall KMO + Bartlett chi-sq/df/p)
#   - pca_adequacy_kmo_per_variable.csv    (per-variable MSA)

set.seed(234)

library(tidyverse)
library(psych)        # KMO(), cortest.bartlett()

output_dir <- "results/18_pca_adequacy_tests"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

###################################################################################################
################################## Load data from script 14 #######################################
###################################################################################################
cow_traits  <- read.csv("results/14_individual_traits/cow_traits_wide.csv",
                        stringsAsFactors = FALSE)
behav_clust <- read.csv("results/14_individual_traits/behaviour_clusters.csv",
                        stringsAsFactors = FALSE)

# Method M3 = cluster-1 intercepts only (same variable selection as script 15)
vars_c1 <- behav_clust$variable[behav_clust$cluster == 1]
cols    <- paste0("int_", vars_c1)

# Kaiser (1974) verbal label for an overall KMO value
kmo_label <- function(k) {
  dplyr::case_when(
    is.na(k)  ~ NA_character_,
    k >= 0.90 ~ "marvelous",
    k >= 0.80 ~ "meritorious",
    k >= 0.70 ~ "middling",
    k >= 0.60 ~ "mediocre",
    k >= 0.50 ~ "acceptable",
    TRUE      ~ "unacceptable"
  )
}

###################################################################################################
################################## Build the M3 data matrix (identical to script 15) ##############
###################################################################################################
mat <- cow_traits[, c("cow", cols)]
mat <- mat[complete.cases(mat), ]     # remove rows with any NA -- same as script 15
X   <- mat[, -1, drop = FALSE]

n_obs  <- nrow(X)
n_vars <- ncol(X)

cat("\n", strrep("=", 70), "\n")
cat("Method M3 (cluster-1 intercepts)\n")
cat(strrep("=", 70), "\n")
cat("  n (cows):", n_obs, " | variables:", n_vars, "\n")

# PCA here standardises variables, so adequacy is assessed on the correlation matrix
R <- cor(X, use = "pairwise.complete.obs")

###################################################################################################
################################## KMO + Bartlett #################################################
###################################################################################################
bart <- psych::cortest.bartlett(R, n = n_obs)   # Bartlett's test of sphericity
kmo  <- psych::KMO(R)                            # KMO measure of sampling adequacy

overall_kmo <- as.numeric(kmo$MSA)
bart_chisq  <- as.numeric(bart$chisq)
bart_df     <- as.numeric(bart$df)
bart_p      <- as.numeric(bart$p.value)

cat(sprintf("  KMO (overall MSA): %.3f  (%s)\n", overall_kmo, kmo_label(overall_kmo)))
cat(sprintf("  Bartlett: chi-sq = %.1f, df = %d, p = %s\n",
            bart_chisq, as.integer(bart_df),
            format.pval(bart_p, digits = 3, eps = 1e-300)))

###################################################################################################
################################## Save + summarise ###############################################
###################################################################################################
adequacy_df <- data.frame(
  method             = "M3_c1_int",
  n_cows             = n_obs,
  n_variables        = n_vars,
  kmo_overall_msa    = round(overall_kmo, 4),
  kmo_interpretation = kmo_label(overall_kmo),
  bartlett_chisq     = round(bart_chisq, 3),
  bartlett_df        = as.integer(bart_df),
  bartlett_p_value   = bart_p,
  stringsAsFactors   = FALSE
)

msa_df <- data.frame(
  variable = names(kmo$MSAi),
  msa      = round(as.numeric(kmo$MSAi), 4),
  stringsAsFactors = FALSE
) %>%
  dplyr::arrange(msa)

write.csv(adequacy_df,
          file.path(output_dir, "pca_adequacy_kmo_bartlett.csv"),
          row.names = FALSE)
write.csv(msa_df,
          file.path(output_dir, "pca_adequacy_kmo_per_variable.csv"),
          row.names = FALSE)

cat("\n\n", strrep("=", 90), "\n")
cat("PCA DATA ADEQUACY SUMMARY (Method M3)\n")
cat(strrep("=", 90), "\n\n")
print(as.data.frame(adequacy_df), right = FALSE, row.names = FALSE)

cat("\n--- Per-variable MSA (ascending) ---\n")
print(as.data.frame(msa_df), right = FALSE, row.names = FALSE)

cat("\n--- Interpretation ---\n")
cat("  KMO  >= 0.50 required, >= 0.60 acceptable, >= 0.80 meritorious (Kaiser 1974).\n")
cat("  Bartlett p < 0.05 => correlation matrix differs from identity => PCA is appropriate.\n")
cat("\nSaved:\n")
cat("  ", file.path(output_dir, "pca_adequacy_kmo_bartlett.csv"), "\n")
cat("  ", file.path(output_dir, "pca_adequacy_kmo_per_variable.csv"), "\n")
