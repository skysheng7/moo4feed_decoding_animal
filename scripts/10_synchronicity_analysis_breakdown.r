###################################################################################################
################################## data loading ###################################################
###################################################################################################
# External packages
library(lubridate)

# Set output path
output_path <- "results"

# Create output directories if they don't exist
base_dir <- paste0(output_path, "/10_synchronicity_analysis_breakdown")
if (!dir.exists(base_dir)) {
  dir.create(base_dir, recursive = TRUE)
}
if (!dir.exists(paste0(base_dir, "/feed_matrices"))) {
  dir.create(paste0(base_dir, "/feed_matrices"), recursive = TRUE)
}
if (!dir.exists(paste0(base_dir, "/water_matrices"))) {
  dir.create(paste0(base_dir, "/water_matrices"), recursive = TRUE)
}
if (!dir.exists(paste0(base_dir, "/combined_matrices"))) {
  dir.create(paste0(base_dir, "/combined_matrices"), recursive = TRUE)
}

# Load the three matrix files
load(paste0(output_path, "/5_synchronicity_analysis/feed_matrices.rda"))
load(paste0(output_path, "/5_synchronicity_analysis/water_matrices.rda"))
load(paste0(output_path, "/5_synchronicity_analysis/combined_matrices.rda"))

###################################################################################################
########################## Export feed_matrices by date ###########################################
###################################################################################################
# Export each dataframe in feed_matrices list as a separate .rda file
for (date_name in names(feed_matrices)) {
  date_char <- as.character(date_name)
  df <- feed_matrices[[date_name]]
  file_name <- paste0("feed_matrices_", date_char, ".rda")
  file_path <- paste0(output_path, "/10_synchronicity_analysis_breakdown/feed_matrices/", file_name)
  save(df, file = file_path)
}

###################################################################################################
########################## Export water_matrices by date ##########################################
###################################################################################################
# Export each dataframe in water_matrices list as a separate .rda file
for (date_name in names(water_matrices)) {
  date_char <- as.character(date_name)
  df <- water_matrices[[date_name]]
  file_name <- paste0("water_matrices_", date_char, ".rda")
  file_path <- paste0(output_path, "/10_synchronicity_analysis_breakdown/water_matrices/", file_name)
  save(df, file = file_path)
}

###################################################################################################
####################### Export combined_matrices by date #########################################
###################################################################################################
# Export each dataframe in combined_matrices list as a separate .rda file
for (date_name in names(combined_matrices)) {
  date_char <- as.character(date_name)
  df <- combined_matrices[[date_name]]
  file_name <- paste0("combined_matrices_", date_char, ".rda")
  file_path <- paste0(output_path, "/10_synchronicity_analysis_breakdown/combined_matrices/", file_name)
  save(df, file = file_path)
}
