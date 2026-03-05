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
############## Export feed_matrices by animal, bin, and feed info #################################
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
################## Export water_matrices by animal, bin info ######################################
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
####################### Export combined_matrices by animal, bin info ##############################
###################################################################################################
# Export each dataframe in combined_matrices list as a separate .rda file
for (date_name in names(combined_matrices)) {
  date_char <- as.character(date_name)
  df <- combined_matrices[[date_name]]
  file_name <- paste0("combined_matrices_", date_char, ".rda")
  file_path <- paste0(output_path, "/10_synchronicity_analysis_breakdown/combined_matrices/", file_name)
  save(df, file = file_path)
}

###################################################################################################
############## Export by date: load *_synch_master_*2.rda and split each day #####################
###################################################################################################
# Helper: load rda, create folder, export each dataframe (day) as separate file
export_by_date_from_rda <- function(rda_path, out_folder, file_prefix) {
  if (!file.exists(rda_path)) return(invisible(NULL))
  load(rda_path)
  out_dir <- paste0(base_dir, "/", out_folder)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  for (date_name in names(df)) {
    date_char <- as.character(date_name)
    day_df <- df[[date_name]]
    file_name <- paste0(file_prefix, "_", date_char, ".rda")
    save(day_df, file = paste0(out_dir, "/", file_name))
  }
}

# combined_matrices: animal, bin, feed (feed may not exist for combined)
export_by_date_from_rda(paste0(base_dir, "/combined_matrices/combined_matrices_synch_master_animal2.rda"),
  "combined_matrices_animal", "combined_matrices_animal")
export_by_date_from_rda(paste0(base_dir, "/combined_matrices/combined_matrices_synch_master_bin2.rda"),
  "combined_matrices_bin", "combined_matrices_bin")

# feed_matrices: animal, bin, feed
export_by_date_from_rda(paste0(base_dir, "/feed_matrices/feed_matrices_synch_master_animal2.rda"),
  "feed_matrices_animal", "feed_matrices_animal")
export_by_date_from_rda(paste0(base_dir, "/feed_matrices/feed_matrices_synch_master_bin2.rda"),
  "feed_matrices_bin", "feed_matrices_bin")
export_by_date_from_rda(paste0(base_dir, "/feed_matrices/feed_matrices_synch_master_feed2.rda"),
  "feed_matrices_feed", "feed_matrices_feed")

# water_matrices: animal, bin, feed (feed may not exist for water)
export_by_date_from_rda(paste0(base_dir, "/water_matrices/water_matrices_synch_master_animal2.rda"),
  "water_matrices_animal", "water_matrices_animal")
export_by_date_from_rda(paste0(base_dir, "/water_matrices/water_matrices_synch_master_bin2.rda"),
  "water_matrices_bin", "water_matrices_bin")

