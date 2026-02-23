################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 1. Data cleaning
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/data_cleaning.html
################################################################

# ---- SETUP: Load Packages ----
library(devtools)
library(moo4feed)

# ---- STEP 1: Set Paths to Data Files ----
# For your own data:
extdata_path <- "data/insentec"
output_path <- "results"

# If feeders and drinkers are in different folders:
# extdata_path_f <- "/path/to/feeder/files"
# extdata_path_w <- "/path/to/drinker/files"

# ---- STEP 2: Find Data Files ----
# Find all feeder files (VR*.DAT)
fileNames.f <- list.files(
  path       = extdata_path,
  pattern    = "^VR.*\\.DAT$",
  recursive  = TRUE,
  full.names = TRUE
) |> sort()

# Find all drinker files (VW*.DAT)
fileNames.w <- list.files(
  path       = extdata_path,
  pattern    = "^VW.*\\.DAT$",
  recursive  = TRUE,
  full.names = TRUE
) |> sort()

# ---- STEP 3: Handle Daylight Saving Time ----
dst_df <- get_dst_switch_info(years = c(2020, 2021), tz = "America/Vancouver")

# ---- STEP 4: Match Files by Date ----
date_compare <- compare_files(fileNames.f, fileNames.w)
fileNames.f <- date_compare$feed
fileNames.w <- date_compare$water

# Get the overall date range
date_result <- get_date_range(fileNames.f)

# ---- STEP 5: Configure Global Settings ----
# Option 1: All at once
set_global_cols(
  # Time zone
  tz = "America/Vancouver",
  
  # Column names in your data files
  id_col = "cow",
  trans_col = "transponder",
  start_col = "start",
  end_col = "end",
  bin_col = "bin",
  dur_col = "duration",
  intake_col = "intake",
  start_weight_col = "start_weight",
  end_weight_col = "end_weight",
  
  # Bin settings
  bins_feed = 1:30,
  bins_wat = 1:5,
  bin_offset = 100
)

# Option 2: Individual settings (alternative)
# set_tz2("America/Vancouver")
# set_id_col2("cow")
# set_trans_col2("transponder")
# set_start_col2("start")
# set_end_col2("end")
# set_bin_col2("bin")
# set_bins_feed2(1:30)
# set_bins_wat2(1:5)
# set_bin_offset2(100)

# ---- STEP 6: Process Feeder Data ----
# the .DAT files I get from the feeders do not have column headers,
# so we need to define the column names manually. Note the number of columns 
# you manually declear here should match the number of values in each row in the .DAT file.
col_names <- c("transponder", "cow", "bin", "start", "end", "duration", 
              "start_weight", "end_weight", "comment", "intake", "intake2", 
              "X1", "X2", "X3", "X4", "x5")

# IDs to remove
drop_ids <- c(0, 1556, 5015, 1111, 1112, 1113, 1114)

# Columns to keep
select_cols <- c("transponder", "cow", "bin", "start", "end", 
                "duration", "start_weight", "end_weight", "intake")

# Process all feeder files
all_fed <- process_all_feed(
    files = fileNames.f,
    col_names   = col_names,
    drop_ids    = drop_ids,
    select_cols = select_cols,
    sep         = ",",
    header      = FALSE,
    adjust_dst  = TRUE
)

# ---- STEP 7: Process Water Data ----
# Column names for water files
col_names_wat <- c(
  "transponder", "cow", "bin", "start", "end",
  "duration", "start_weight", "end_weight", "intake"
)

# Use same IDs to remove
drop_ids <- c(0, 1556, 5015, 1111:1114)

# Columns to keep
select_cols_wat <- c(
  "transponder", "cow", "bin", "start", "end",
  "duration", "start_weight", "end_weight", "intake"
)

# Process all water files
all_wat <- process_all_water(
  files        = fileNames.w,
  col_names    = col_names_wat,
  drop_ids     = drop_ids,
  select_cols  = select_cols_wat,
  sep          = ",",
  header       = FALSE,
  adjust_dst   = TRUE
)

# ---- STEP 8: Quality Check Configuration ----
my_qc_config <- qc_config(
  # Feed thresholds
  high_dur_feed = 2000,
  large_intake_visit_feed = 8,
  large_intake_rate_feed = 0.008,
  low_feed_intake = 35,
  high_feed_intake = 75,
  
  # Water thresholds
  high_dur_water = 1800,
  large_intake_visit_water = 30,
  large_intake_rate_water = 0.35,
  low_wat_intake = 60,
  high_wat_intake = 180,
  
  # General settings
  low_visit_threshold = 10,
  total_cows_expected = 48,
  replacement_threshold = 26,
  calibration_error = 0.5
)

# ---- STEP 9: Run Quality Check ----
qc_results <- qc(
  feed = all_fed,
  water = all_wat,
  cfg = my_qc_config,
  id_col = id_col2(),               # Animal ID column (default from global vars)
  start_col = start_col2(),         # Visit start time column (default from global vars)
  end_col = end_col2(),             # Visit end time column (default from global vars)
  bin_col = bin_col2(),             # Bin/feeder ID column (default from global vars)
  dur_col = duration_col2(),        # Visit duration column (default from global vars)
  intake_col = intake_col2(),       # Intake amount column (default from global vars)
  start_weight_col = start_weight_col2(),  # Start weight column (default from global vars)
  end_weight_col = end_weight_col2(),      # End weight column (default from global vars)
  tz = tz2(),                       # Timezone (default from global vars)
  bins_feed = bins_feed2(),         # Valid feed bin numbers (default from global vars)
  bins_wat = bins_wat2(),           # Valid water bin numbers (default from global vars)
  bin_offset = bin_offset2(),       # Bin offset for water bins (default from global vars)
  verbose = FALSE,
  fix_double_detections = TRUE
)

# save warning messages to a file
warning <- qc_results$warnings

# ---- STEP 10: Extract Cleaned Data ----
qc_feed <- qc_results$feed
qc_water <- qc_results$water
qc_combined <- qc_results$combined

# ---- STEP 11: KNN Outlier Detection ----
# Detect outliers in feed data
feed_with_outliers <- knn_clean_feed(
  qc_feed,
  k = 50,
  threshold_percentile = 99.96,
  custom_scaling = list(
    rate = 500,
    intake = 30,
    duration = 0.02
  ),
  date_col = "date",                # Date column name
  remove_outliers = FALSE
)

# Visualize feed outliers
p1 <- viz_outliers(
  feed_with_outliers,
  x_var = "duration",
  y_var = "intake",
  title = "Feed Outliers: Duration vs Intake",
  jitter_amount = 0.2,
  regular_color = "lightgreen",
  outlier_color = "orange"
)
print(p1)

p2 <- viz_outliers(
  feed_with_outliers,
  x_var = "rate",
  y_var = "intake",
  title = "Feed Outliers: Rate vs Intake",
  jitter_amount = 0,
  regular_color = "lightgreen",
  outlier_color = "orange"
)
print(p2)

# Detect outliers in water data
water_with_outliers <- knn_clean_water(
  qc_water,
  k = 50,
  threshold_percentile = 99.9,
  custom_scaling = list(
    rate = 500,
    intake = 30,
    duration = 0.02
  ),
  intake_col = intake_col2(),       # Intake amount column (default from global vars)
  duration_col = duration_col2(),   # Visit duration column (default from global vars)
  date_col = "date",                # Date column name
  remove_outliers = FALSE
)

# Visualize water outliers
p3 <- viz_outliers(
  water_with_outliers,
  x_var = "duration",
  y_var = "intake",
  title = "Water Outliers: Duration vs Intake",
  regular_color = "lightblue",
  outlier_color = "orange"
)
print(p3)

p4 <- viz_outliers(
  water_with_outliers,
  x_var = "rate",
  y_var = "intake",
  title = "Water Outliers: Rate vs Intake",
  jitter_amount = 0,
  regular_color = "lightblue",
  outlier_color = "orange"
)
print(p4)

# Remove outliers
cleaned_feed_no_outliers <- knn_clean_feed(
  qc_feed,
  k = 50,
  threshold_percentile = 99.96,
  custom_scaling = list(rate = 500, intake = 30, duration = 0.02),
  date_col = "date",                # Date column name
  remove_outliers = TRUE
)

# Same for water data
cleaned_water_no_outliers <- knn_clean_water(
  qc_water,
  k = 50,
  threshold_percentile = 99.9,
  custom_scaling = list(rate = 500, intake = 30, duration = 0.02),
  date_col = "date",                # Date column name
  remove_outliers = TRUE
)

# Set final cleaned datasets
clean_feed <- cleaned_feed_no_outliers
clean_water <- cleaned_water_no_outliers
clean_comb <- combine_feed_water(clean_feed, clean_water)

# ---- STEP 12: Create Daily Summaries ----
daily_summary <- feed_water_summary(
  feed = qc_feed,
  water = qc_water,
  warn = warning,
  cfg = my_qc_config,
  id_col = id_col2(),               # Animal ID column (default from global vars)
  intake_col = intake_col2(),       # Intake amount column (default from global vars)
  dur_col = duration_col2()         # Visit duration column (default from global vars)
)

# Get summary dataframe
summary_df <- daily_summary$summary

# Get updated warnings
warning <- daily_summary$warn


################################################################
# Save results
################################################################
# Create output directory if it doesn't exist
if (!dir.exists(paste0(output_path, "/1_data_cleaning"))) {
  dir.create(paste0(output_path, "/1_data_cleaning"), recursive = TRUE)
}
write.csv(warning, file = paste0(output_path, "/1_data_cleaning/warnings.csv"))
write.csv(summary_df, file = paste0(output_path, "/1_data_cleaning/summary_df.csv"))
save(clean_feed, file = paste0(output_path, "/1_data_cleaning/clean_feed.rda"))
save(clean_water, file = paste0(output_path, "/1_data_cleaning/clean_water.rda"))
save(clean_comb, file = paste0(output_path, "/1_data_cleaning/clean_comb.rda"))