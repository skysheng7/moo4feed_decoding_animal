################################################################
# Package setup
#     - Installation guide from `moo4feed` package site:
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 1b. Rate calculation
#   - Calculate median feed and water intake rate (kg/s)
#     per cow per day and add to summary_df
################################################################

# ---- SETUP: Global Variables (REQUIRED FIRST!) ----
library(moo4feed)
library(dplyr)

# Set up your column names and timezone (modify these!)
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

# ---- STEP 1: Load Your Data ----
output_path <- "results"
load(paste0(output_path, "/1_data_cleaning/clean_feed.rda"))
load(paste0(output_path, "/1_data_cleaning/clean_water.rda"))

# ---- STEP 2: Combine Visit-Level Data ----
all_feed <- moo4feed::merge_list_df(clean_feed)
all_water <- moo4feed::merge_list_df(clean_water)

# ---- STEP 3: Calculate Median Rate Per Cow Per Day ----
feed_rates <- all_feed |>
  dplyr::group_by(cow, date) |>
  dplyr::summarise(median_feed_rate = median(rate, na.rm = TRUE),
                   .groups = "drop")

water_rates <- all_water |>
  dplyr::group_by(cow, date) |>
  dplyr::summarise(median_water_rate = median(rate, na.rm = TRUE),
                   .groups = "drop")

# ---- STEP 4: Join to summary_df ----
summary_df <- moo4feed::read_data_safely(
  paste0(output_path, "/1_data_cleaning/summary_df.csv"),
  header = TRUE, sep = ","
)

# Ensure date and cow types match for joining
summary_df$date <- as.character(summary_df$date)
feed_rates$date <- as.character(feed_rates$date)
water_rates$date <- as.character(water_rates$date)

summary_df$cow <- as.integer(summary_df$cow)
feed_rates$cow <- as.integer(feed_rates$cow)
water_rates$cow <- as.integer(water_rates$cow)

summary_df <- summary_df |>
  dplyr::left_join(feed_rates, by = c("cow", "date")) |>
  dplyr::left_join(water_rates, by = c("cow", "date"))

# ---- STEP 5: Save Results ----
write.csv(summary_df,
          file = paste0(output_path, "/1_data_cleaning/summary_df.csv"),
          row.names = FALSE)

cat("Done. Added median_feed_rate and median_water_rate to summary_df.\n")
cat("Rows:", nrow(summary_df), "\n")
cat("Feed rate NAs:", sum(is.na(summary_df$median_feed_rate)), "\n")
cat("Water rate NAs:", sum(is.na(summary_df$median_water_rate)), "\n")
