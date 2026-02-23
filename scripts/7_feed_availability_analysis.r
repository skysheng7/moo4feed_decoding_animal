################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 7. Feed availability analysis
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/feed_availability_analysis.html
################################################################
# ---- SETUP: Global Variables (REQUIRED FIRST!) ----
library(moo4feed)
library(ggplot2)
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
# Load your cleaned data from previous analysis
output_path <- "results"
load(paste0(output_path, "/1_data_cleaning/clean_feed.rda"))

# ---- STEP 2: Detect Feed Additions (Per-Bin) ----
# This is required for calculating feed availability
feed_additions <- detect_feed_additions(
  data = clean_feed,
  min_weight_increase = 5,      # Minimum kg to count as addition
  max_bin_time_gap = 3600,      # Group additions within 1 hour (seconds)
  aggregate_all_bin = FALSE     # Keep per-bin (REQUIRED for availability)
)

# Check results
head(feed_additions[[1]])

# ---- STEP 3: Detect Aggregated Feed Events (Optional) ----
# Use this to identify coordinated multi-bin feeding events
feed_events <- detect_feed_additions(
  data = clean_feed,
  min_weight_increase = 5,
  max_bin_time_gap = 3600,
  min_bins_for_group = 3,       # At least 3 bins to count as event
  aggregate_all_bin = TRUE      # Aggregate across bins
)

# Check results
head(feed_events[[1]])

# ---- STEP 4: Calculate Feed Availability ----
availability <- calculate_feed_availability(
  visit_data = clean_feed,
  feed_addition_data = feed_additions  # From Step 2 (aggregate_all_bin = FALSE)
)

# Access visit-level data with feed percentages
visits_with_pct <- availability$visits

# Access daily summaries per animal
daily_summaries <- availability$daily_summary

# View first day results
head(visits_with_pct[[1]])
print(daily_summaries[[1]])

# ---- STEP 5: Analyze Patterns ----
# Combine all visits across days
all_visits <- do.call(rbind, availability$visits)

# Filter to valid visits
valid_visits <- all_visits |>
  dplyr::filter(!is.na(pct_feed_remaining))

# Find animals with lowest feed availability using visit-level data
low_availability <- valid_visits |>
  dplyr::group_by(cow) |>
  dplyr::summarise(
    overall_mean_pct = mean(pct_feed_remaining, na.rm = TRUE),
    overall_median_pct = median(pct_feed_remaining, na.rm = TRUE),
    total_visits = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(overall_median_pct)

print(low_availability)

# ---- STEP 6: Visualize Results ----
# Distribution of feed availability

# Histogram of feed availability
ggplot(valid_visits, aes(x = pct_feed_remaining)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Feed Availability at Visits",
    x = "Percentage of Feed Remaining (%)",
    y = "Number of Visits"
  ) +
  theme_minimal()

# Violin plot of feed availability by animal (top 5 and bottom 5 by median pct_feed_remaining)
# Calculate median feed availability per animal
animal_medians <- valid_visits |>
  dplyr::group_by(cow) |>
  dplyr::summarise(
    median_pct = median(pct_feed_remaining, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(median_pct)

# Top 5 animals with highest median feed availability
top_animals <- animal_medians |>
  tail(5) |>
  dplyr::pull(cow)

# Bottom 5 animals with lowest median feed availability
bottom_animals <- animal_medians |>
  head(5) |>
  dplyr::pull(cow)

all_animals <- c(top_animals, bottom_animals)

valid_visits |>
  dplyr::filter(cow %in% all_animals) |>
  ggplot(aes(x = reorder(cow, pct_feed_remaining, FUN = median),
             y = pct_feed_remaining)) +
  geom_violin(fill = "olivedrab3", alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 1) +
  labs(
    title = "Feed Availability by Animal (Top 5 and Bottom 5 by Median)",
    x = "Animal ID",
    y = "Percentage of Feed Remaining (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################
# Save results
################################################################
# Create output directory if it doesn't exist
if (!dir.exists(paste0(output_path, "/7_feed_availability_analysis"))) {
  dir.create(paste0(output_path, "/7_feed_availability_analysis"), recursive = TRUE)
}

# Save feed additions by day as RDA
save(feed_additions, file = paste0(output_path, "/7_feed_availability_analysis/feed_additions.rda"))

# Save feed events by day as RDA
save(feed_events, file = paste0(output_path, "/7_feed_availability_analysis/feed_events.rda"))

# Save availability results as RDA
save(availability, file = paste0(output_path, "/7_feed_availability_analysis/availability.rda"))

# Save combined data as CSV
write.csv(all_visits, 
          paste0(output_path, "/7_feed_availability_analysis/all_visits_with_availability.csv"), 
          row.names = FALSE)

# Save low availability summary
write.csv(low_availability, 
          paste0(output_path, "/7_feed_availability_analysis/low_availability_summary.csv"), 
          row.names = FALSE)

# Save first day summaries as examples
write.csv(daily_summaries[[1]], 
          paste0(output_path, "/7_feed_availability_analysis/daily_summary_first_day.csv"), 
          row.names = FALSE)

write.csv(visits_with_pct[[1]], 
          paste0(output_path, "/7_feed_availability_analysis/visits_first_day.csv"), 
          row.names = FALSE)