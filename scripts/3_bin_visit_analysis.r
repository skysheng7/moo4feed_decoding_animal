################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 3. Bin visit analysis
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/bin_visit_analysis.html
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
load(paste0(output_path, "/1_data_cleaning/clean_water.rda"))

# ---- STEP 2: Calculate Unique Bin Visits ----
# Get overall summary across all days
bin_visits <- unique_bin_visits(
  feed = clean_feed,            # Your cleaned feed data
  water = clean_water,          # Your cleaned water data
  return_list = FALSE           # FALSE = combined summary, TRUE = day-by-day
)

# View results
head(bin_visits)

# ---- STEP 3: Identify Most/Least Exploratory Animals ----
# Calculate averages per animal
avg_bin_visits <- bin_visits |>
  dplyr::group_by(cow) |>
  dplyr::summarize(
    avg_feed_bins = mean(unique_feed_bins_visited),
    avg_water_bins = mean(unique_water_bins_visited), 
    avg_total_bins = mean(total_bins_visited),
    days_observed = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(avg_total_bins))    # Most exploratory first

# Get top and bottom explorers
top_explorers <- head(avg_bin_visits, 10)       # Most exploratory
creatures_of_habit <- tail(avg_bin_visits, 10)  # Least exploratory

print(top_explorers)
print(creatures_of_habit)

# ---- STEP 4: Visualize Results ----
# Top exploratory animals bar plot
top_50_explorers <- head(avg_bin_visits, 50)

ggplot(top_50_explorers, aes(x = reorder(cow, avg_total_bins), y = avg_total_bins)) +
  geom_bar(stat = "identity", fill = "lightcoral", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average Total Bins Visited by Animal",
    subtitle = "Sorted by average total unique bins visited",
    x = "Animal ID",
    y = "Average Total Unique Bins Visited"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

################################################################
# Save results
################################################################
# Create output directory if it doesn't exist
if (!dir.exists(paste0(output_path, "/3_bin_visit_analysis"))) {
  dir.create(paste0(output_path, "/3_bin_visit_analysis"), recursive = TRUE)
}

# Save bin visits summary
write.csv(bin_visits, 
          paste0(output_path, "/3_bin_visit_analysis/bin_visits.csv"), 
          row.names = FALSE)

# Save averaged bin visits summary
write.csv(avg_bin_visits, 
          paste0(output_path, "/3_bin_visit_analysis/avg_bin_visits.csv"), 
          row.names = FALSE)

# Save top explorers
write.csv(top_explorers, 
          paste0(output_path, "/3_bin_visit_analysis/top_explorers.csv"), 
          row.names = FALSE)

# Save creatures of habit
write.csv(creatures_of_habit, 
          paste0(output_path, "/3_bin_visit_analysis/creatures_of_habit.csv"), 
          row.names = FALSE)