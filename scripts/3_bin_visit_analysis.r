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
  id_col = "cow",           # Your animal ID column
  start_col = "start",      # Visit start time column  
  end_col = "end",          # Visit end time column
  bin_col = "bin",          # Bin/feeder ID column
  intake_col = "intake",    # Feed intake amount column
  dur_col = "duration",     # Visit duration column
  tz = "America/Vancouver"  # Your timezone
)

# ---- STEP 1: Load Your Data ----
# Load your cleaned data
data(clean_feed)
data(clean_water)

# Or use your own cleaned data from previous tutorials:
# clean_feed <- your_cleaned_feed_data
# clean_water <- your_cleaned_water_data

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