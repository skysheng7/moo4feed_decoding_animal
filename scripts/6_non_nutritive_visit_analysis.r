################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 6. Non-nutritive visit analysis
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/non_nutritive_visits.html
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

# ---- STEP 2: Create QC Configuration ----
my_qc_config <- qc_config(
  calibration_error = 0.5    # Equipment measurement threshold (kg)
)

# ---- STEP 3: Calculate Non-Nutritive Visits ----
# Basic calculation (unsorted)
non_nutritive <- calculate_non_nutritive_visits(
  data = clean_feed,
  cfg = my_qc_config
)

# View first day results
head(non_nutritive[[1]])

# Sort by highest visits first (descending)
non_nutritive_desc <- calculate_non_nutritive_visits(
  data = clean_feed,
  cfg = my_qc_config,
  sort = -1
)

head(non_nutritive_desc[[1]], 5)

# Sort by lowest visits first (ascending)
non_nutritive_asc <- calculate_non_nutritive_visits(
  data = clean_feed,
  cfg = my_qc_config,
  sort = 1
)

head(non_nutritive_asc[[1]], 5)

# ---- STEP 4: Calculate No-Feed Visits ----
# Basic calculation (unsorted)
no_feed <- calculate_no_feed_visits(
  data = clean_feed,
  cfg = my_qc_config
)

# View first day results
head(no_feed[[1]])

# Sort by highest empty bin visits (descending)
no_feed_desc <- calculate_no_feed_visits(
  data = clean_feed,
  cfg = my_qc_config,
  sort = -1
)

head(no_feed_desc[[1]], 5)

# ---- STEP 5: Visualize Results ----
# Non-nutritive visits bar plot (first day example)
top_nn <- head(non_nutritive_desc[[1]], 50)

ggplot(top_nn, aes(x = reorder(cow, number_of_non_nutritive_visits), y = number_of_non_nutritive_visits)) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Frequency of Non-Nutritive Visits",
    subtitle = "Non-nutritive visits: Feed available but animal did not eat",
    x = "Animal ID",
    y = "Total Non-Nutritive Visits"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

# Empty bin visits bar plot (first day example)
top_empty <- head(no_feed_desc[[1]], 50)

ggplot(top_empty, aes(x = reorder(cow, number_of_visits_when_no_feed), y = number_of_visits_when_no_feed)) +
  geom_bar(stat = "identity", fill = "olivedrab3", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Frequency of Empty Bin Visits",
    subtitle = "Empty bin visits: No feed available in the visited bin",
    x = "Animal ID",
    y = "Total Empty Bin Visits"
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
if (!dir.exists(paste0(output_path, "/6_non_nutritive_visit_analysis"))) {
  dir.create(paste0(output_path, "/6_non_nutritive_visit_analysis"), recursive = TRUE)
}

# Save non-nutritive visits by day as RDA
save(non_nutritive, file = paste0(output_path, "/6_non_nutritive_visit_analysis/non_nutritive.rda"))
save(non_nutritive_desc, file = paste0(output_path, "/6_non_nutritive_visit_analysis/non_nutritive_desc.rda"))
save(non_nutritive_asc, file = paste0(output_path, "/6_non_nutritive_visit_analysis/non_nutritive_asc.rda"))

# Save no-feed visits by day as RDA
save(no_feed, file = paste0(output_path, "/6_non_nutritive_visit_analysis/no_feed.rda"))
save(no_feed_desc, file = paste0(output_path, "/6_non_nutritive_visit_analysis/no_feed_desc.rda"))

# Save first day summaries as CSV (examples)
write.csv(non_nutritive_desc[[1]], 
          paste0(output_path, "/6_non_nutritive_visit_analysis/non_nutritive_first_day.csv"), 
          row.names = FALSE)

write.csv(no_feed_desc[[1]], 
          paste0(output_path, "/6_non_nutritive_visit_analysis/no_feed_first_day.csv"), 
          row.names = FALSE)