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

# Or use your own cleaned data from previous tutorials:
# clean_feed <- your_cleaned_feed_data

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