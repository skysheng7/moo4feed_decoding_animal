################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 2. Meal clustering
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/meal_clustering.html
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

# ---- STEP 1: Find Optimal Meal Interval ----
# Method 1: Simple percentile approach
eps_percentile <- meal_interval(
  data = your_data, 
  method = "percentile", 
  percentile = 0.9,                    # Try: 0.80, 0.93, 0.95, 0.99
  lower_bound = NULL, 
  upper_bound = NULL,
  id_col = id_col2(), 
  start_col = start_col2(), 
  end_col = end_col2(),
  tz = tz2()
)

# Method 2: Gaussian Mixture Model (more sophisticated)
eps_gmm <- meal_interval(
  data = your_data, 
  method = "gmm", 
  percentile = 0.9,
  lower_bound = NULL, 
  upper_bound = NULL,
  use_log_transform = TRUE,             # Try: FALSE
  log_multiplier = 20,                  # Try: 10, 30, 50
  log_offset = 1,                       # Try: 0.5, 2
  id_col = id_col2(), 
  start_col = start_col2(), 
  end_col = end_col2(),
  tz = tz2()
)

# Method 3: Conservative approach (uses both methods, takes minimum)
eps_both <- meal_interval(
  data = your_data, 
  method = "both", 
  percentile = 0.9,
  lower_bound = NULL, 
  upper_bound = NULL,
  id_col = id_col2(), 
  start_col = start_col2(), 
  end_col = end_col2(),
  tz = tz2()
)

# ---- STEP 2: Visualize Gap Distributions ----
# Visualize percentile method
p1 <- viz_eps_percentile(
  data = your_data, 
  percentile = 0.9,                    # Try different values!
  lower_bound = NULL, 
  upper_bound = NULL,
  xlim = 15,                           # Adjust for your data range
  id_col = id_col2(), 
  start_col = start_col2(), 
  end_col = end_col2(),
  tz = tz2()
)

# Visualize GMM method  
p2 <- viz_eps_gmm(
  data = your_data, 
  use_log_transform = TRUE,             # Try: FALSE
  xlim = 20,                            # Use smaller values for log transform
  show_components = TRUE,               # Try: FALSE
  id_col = id_col2(), 
  start_col = start_col2(), 
  end_col = end_col2(),
  tz = tz2()
)

# ---- STEP 3: Cluster Visits into Meals ----
# - Step 1 + 2 are optional, it's designed to help you find the optimal interval (eps) 
#   or find the best automatic method of identifyingoptimal eps for the clustering.
# - You can skip step 1 + 2 and just use the 2 functions below if you are confident.
# Option A: Just get meal summaries
meals <- cluster_meals(
  data = your_data,
  eps = NULL,                           # Auto-determine, or set specific value
  min_pts = 2,                          # Try: 3, 4, 5 for stricter clustering
  method = "gmm",                       # Try: "percentile", "both"
  eps_scope = "all_animals",            # Try: "one_animal_all_days", "one_animal_single_day"
  lower_bound = 5, 
  upper_bound = 60,
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  intake_col = intake_col2(),
  dur_col = duration_col2(),
  tz = tz2()
)

# Option B: Label individual visits with meal info (recommended!)
labeled_visits <- meal_label_visits(
  data = your_data,
  eps = NULL,                           # Auto-determine optimal interval
  min_pts = 2,                          # Minimum visits to form a meal
  method = "gmm",                       # Clustering method
  eps_scope = "all_animals",            # Scope for eps calculation
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  intake_col = intake_col2(),
  dur_col = duration_col2(),
  tz = tz2()
)

# ---- STEP 4: Visualize Meal Patterns ----
# Create timeline plots showing meals
meal_plots <- viz_meal_clusters(
  data = labeled_visits,
  point_size = 2,                       # Try: 1, 3, 4
  point_alpha = 0.7,                    # Try: 0.5, 0.8, 1.0
  color_palette = "Set 3",              # Try: "Dark 2", "Pastel 1", "Set 1"
  outlier_color = "grey50",             # Try: "red", "black"
  title_prefix = "Animal",              # Try: "Cow", ""
  text_size = 12,                       # Try: 10, 14, 16
  time_breaks = "4 hours",              # Try: "2 hours", "6 hours"
  id_col = id_col2(),
  start_col = start_col2(),
  tz = tz2()
)

# ---- STEP 5: Extract and Combine Specific Plots ----
# Get plots for specific animals
animal_plots <- extract_plots(
  plot_list = meal_plots, 
  animals = c("6084", "5120"),          # Your animal IDs
  dates = NULL                          # All dates, or specify: c("2020-10-31")
)

# Get plots for specific dates
date_plots <- extract_plots(
  plot_list = meal_plots,
  animals = NULL,                       # All animals
  dates = "2020-10-31"                  # Your specific date
)

# Combine multiple days for one animal
combined_plots <- combine_animal_plots(
  plot_list = meal_plots, 
  animal_id = "5124",                   # Your animal ID
  plots_per_page = 4,                   # Number of plots per page
  method = "vertical"                   # Try: "grid"
)

# Combine multiple animals for one date
date_combined <- combine_date_plots(
  plot_list = meal_plots,
  date = "2020-10-31",                  # Your specific date
  plots_per_page = 3,
  method = "vertical"
)

# ---- BONUS: Quick Analysis ----
# Check meal summary statistics
summary(meals$meal_duration)           # Meal durations in seconds
summary(meals$visit_count)             # Visits per meal
summary(meals$total_intake)            # Intake per meal
meals_summary <- table(meals$cow, meals$date)           # Meals per animal, per day
meals_summary

# Check labeling success on the first day
labeled_visits_summary <- table(labeled_visits[[1]]$meal_id == 0)    # TRUE = outliers, FALSE = assigned to meals
labeled_visits_summary