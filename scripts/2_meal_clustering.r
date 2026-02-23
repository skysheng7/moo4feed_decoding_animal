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

# load data
output_path <- "results"
load(paste0(output_path, "/1_data_cleaning/clean_feed.rda"))
your_data <- clean_feed

# ---- STEP 1: Find Optimal Meal Interval & Visualize Gap Distributions ----
# Visualize GMM method  
p_gmm_log_20 <- viz_eps_gmm(your_data, 
                            lower_bound = NULL,
                            upper_bound = NULL,
                            bins = 100,
                            colors = grDevices::hcl.colors(4, "Set 3"),
                            title_prefix = "Distribution of time gap between visits \n& GMM-based meal interval (eps)\n",
                            show_components = TRUE,
                            use_log_transform = TRUE,
                            log_multiplier = 20,
                            log_offset = 1,
                            xlim = 10)
print(p_gmm_log_20)


# ---- STEP 2: Cluster Visits into Meals ----
# - Step 1 is optional, it's designed to help you find the optimal interval (eps) 
# - You can skip step 1 and just use the 2 functions below if you are confident.
# Option A: Just get meal summaries
meal_summaries <- cluster_meals(data = your_data,
                            eps = NULL,  # Auto-determine eps using GMM method
                            min_pts = 2,  # 🎯 Try changing this to 3, 4, or 5!
                            method = "gmm",
                            percentile = 0.9,
                            eps_scope = "all_animals",
                            lower_bound = NULL,
                            upper_bound = NULL,
                            use_log_transform = TRUE,
                            log_multiplier = 20,
                            log_offset = 1)

# Look at the results
head(meal_summaries)

# Option B: Label individual visits with meal info (recommended!)
labeled_visits <- meal_label_visits(data = your_data,
                                   eps = NULL,
                                   min_pts = 2,
                                   method = "gmm",
                                   percentile = 0.9,
                                   eps_scope = "all_animals",
                                   lower_bound = NULL,
                                   upper_bound = NULL,
                                   use_log_transform = TRUE,
                                   log_multiplier = 20,
                                   log_offset = 1)

# ---- STEP 3: Visualize Meal Patterns ----
# Create timeline plots showing meals
meal_plots <- viz_meal_clusters(data = labeled_visits,
                               point_size = 2,
                               point_alpha = 0.7,
                               ncol_facet = 1,
                               date_format = "%Y-%m-%d",
                               time_breaks = "4 hours",
                               time_labels = "%H",
                               color_palette = "Set 3",  # 🎯 Try "Dark 2", "Pastel 1"!
                               outlier_color = "grey50",
                               title_prefix = "Cow",  # 🎯 Try "Animal" or ""!
                               text_size = 10,
                               title_size = NULL)

# ---- BONUS: Quick Analysis ----
# Check meal summary statistics
summary(meal_summaries$meal_duration)           # Meal durations in seconds
summary(meal_summaries$visit_count)             # Visits per meal
summary(meal_summaries$total_intake)            # Intake per meal
meal_summaries_summary <- table(meal_summaries$cow, meal_summaries$date)           # Meals per animal, per day
meal_summaries_summary

# Check labeling success on the first day
labeled_visits_success <- table(labeled_visits[[1]]$meal_id == 0)    # TRUE = outliers, FALSE = assigned to meals
labeled_visits_success

################################################################
# Save results
################################################################
# Create output directory if it doesn't exist
if (!dir.exists(paste0(output_path, "/2_meal_clustering"))) {
  dir.create(paste0(output_path, "/2_meal_clustering"), recursive = TRUE)
}

# Save GMM visualization plot
ggsave(paste0(output_path, "/2_meal_clustering/gmm_gap_distribution.pdf"), 
       p_gmm_log_20, width = 10, height = 6, units = "in")

# Save meal summaries as CSV (single data frame)
write.csv(meal_summaries, 
          paste0(output_path, "/2_meal_clustering/meal_summaries.csv"), 
          row.names = FALSE)

# Save labeled visits as RDA (list of data frames)
save(labeled_visits, file = paste0(output_path, "/2_meal_clustering/labeled_visits.rda"))

# Get all unique animal IDs from the data
all_animals <- unique(meal_summaries$cow)

# Export one PDF per animal with 3 plots per page
for (animal in all_animals) {
  combined_plots <- combine_animal_plots(
    plot_list = meal_plots, 
    animal_id = animal,
    plots_per_page = 3,
    method = "vertical"
  )
  
  # Save as PDF
  output_file <- paste0(output_path, "/2_meal_clustering/", animal, ".pdf")
  ggsave(output_file, combined_plots, width = 8.5, height = 11, units = "in")
}