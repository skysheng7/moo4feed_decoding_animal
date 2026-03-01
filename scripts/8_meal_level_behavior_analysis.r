################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 8. Meal level behavior analysis
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/meal_behavior_analysis.html
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

# ---- STEP 1: Prepare Meal-Labeled Data ----
# Load your cleaned data from previous analysis
output_path <- "results"
load(paste0(output_path, "/1_data_cleaning/clean_feed.rda"))
load(paste0(output_path, "/1_data_cleaning/clean_comb.rda"))

# Label visits with meals (see Tutorial 2 for details)
load(paste0(output_path, "/2_meal_clustering/labeled_visits.rda"))
load(paste0(output_path, "/4_replacement_detection/replacements.rda"))

# ---- STEP 2: Analyze Non-Nutritive Visits Within Meals ----
my_qc_config <- qc_config(calibration_error = 0.5)

meal_visits <- meal_non_nutritive_summary(
  data = labeled_visits,
  cfg = my_qc_config
)

# View results
head(meal_visits[[1]])

# Combine all days
all_meal_visits <- do.call(rbind, meal_visits)

# Find animals with high exploratory behavior
high_exploratory <- all_meal_visits |>
  dplyr::group_by(cow) |>
  dplyr::summarise(
    avg_non_nutritive = mean(mean_non_nutritive_per_meal, na.rm = TRUE),
    avg_empty_bin = mean(mean_empty_bin_per_meal, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(avg_non_nutritive))

print(high_exploratory)

# ---- STEP 3: Analyze Actor/Reactor Roles Within Meals ----
# Label combined data with meals, but including both feed and water data
labeled_comb <- meal_label_visits(
  data = clean_comb,
  eps = NULL,
  min_pts = 2,
  method = "gmm",
  eps_scope = "all_animals",
  lower_bound = NULL,
  upper_bound = NULL,
  use_log_transform = TRUE,
  log_multiplier = 20,
  log_offset = 1
)

# Analyze roles within meals
meal_roles <- meal_replacement_roles(
  visit_data = labeled_comb,
  replacement_data = replacements,
  time_tolerance = 1
)

# View meal-level results
head(meal_roles[[1]])

# ---- STEP 4: Get Daily Role Summaries ----
daily_roles <- meal_replacement_roles_summary(meal_roles)

# View daily summaries
print(daily_roles[[1]])

# Combine all days
all_daily_roles <- do.call(rbind, daily_roles)

# Find dominant/subordinate animals
dominance_summary <- all_daily_roles |>
  dplyr::group_by(cow) |>
  dplyr::summarise(
    avg_pct_actor = mean(mean_pct_actor, na.rm = TRUE),
    avg_pct_reactor = mean(mean_pct_reactor, na.rm = TRUE),
    total_meals = sum(total_meals),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dominance_ratio = avg_pct_actor / (avg_pct_reactor + 0.01)
  ) |>
  dplyr::arrange(desc(dominance_ratio))

print(dominance_summary)

# ---- STEP 5: Visualize Results ----
# Distribution of non-nutritive visits
ggplot(all_meal_visits, aes(x = mean_non_nutritive_per_meal)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Non-Nutritive Visits Per Meal",
    x = "Average Non-Nutritive Visits Per Meal",
    y = "Frequency"
  ) +
  theme_minimal()

# Non-nutritive visits bar plot by animal (first day example)
first_day_data <- meal_visits[[1]] |>
  dplyr::arrange(desc(median_non_nutritive_per_meal)) |>
  head(50)

ggplot(first_day_data, aes(x = reorder(cow, median_non_nutritive_per_meal), 
                           y = median_non_nutritive_per_meal)) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Average Non-Nutritive Visits Per Meal by Animal",
    subtitle = "Animals sorted by median non-nutritive visits per meal (first day example)",
    x = "Animal ID",
    y = "Average Non-Nutritive Visits Per Meal"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

# Empty bin visits bar plot by animal (first day example)
first_day_empty_bin <- meal_visits[[1]] |>
  dplyr::arrange(desc(median_empty_bin_per_meal)) |>
  head(50)

ggplot(first_day_empty_bin, aes(x = reorder(cow, median_empty_bin_per_meal), 
                                y = median_empty_bin_per_meal)) +
  geom_bar(stat = "identity", fill = "olivedrab3", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Average Empty Bin Visits Per Meal by Animal",
    subtitle = "Animals sorted by median empty bin visits per meal (first day example)",
    x = "Animal ID",
    y = "Average Empty Bin Visits Per Meal"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

# Actor vs Reactor diverging bar plot (first day example)
first_day_roles <- daily_roles[[1]]

# Reshape data to long format and make actor values negative for diverging plot
plot_data <- dplyr::bind_rows(
  first_day_roles |> 
    dplyr::select(cow, pct = median_pct_actor) |> 
    dplyr::mutate(role = "Actor", plot_value = -pct),
  first_day_roles |> 
    dplyr::select(cow, pct = median_pct_reactor) |> 
    dplyr::mutate(role = "Reactor", plot_value = pct)
)

# Sort animals by their actor percentage for a cleaner chart
cow_order <- first_day_roles |>
  dplyr::arrange(median_pct_actor) |>
  dplyr::pull(cow)

plot_data$cow <- factor(plot_data$cow, levels = cow_order)

# Create diverging bar chart
ggplot(plot_data, aes(x = cow, y = plot_value, fill = role)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("Actor" = "#e78ac3", "Reactor" = "#a6d854")) +
  scale_y_continuous(labels = abs) + # Show absolute values on axis
  labs(
    title = "Actor vs Reactor Roles by Animal",
    subtitle = "First day: Diverging bars show median % visits as actor (left) vs reactor (right)",
    x = "Animal ID",
    y = "Median % Visits",
    fill = "Role"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 8),
    legend.position = "top"
  )

# Actor-Reactor bar plot (first day example)
first_day_roles_sorted <- first_day_roles |>
  dplyr::filter(median_pct_actor_reactor > 0) |>
  dplyr::arrange(desc(median_pct_actor_reactor))

ggplot(first_day_roles_sorted, aes(x = reorder(cow, median_pct_actor_reactor), 
                                   y = median_pct_actor_reactor)) +
  geom_bar(stat = "identity", fill = "#edb870", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Median % Visits as Both Actor and Reactor by Animal",
    subtitle = "Animals with non-zero median_pct_actor_reactor, sorted high to low (first day example)",
    x = "Animal ID",
    y = "Median % Visits as Actor-Reactor"
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
if (!dir.exists(paste0(output_path, "/8_meal_level_behavior_analysis"))) {
  dir.create(paste0(output_path, "/8_meal_level_behavior_analysis"), recursive = TRUE)
}

# Save meal-level non-nutritive visits by day as RDA
save(meal_visits, file = paste0(output_path, "/8_meal_level_behavior_analysis/meal_visits.rda"))

# Save meal roles by day as RDA
save(meal_roles, file = paste0(output_path, "/8_meal_level_behavior_analysis/meal_roles.rda"))

# Save daily role summaries by day as RDA
save(daily_roles, file = paste0(output_path, "/8_meal_level_behavior_analysis/daily_roles.rda"))

# Save combined summaries as CSV
write.csv(all_meal_visits, 
          paste0(output_path, "/8_meal_level_behavior_analysis/all_meal_visits.csv"), 
          row.names = FALSE)

write.csv(high_exploratory, 
          paste0(output_path, "/8_meal_level_behavior_analysis/high_exploratory.csv"), 
          row.names = FALSE)

write.csv(all_daily_roles, 
          paste0(output_path, "/8_meal_level_behavior_analysis/all_daily_roles.csv"), 
          row.names = FALSE)

write.csv(dominance_summary, 
          paste0(output_path, "/8_meal_level_behavior_analysis/dominance_summary.csv"), 
          row.names = FALSE)

# Save first day examples
write.csv(meal_visits[[1]], 
          paste0(output_path, "/8_meal_level_behavior_analysis/meal_visits_first_day.csv"), 
          row.names = FALSE)

write.csv(daily_roles[[1]], 
          paste0(output_path, "/8_meal_level_behavior_analysis/daily_roles_first_day.csv"), 
          row.names = FALSE)