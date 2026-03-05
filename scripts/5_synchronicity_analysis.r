################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 5. Synchronicity analysis
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/synchronicity_analysis.html
################################################################
# ---- SETUP: Global Variables (REQUIRED FIRST!) ----
library(moo4feed)
library(ggplot2)
library(dplyr)

# Set up column names and bin configuration to match your data structure
set_global_cols(
  # Time zone
  tz = "America/Vancouver",        # Your timezone
  
  # Column names in your cleaned data
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
  bins_feed = 1:30,                # All feed bin IDs in your barn
  bins_wat = 1:5,                  # All water bin IDs in your barn
  bin_offset = 100,                # Add this to water bin IDs to avoid conflicts
  
  # Physical bin layout for spatial neighbor analysis
  # Rows separated by "\n", bins within rows separated by "-"
  # Only bins in the same row are considered spatial neighbors (left/right adjacency)
  bin_layout = "1-2-3-4-5-6-101-102-7-8-9-10-11-12-13-14-15-16-17-18-103-104-19-20-21-22-23-24-25-26-27-28-29-30-105"
)

# ---- STEP 1: Load Cleaned Data ----
# Load data from previous analysis
output_path <- "results"
load(paste0(output_path, "/1_data_cleaning/clean_feed.rda"))
load(paste0(output_path, "/1_data_cleaning/clean_water.rda"))
load(paste0(output_path, "/1_data_cleaning/clean_comb.rda"))

# ---- STEP 2: Create Time-Based Activity Matrices ----
# Process feed data to create time-based matrices (for pair-wise analysis)
feed_matrices <- matrix_process(
  data_list = clean_feed,          # Your cleaned feed data
  type = "feed",                   # Data type: "feed" or "drink"
  resolution = "sec",              # Time resolution: "sec" (detailed) or "min" (faster)
  id_col = id_col2(),              # Animal ID column (from global vars)
  start_col = start_col2(),        # Visit start time column (from global vars)
  end_col = end_col2(),            # Visit end time column (from global vars)
  bin_col = bin_col2(),            # Bin ID column (from global vars)
  start_weight_col = start_weight_col2(),  # Start weight column (from global vars)
  end_weight_col = end_weight_col2(),      # End weight column (from global vars)
  bins_feed = bins_feed2()         # Valid feed bin IDs (from global vars)
)

# Process water data to create time-based matrices (for pair-wise analysis)
water_matrices <- matrix_process(
  data_list = clean_water,         # Your cleaned water data
  type = "drink",                  # Data type for water/drinking
  resolution = "sec",              # Must match resolution used for feed_matrices
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  bins_wat = bins_wat2()           # Valid water bin IDs (from global vars)
)

# IMPORTANT: Process combined feed and water data for neighbor analysis
# Water bins are often between feed bins, so analyze them together for spatial relationships
# Use clean_comb dataset (already combined) or create your own with combine_feed_water()
combined_matrices <- matrix_process(
  data_list = clean_comb,          # Combined feed and water data
  type = "feed_and_drink",         # Combined type
  resolution = "sec",              # Must match resolution used above
  id_col = id_col2(),
  start_col = start_col2(),
  end_col = end_col2(),
  bin_col = bin_col2(),
  bins_feed = bins_feed2(),
  bins_wat = bins_wat2()
)

# ---- STEP 3: Pair-wise Co-Occurrence Analysis ----
# Analyze feeding synchronicity (animals feeding at same time, any bins)
pair_feed_results <- synch_pair_analysis(
  matrix_data = feed_matrices,     # Output from matrix_process()
  type = "feed",                   # Data type: "feed" or "drink"
  resolution = "sec",              # Must match matrix_process() resolution
  id_col = id_col2()               # Animal ID column (from global vars)
)

# Results contain three matrices/lists:
names(pair_feed_results)  # bout, total_time, avg_duration

# Analyze drinking synchronicity (animals drinking at same time, any bins)
pair_water_results <- synch_pair_analysis(
  matrix_data = water_matrices,    # Output from matrix_process()
  type = "drink",                  # Data type for water/drinking
  resolution = "sec",              # Must match matrix_process() resolution
  id_col = id_col2()
)

# IMPORTANT: Analyze combined feed+water synchronicity for comparison with neighbor analysis
pair_combined_results <- synch_pair_analysis(
  matrix_data = combined_matrices, # Output from matrix_process() with combined data
  type = "feed_and_drink",         # Combined type
  resolution = "sec",              # Must match matrix_process() resolution
  id_col = id_col2()
)

# Example: inspect first day matrices from combined pair analysis
combined_bout_matrix <- pair_combined_results$bout[[1]]
combined_time_matrix <- pair_combined_results$total_time[[1]]

# Access pair analysis results (matrices or lists of matrices for multi-day)
# For single day: each element is a matrix
# For multi-day: each element is a list with one matrix per day
bout_counts <- pair_feed_results$bout           # Number of distinct bouts together
total_times <- pair_feed_results$total_time    # Total time together (seconds or minutes)
avg_durations <- pair_feed_results$avg_duration # Average duration per bout

# Example: Access first day's bout matrix
if (is.list(pair_feed_results$bout)) {
  day1_bouts <- pair_feed_results$bout[[1]]    # Multi-day data
} else {
  day1_bouts <- pair_feed_results$bout         # Single-day data
}

# ---- STEP 4: Spatial Neighbor Proximity Analysis ----
# Check bin layout
cat("Current bin layout:\n")
cat(bin_layout2(), "\n")

# IMPORTANT: Use combined matrices for neighbor analysis!
# Water bins are often between feed bins, so we analyze them together
neighbor_results <- synch_neighbor_analysis(
  matrix_data = combined_matrices, # Output from matrix_process() with combined data
  bin_layout = bin_layout2(),      # Physical bin arrangement (from global vars)
  type = "feed_and_drink",         # Combined type
  resolution = "sec",              # Must match matrix_process() resolution
  id_col = id_col2()               # Animal ID column (from global vars)
)

# Results contain three matrices/lists:
names(neighbor_results)  # bout, total_time, avg_duration

# Access neighbor analysis results
neighbor_bouts <- neighbor_results$bout         # Bouts at neighboring bins
neighbor_times <- neighbor_results$total_time  # Time at neighboring bins
neighbor_avg <- neighbor_results$avg_duration  # Average neighbor bout duration

# Compare co-occurrence vs neighbor patterns (example)
# Extract matrices for the first day
combined_bout_matrix <- pair_combined_results$bout[[1]]
combined_time_matrix <- pair_combined_results$total_time[[1]]
neighbor_bout_matrix <- neighbor_results$bout[[1]]
neighbor_time_matrix <- neighbor_results$total_time[[1]]

example_animal1 <- rownames(combined_time_matrix)[1]
example_animal2 <- rownames(combined_time_matrix)[2]
cat(sprintf("Comparison for animals %s and %s (first day):\n
Co-occurrence (feeding OR drinking at same time, any bins): %d bouts, %d seconds total, %.1f seconds per bout on average
Neighbor proximity (feeding OR drinking at adjacent bins): %d bouts, %d seconds total, %.1f seconds per bout on average\n",
  example_animal1, example_animal2,
  combined_bout_matrix[example_animal1, example_animal2],
  combined_time_matrix[example_animal1, example_animal2],
  pair_combined_results$avg_duration[[1]][example_animal1, example_animal2],
  neighbor_bout_matrix[example_animal1, example_animal2],
  neighbor_time_matrix[example_animal1, example_animal2],
  neighbor_results$avg_duration[[1]][example_animal1, example_animal2]
))

# ---- STEP 5: Convert Matrices to Tidy Data Frames ----
# Convert pair analysis results to data frame for easier analysis
pair_feed_df <- synch_pairs_to_df(
  synch_results = pair_feed_results,  # Output from synch_pair_analysis()
  min_time = 0,                       # Minimum time threshold (0 = include all)
  sort_by = "total_time",             # Column to sort by: "total_time", "bouts", "avg_duration"
  decreasing = TRUE                   # TRUE = descending, FALSE = ascending
)

# Convert water pair analysis results to data frame
pair_water_df <- synch_pairs_to_df(
  synch_results = pair_water_results,  # Output from synch_pair_analysis() for water
  min_time = 0,
  sort_by = "total_time",
  decreasing = TRUE
)

# Convert combined feed+water pair analysis results to data frame
pair_combined_df <- synch_pairs_to_df(
  synch_results = pair_combined_results,  # Output from synch_pair_analysis() for combined
  min_time = 0,
  sort_by = "total_time",
  decreasing = TRUE
)

# Convert neighbor analysis results to data frame
neighbor_df <- synch_pairs_to_df(
  synch_results = neighbor_results,  # Output from synch_neighbor_analysis()
  min_time = 0,                      # Minimum neighbor time threshold
  sort_by = "total_time",            # Sort by time at neighboring bins
  decreasing = TRUE
)

# ---- STEP 6: Find Most/Least Synchronized Pairs ----
# Get top 10 most synchronized feeding pairs
top_pairs <- head(pair_feed_df, 10)
print(top_pairs)

# Get top 10 least synchronized feeding pairs
least_pairs <- synch_pairs_to_df(
  pair_feed_results,
  min_time = 0,
  sort_by = "total_time",
  decreasing = FALSE               # Least synchronized first
)
print(head(least_pairs, 10))

# Filter pairs with high synchronicity (e.g., >100 seconds together)
high_sync_pairs <- pair_feed_df[pair_feed_df$total_time > 100, ]

# ---- STEP 7: Compare Neighbor Preference to Total Co-Occurrence ----
# IMPORTANT: Compare combined feed+water co-occurrence with neighbor proximity
# This ensures we're comparing the same activity types (both feed and water)
neighbor_compare <- synch_neighbor_compare(
  pair_results = pair_combined_results, # Combined co-occurrence results
  neighbor_results = neighbor_results,  # Neighbor results (combined feed+water)
  min_cooccurrence = 0,                # Minimum co-occurrence time threshold (0 = all pairs)
  sort_by = "neighbor_ratio",          # Sort by neighbor preference ratio
  decreasing = TRUE                    # Highest preference first
)

# neighbor_compare contains:
# - cooccurrence_time: Total time active together (feed OR water) anywhere
# - neighbor_time: Time at neighboring bins (feed OR water)
# - neighbor_ratio: neighbor_time / cooccurrence_time (0 to 1)

# Find pairs that prefer neighboring bins (high ratio)
high_neighbor_preference <- neighbor_compare[neighbor_compare$neighbor_ratio > 0.5, ]

# ---- STEP 8: Visualize Results ----
# Create a symmetric dataset for the heatmap to show all pairs
heatmap_data <- rbind(
  pair_feed_df,
  pair_feed_df |> mutate(temp = animal1, animal1 = animal2, animal2 = temp) |> select(-temp)
)

# Convert to symmetric matrix for clustering
all_animals <- unique(c(heatmap_data$animal1, heatmap_data$animal2))
time_matrix <- matrix(0, nrow = length(all_animals), ncol = length(all_animals),
                      dimnames = list(all_animals, all_animals))

# Fill the matrix
for (i in seq_len(nrow(heatmap_data))) {
  time_matrix[as.character(heatmap_data$animal1[i]), 
              as.character(heatmap_data$animal2[i])] <- heatmap_data$total_time[i]
}

# Convert similarity to distance for clustering (higher similarity = lower distance)
max_val <- max(time_matrix, na.rm = TRUE)
dist_matrix <- max_val + 1 - time_matrix
diag(dist_matrix) <- 0  # Set diagonal to 0

# Perform hierarchical clustering
dist_obj <- as.dist(dist_matrix)
hc <- hclust(dist_obj, method = "ward.D2")
animal_order <- hc$labels[hc$order]

# Build a complete grid (including diagonal and zero-time pairs) for plotting
plot_grid <- expand.grid(
  animal1 = animal_order,
  animal2 = animal_order,
  stringsAsFactors = FALSE
)
plot_grid$total_time <- time_matrix[cbind(plot_grid$animal1, plot_grid$animal2)]
plot_grid$animal1 <- factor(plot_grid$animal1, levels = animal_order)
plot_grid$animal2 <- factor(plot_grid$animal2, levels = animal_order)

# Visualize the total time spent together as a heatmap using viridis palette
ggplot(plot_grid, aes(x = animal1, y = animal2, fill = total_time)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Total Time\n(seconds)", direction = -1) +
  labs(
    title = "Pair-wise Synchronicity Heatmap",
    subtitle = "Total time spent feeding together (clustered by similarity, first day)",
    x = "Animal ID",
    y = "Animal ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Visualize neighbor feeding time as a heatmap (clustered by similarity)
# Convert neighbor results to data frame
neighbor_df_viz <- synch_pairs_to_df(
  synch_results = neighbor_results,
  min_time = 0,
  sort_by = "total_time",
  decreasing = TRUE
)

# Create a symmetric dataset for the neighbor heatmap
neighbor_heatmap_data <- rbind(
  neighbor_df_viz,
  neighbor_df_viz |> dplyr::mutate(temp = animal1, animal1 = animal2, animal2 = temp) |> dplyr::select(-temp)
)

# Convert to symmetric matrix for clustering
all_animals_neighbor <- unique(c(neighbor_heatmap_data$animal1, neighbor_heatmap_data$animal2))
neighbor_time_matrix <- matrix(0, nrow = length(all_animals_neighbor), 
                                ncol = length(all_animals_neighbor),
                                dimnames = list(all_animals_neighbor, all_animals_neighbor))

# Fill the matrix
for (i in seq_len(nrow(neighbor_heatmap_data))) {
  neighbor_time_matrix[as.character(neighbor_heatmap_data$animal1[i]), 
                       as.character(neighbor_heatmap_data$animal2[i])] <- 
    neighbor_heatmap_data$total_time[i]
}

# Convert similarity to distance for clustering
max_val_neighbor <- max(neighbor_time_matrix, na.rm = TRUE)
neighbor_dist_matrix <- max_val_neighbor + 1 - neighbor_time_matrix
diag(neighbor_dist_matrix) <- 0

# Perform hierarchical clustering
neighbor_dist_obj <- as.dist(neighbor_dist_matrix)
neighbor_hc <- hclust(neighbor_dist_obj, method = "ward.D2")
neighbor_animal_order <- neighbor_hc$labels[neighbor_hc$order]

# Build a complete grid (including diagonal and zero-time pairs) for plotting
neighbor_plot_grid <- expand.grid(
  animal1 = neighbor_animal_order,
  animal2 = neighbor_animal_order,
  stringsAsFactors = FALSE
)
neighbor_plot_grid$total_time <- neighbor_time_matrix[cbind(neighbor_plot_grid$animal1, neighbor_plot_grid$animal2)]
neighbor_plot_grid$animal1 <- factor(neighbor_plot_grid$animal1, levels = neighbor_animal_order)
neighbor_plot_grid$animal2 <- factor(neighbor_plot_grid$animal2, levels = neighbor_animal_order)

# Visualize the total time spent feeding as neighbors as a heatmap
ggplot(neighbor_plot_grid, aes(x = animal1, y = animal2, fill = total_time)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Total Time\n(seconds)", direction = -1) +
  labs(
    title = "Neighbor Synchronicity Heatmap",
    subtitle = "Total time spent feeding or drinking as neighbors (clustered by similarity, first day)",
    x = "Animal ID",
    y = "Animal ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

################################################################
# Save results
################################################################
# Create output directory if it doesn't exist
if (!dir.exists(paste0(output_path, "/5_synchronicity_analysis"))) {
  dir.create(paste0(output_path, "/5_synchronicity_analysis"), recursive = TRUE)
}

# Save time-based activity matrices (intermediate outputs from matrix_process)
save(feed_matrices, file = paste0(output_path, "/5_synchronicity_analysis/feed_matrices.rda"))
save(water_matrices, file = paste0(output_path, "/5_synchronicity_analysis/water_matrices.rda"))
save(combined_matrices, file = paste0(output_path, "/5_synchronicity_analysis/combined_matrices.rda"))

# Save pair analysis results as RDA
save(pair_feed_results, file = paste0(output_path, "/5_synchronicity_analysis/pair_feed_results.rda"))
save(pair_water_results, file = paste0(output_path, "/5_synchronicity_analysis/pair_water_results.rda"))
save(pair_combined_results, file = paste0(output_path, "/5_synchronicity_analysis/pair_combined_results.rda"))

# Save neighbor analysis results as RDA
save(neighbor_results, file = paste0(output_path, "/5_synchronicity_analysis/neighbor_results.rda"))

# Save data frames as RDA
save(pair_feed_df, file = paste0(output_path, "/5_synchronicity_analysis/pair_feed_df.rda"))
save(pair_water_df, file = paste0(output_path, "/5_synchronicity_analysis/pair_water_df.rda"))
save(pair_combined_df, file = paste0(output_path, "/5_synchronicity_analysis/pair_combined_df.rda"))
save(neighbor_df, file = paste0(output_path, "/5_synchronicity_analysis/neighbor_df.rda"))
save(neighbor_compare, file = paste0(output_path, "/5_synchronicity_analysis/neighbor_compare.rda"))
