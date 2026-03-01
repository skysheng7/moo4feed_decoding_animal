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

# ---- STEP 2: Create Time-Based Activity Matrices (one day at a time to save RAM) ----
# Each data frame in the list is processed individually and saved to disk immediately.
# This avoids holding all matrices in memory at once.

matrix_out_path <- paste0(output_path, "/5_synchronicity_analysis/matrices")
if (!dir.exists(matrix_out_path)) dir.create(matrix_out_path, recursive = TRUE)

# --- Feed matrices ---
feed_matrix_files <- character(length(clean_feed))
for (i in seq_along(clean_feed)) {
  day_label <- names(clean_feed)[i]
  if (is.null(day_label) || day_label == "") day_label <- sprintf("day%04d", i)
  
  mat_list <- matrix_process(
    data_list = clean_feed[i],
    type = "feed",
    resolution = "sec",
    id_col = id_col2(),
    start_col = start_col2(),
    end_col = end_col2(),
    bin_col = bin_col2(),
    start_weight_col = start_weight_col2(),
    end_weight_col = end_weight_col2(),
    bins_feed = bins_feed2()
  )
  feed_matrix_i <- mat_list[[1]]
  out_file <- paste0(matrix_out_path, "/feed_matrix_", day_label, ".rda")
  save(feed_matrix_i, file = out_file)
  feed_matrix_files[i] <- out_file
  rm(mat_list, feed_matrix_i); gc()
  cat(sprintf("Feed matrix saved: %s (%d/%d)\n", day_label, i, length(clean_feed)))
}

# --- Water matrices ---
water_matrix_files <- character(length(clean_water))
for (i in seq_along(clean_water)) {
  day_label <- names(clean_water)[i]
  if (is.null(day_label) || day_label == "") day_label <- sprintf("day%04d", i)
  
  mat_list <- matrix_process(
    data_list = clean_water[i],
    type = "drink",
    resolution = "sec",
    id_col = id_col2(),
    start_col = start_col2(),
    end_col = end_col2(),
    bin_col = bin_col2(),
    bins_wat = bins_wat2()
  )
  water_matrix_i <- mat_list[[1]]
  out_file <- paste0(matrix_out_path, "/water_matrix_", day_label, ".rda")
  save(water_matrix_i, file = out_file)
  water_matrix_files[i] <- out_file
  rm(mat_list, water_matrix_i); gc()
  cat(sprintf("Water matrix saved: %s (%d/%d)\n", day_label, i, length(clean_water)))
}

# --- Combined (feed + water) matrices ---
combined_matrix_files <- character(length(clean_comb))
for (i in seq_along(clean_comb)) {
  day_label <- names(clean_comb)[i]
  if (is.null(day_label) || day_label == "") day_label <- sprintf("day%04d", i)
  
  mat_list <- matrix_process(
    data_list = clean_comb[i],
    type = "feed_and_drink",
    resolution = "sec",
    id_col = id_col2(),
    start_col = start_col2(),
    end_col = end_col2(),
    bin_col = bin_col2(),
    bins_feed = bins_feed2(),
    bins_wat = bins_wat2()
  )
  combined_matrix_i <- mat_list[[1]]
  out_file <- paste0(matrix_out_path, "/combined_matrix_", day_label, ".rda")
  save(combined_matrix_i, file = out_file)
  combined_matrix_files[i] <- out_file
  rm(mat_list, combined_matrix_i); gc()
  cat(sprintf("Combined matrix saved: %s (%d/%d)\n", day_label, i, length(clean_comb)))
}

# Free the raw data lists — no longer needed
rm(clean_feed, clean_water, clean_comb); gc()

# ---- STEP 3: Pair-wise Co-Occurrence Analysis (one day at a time to save RAM) ----
# Load each saved matrix, run synch_pair_analysis, save the per-day result, then free memory.

pair_out_path <- paste0(output_path, "/5_synchronicity_analysis/pair_results")
if (!dir.exists(pair_out_path)) dir.create(pair_out_path, recursive = TRUE)

# --- Feed pair analysis ---
pair_feed_files <- character(length(feed_matrix_files))
for (i in seq_along(feed_matrix_files)) {
  day_label <- sub(".*feed_matrix_(.+)\\.rda$", "\\1", feed_matrix_files[i])
  
  load(feed_matrix_files[i])  # loads: feed_matrix_i
  res <- synch_pair_analysis(
    matrix_data = list(feed_matrix_i),
    type = "feed",
    resolution = "sec",
    id_col = id_col2()
  )
  pair_feed_i <- list(
    bout         = res$bout[[1]],
    total_time   = res$total_time[[1]],
    avg_duration = res$avg_duration[[1]]
  )
  out_file <- paste0(pair_out_path, "/pair_feed_", day_label, ".rda")
  save(pair_feed_i, file = out_file)
  pair_feed_files[i] <- out_file
  rm(feed_matrix_i, res, pair_feed_i); gc()
  cat(sprintf("Pair feed result saved: %s (%d/%d)\n", day_label, i, length(feed_matrix_files)))
}

# --- Water pair analysis ---
pair_water_files <- character(length(water_matrix_files))
for (i in seq_along(water_matrix_files)) {
  day_label <- sub(".*water_matrix_(.+)\\.rda$", "\\1", water_matrix_files[i])
  
  load(water_matrix_files[i])  # loads: water_matrix_i
  res <- synch_pair_analysis(
    matrix_data = list(water_matrix_i),
    type = "drink",
    resolution = "sec",
    id_col = id_col2()
  )
  pair_water_i <- list(
    bout         = res$bout[[1]],
    total_time   = res$total_time[[1]],
    avg_duration = res$avg_duration[[1]]
  )
  out_file <- paste0(pair_out_path, "/pair_water_", day_label, ".rda")
  save(pair_water_i, file = out_file)
  pair_water_files[i] <- out_file
  rm(water_matrix_i, res, pair_water_i); gc()
  cat(sprintf("Pair water result saved: %s (%d/%d)\n", day_label, i, length(water_matrix_files)))
}

# --- Combined pair analysis ---
pair_combined_files <- character(length(combined_matrix_files))
for (i in seq_along(combined_matrix_files)) {
  day_label <- sub(".*combined_matrix_(.+)\\.rda$", "\\1", combined_matrix_files[i])
  
  load(combined_matrix_files[i])  # loads: combined_matrix_i
  res <- synch_pair_analysis(
    matrix_data = list(combined_matrix_i),
    type = "feed_and_drink",
    resolution = "sec",
    id_col = id_col2()
  )
  pair_combined_i <- list(
    bout         = res$bout[[1]],
    total_time   = res$total_time[[1]],
    avg_duration = res$avg_duration[[1]]
  )
  out_file <- paste0(pair_out_path, "/pair_combined_", day_label, ".rda")
  save(pair_combined_i, file = out_file)
  pair_combined_files[i] <- out_file
  rm(combined_matrix_i, res, pair_combined_i); gc()
  cat(sprintf("Pair combined result saved: %s (%d/%d)\n", day_label, i, length(combined_matrix_files)))
}

# ---- STEP 4: Spatial Neighbor Proximity Analysis (one day at a time to save RAM) ----
cat("Current bin layout:\n")
cat(bin_layout2(), "\n")

neighbor_out_path <- paste0(output_path, "/5_synchronicity_analysis/neighbor_results")
if (!dir.exists(neighbor_out_path)) dir.create(neighbor_out_path, recursive = TRUE)

neighbor_files <- character(length(combined_matrix_files))
for (i in seq_along(combined_matrix_files)) {
  day_label <- sub(".*combined_matrix_(.+)\\.rda$", "\\1", combined_matrix_files[i])
  
  load(combined_matrix_files[i])  # loads: combined_matrix_i
  res <- synch_neighbor_analysis(
    matrix_data = list(combined_matrix_i),
    bin_layout = bin_layout2(),
    type = "feed_and_drink",
    resolution = "sec",
    id_col = id_col2()
  )
  neighbor_i <- list(
    bout         = res$bout[[1]],
    total_time   = res$total_time[[1]],
    avg_duration = res$avg_duration[[1]]
  )
  out_file <- paste0(neighbor_out_path, "/neighbor_", day_label, ".rda")
  save(neighbor_i, file = out_file)
  neighbor_files[i] <- out_file
  rm(combined_matrix_i, res, neighbor_i); gc()
  cat(sprintf("Neighbor result saved: %s (%d/%d)\n", day_label, i, length(combined_matrix_files)))
}

# Example: inspect first day from saved results
load(pair_combined_files[1])   # loads: pair_combined_i
load(neighbor_files[1])        # loads: neighbor_i
example_animal1 <- rownames(pair_combined_i$total_time)[1]
example_animal2 <- rownames(pair_combined_i$total_time)[2]
cat(sprintf(
  "Comparison for animals %s and %s (first day):\nCo-occurrence: %d bouts, %d seconds total, %.1f sec/bout avg\nNeighbor: %d bouts, %d seconds total, %.1f sec/bout avg\n",
  example_animal1, example_animal2,
  pair_combined_i$bout[example_animal1, example_animal2],
  pair_combined_i$total_time[example_animal1, example_animal2],
  pair_combined_i$avg_duration[example_animal1, example_animal2],
  neighbor_i$bout[example_animal1, example_animal2],
  neighbor_i$total_time[example_animal1, example_animal2],
  neighbor_i$avg_duration[example_animal1, example_animal2]
))
rm(pair_combined_i, neighbor_i); gc()

# ---- STEP 5: Convert Matrices to Tidy Data Frames (aggregate across all days) ----
# Load each per-day result, convert to df, accumulate, then free memory.

# Helper: convert a single per-day result list to a data frame of pair rows
synch_result_to_df <- function(res_i) {
  bout_mat  <- res_i$bout
  time_mat  <- res_i$total_time
  avg_mat   <- res_i$avg_duration
  animals   <- rownames(bout_mat)
  pairs     <- which(upper.tri(bout_mat), arr.ind = TRUE)
  data.frame(
    animal1      = animals[pairs[, 1]],
    animal2      = animals[pairs[, 2]],
    bouts        = bout_mat[pairs],
    total_time   = time_mat[pairs],
    avg_duration = avg_mat[pairs],
    stringsAsFactors = FALSE
  )
}

# --- Aggregate feed pair results ---
pair_df_list <- vector("list", length(pair_feed_files))
for (i in seq_along(pair_feed_files)) {
  load(pair_feed_files[i])   # loads: pair_feed_i
  pair_df_list[[i]] <- synch_result_to_df(pair_feed_i)
  rm(pair_feed_i); gc()
}
pair_df_all <- do.call(rbind, pair_df_list)
pair_df <- aggregate(
  cbind(bouts, total_time) ~ animal1 + animal2,
  data = pair_df_all,
  FUN = sum
)
pair_df$avg_duration <- ifelse(pair_df$bouts > 0, pair_df$total_time / pair_df$bouts, 0)
pair_df <- pair_df[order(pair_df$total_time, decreasing = TRUE), ]
rm(pair_df_list, pair_df_all); gc()

# --- Aggregate neighbor results ---
neighbor_df_list <- vector("list", length(neighbor_files))
for (i in seq_along(neighbor_files)) {
  load(neighbor_files[i])   # loads: neighbor_i
  neighbor_df_list[[i]] <- synch_result_to_df(neighbor_i)
  rm(neighbor_i); gc()
}
neighbor_df_all <- do.call(rbind, neighbor_df_list)
neighbor_df <- aggregate(
  cbind(bouts, total_time) ~ animal1 + animal2,
  data = neighbor_df_all,
  FUN = sum
)
neighbor_df$avg_duration <- ifelse(neighbor_df$bouts > 0, neighbor_df$total_time / neighbor_df$bouts, 0)
neighbor_df <- neighbor_df[order(neighbor_df$total_time, decreasing = TRUE), ]
rm(neighbor_df_list, neighbor_df_all); gc()

# ---- STEP 6: Find Most/Least Synchronized Pairs ----
# Get top 10 most synchronized feeding pairs
top_pairs <- head(pair_df, 10)
print(top_pairs)

# Get top 10 least synchronized feeding pairs
least_pairs <- pair_df[order(pair_df$total_time, decreasing = FALSE), ]
print(head(least_pairs, 10))

# Filter pairs with high synchronicity (e.g., >100 seconds together)
high_sync_pairs <- pair_df[pair_df$total_time > 100, ]

# ---- STEP 7: Compare Neighbor Preference to Total Co-Occurrence ----
# Build neighbor_compare from aggregated combined pair df and neighbor df.

# Aggregate combined pair results across all days
pair_combined_df_list <- vector("list", length(pair_combined_files))
for (i in seq_along(pair_combined_files)) {
  load(pair_combined_files[i])   # loads: pair_combined_i
  pair_combined_df_list[[i]] <- synch_result_to_df(pair_combined_i)
  rm(pair_combined_i); gc()
}
pair_combined_df_all <- do.call(rbind, pair_combined_df_list)
pair_combined_df <- aggregate(
  cbind(bouts, total_time) ~ animal1 + animal2,
  data = pair_combined_df_all,
  FUN = sum
)
pair_combined_df$avg_duration <- ifelse(
  pair_combined_df$bouts > 0, pair_combined_df$total_time / pair_combined_df$bouts, 0
)
rm(pair_combined_df_list, pair_combined_df_all); gc()

# Merge co-occurrence and neighbor totals
neighbor_compare <- merge(
  pair_combined_df[, c("animal1", "animal2", "total_time")],
  neighbor_df[, c("animal1", "animal2", "total_time")],
  by = c("animal1", "animal2"),
  all.x = TRUE,
  suffixes = c("_cooccurrence", "_neighbor")
)
neighbor_compare$total_time_neighbor[is.na(neighbor_compare$total_time_neighbor)] <- 0
neighbor_compare$neighbor_ratio <- ifelse(
  neighbor_compare$total_time_cooccurrence > 0,
  neighbor_compare$total_time_neighbor / neighbor_compare$total_time_cooccurrence,
  0
)
names(neighbor_compare)[names(neighbor_compare) == "total_time_cooccurrence"] <- "cooccurrence_time"
names(neighbor_compare)[names(neighbor_compare) == "total_time_neighbor"]     <- "neighbor_time"
neighbor_compare <- neighbor_compare[order(neighbor_compare$neighbor_ratio, decreasing = TRUE), ]

# neighbor_compare contains:
# - cooccurrence_time: Total time active together (feed OR water) anywhere
# - neighbor_time: Time at neighboring bins (feed OR water)
# - neighbor_ratio: neighbor_time / cooccurrence_time (0 to 1)

# Find pairs that prefer neighboring bins (high ratio)
high_neighbor_preference <- neighbor_compare[neighbor_compare$neighbor_ratio > 0.5, ]

# ---- STEP 8: Visualize Results ----
# Create a symmetric dataset for the heatmap to show all pairs
heatmap_data <- rbind(
  pair_df,
  pair_df |> mutate(temp = animal1, animal1 = animal2, animal2 = temp) |> select(-temp)
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
    subtitle = "Total time spent feeding together across all days (clustered by similarity)",
    x = "Animal ID",
    y = "Animal ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Visualize neighbor feeding time as a heatmap (clustered by similarity)
# Create a symmetric dataset for the neighbor heatmap
neighbor_heatmap_data <- rbind(
  neighbor_df,
  neighbor_df |> dplyr::mutate(temp = animal1, animal1 = animal2, animal2 = temp) |> dplyr::select(-temp)
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
    subtitle = "Total time spent feeding or drinking as neighbors across all days (clustered by similarity)",
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
# Per-day matrices and per-day pair/neighbor results are already saved to disk
# during the iteration loops above (in matrices/, pair_results/, neighbor_results/).
# Here we save the aggregated summary data frames.

# Save aggregated data frames as RDA
save(pair_df,          file = paste0(output_path, "/5_synchronicity_analysis/pair_df.rda"))
save(neighbor_df,      file = paste0(output_path, "/5_synchronicity_analysis/neighbor_df.rda"))
save(neighbor_compare, file = paste0(output_path, "/5_synchronicity_analysis/neighbor_compare.rda"))
save(pair_combined_df, file = paste0(output_path, "/5_synchronicity_analysis/pair_combined_df.rda"))

# Save file path vectors so downstream scripts can reload per-day results if needed
save(feed_matrix_files,    file = paste0(output_path, "/5_synchronicity_analysis/feed_matrix_files.rda"))
save(water_matrix_files,   file = paste0(output_path, "/5_synchronicity_analysis/water_matrix_files.rda"))
save(combined_matrix_files,file = paste0(output_path, "/5_synchronicity_analysis/combined_matrix_files.rda"))
save(pair_feed_files,      file = paste0(output_path, "/5_synchronicity_analysis/pair_feed_files.rda"))
save(pair_water_files,     file = paste0(output_path, "/5_synchronicity_analysis/pair_water_files.rda"))
save(pair_combined_files,  file = paste0(output_path, "/5_synchronicity_analysis/pair_combined_files.rda"))
save(neighbor_files,       file = paste0(output_path, "/5_synchronicity_analysis/neighbor_files.rda"))
