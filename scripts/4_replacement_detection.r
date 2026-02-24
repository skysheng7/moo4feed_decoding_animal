################################################################
# Package setup
#     - Installation guide from `moo4feed` package site: 
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 4. Replacement Detection
#   - Vignette with tutorial guide: 
#   https://www.skysheng.io/moo4feed/articles/replacement_detection.html
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
load(paste0(output_path, "/1_data_cleaning/clean_comb.rda"))

# ---- STEP 2: Detect Replacement Events ----
# Process replacement events for all days
replacements <- record_replacement_days(
  comb = clean_comb,                    # Your cleaned feed/water or both feed + water data
  cfg = qc_config(replacement_threshold = 26)  # Time gap (seconds) to classify replacement behavior
)

# Examine the first few replacement events
head(replacements[[1]])

# Summary of replacement events
cat("Replacement events per day:\n")
sapply(replacements, nrow)

# ---- STEP 3: Analyze Replacement Patterns ----
# Combine all days for analysis
all_replacements <- do.call(rbind, replacements)

# Animals that most frequently replace others (actors)
top_actors <- all_replacements |>
  dplyr::count(actor_cow, sort = TRUE, name = "times_replaced_others") |>
  head(5)

cat("Top cows that most frequently displace others:\n")
print(top_actors)

# Animals that are most frequently replaced (reactors)
top_reactors <- all_replacements |>
  dplyr::count(reactor_cow, sort = TRUE, name = "times_replaced") |>
  head(5)

cat("\nTop cows that are most frequently displaced:\n")
print(top_reactors)

# Analyze replacement timing throughout the day
all_replacements$hour <- lubridate::hour(all_replacements$time)

hourly_replacements <- all_replacements |>
  dplyr::count(hour, name = "replacement_count")

cat("\nReplacement events by hour of day:\n")
print(hourly_replacements)

# ---- STEP 4: Visualize Results ----
# Replacement events by hour
ggplot(hourly_replacements, aes(x = hour, y = replacement_count)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Replacement Events by Hour of Day",
    subtitle = "Shows when displacement behavior most frequently occurs",
    x = "Hour of Day (0-23)",
    y = "Total Replacement Events"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.minor.x = element_blank()
  )

# Most active displacing animals bar plot
top_50_actors <- head(all_replacements |>
  dplyr::count(actor_cow, sort = TRUE, name = "times_replaced_others"), 50)

ggplot(top_50_actors, aes(x = reorder(actor_cow, times_replaced_others), y = times_replaced_others)) +
  geom_bar(stat = "identity", fill = "indianred", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Frequency of Initiating Replacements",
    subtitle = "Animals that most frequently pushed others away from bins",
    x = "Animal ID",
    y = "Number of Replacements Initiated"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold")
  )

################################################################
# Save results
################################################################
# Create output directory if it doesn't exist
if (!dir.exists(paste0(output_path, "/4_replacement_detection"))) {
  dir.create(paste0(output_path, "/4_replacement_detection"), recursive = TRUE)
}

# Save all replacements combined
write.csv(all_replacements, 
          paste0(output_path, "/4_replacement_detection/all_replacements.csv"), 
          row.names = FALSE)


# Save replacements by day as RDA
save(replacements, file = paste0(output_path, "/4_replacement_detection/replacements.rda"))