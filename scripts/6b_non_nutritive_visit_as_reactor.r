################################################################
# Package setup
#     - Installation guide from `moo4feed` package site:
#     https://www.skysheng.io/moo4feed/index.html
################################################################

# install packages if you haven't done so
# install.packages("devtools")
# devtools::install_github("skysheng7/moo4feed")

################################################################
# 6b. Non-nutritive visit displacement classification
#   - For each non-nutritive visit, classify whether the cow
#     left because it was displaced (reactor) or left voluntarily
#   - Uses replacement events from step 4 to match
################################################################

# ---- SETUP: Global Variables (REQUIRED FIRST!) ----
library(moo4feed)
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
output_path <- "results"
load(paste0(output_path, "/1_data_cleaning/clean_feed.rda"))
load(paste0(output_path, "/4_replacement_detection/replacements.rda"))

# ---- STEP 2: Configuration ----
my_qc_config <- qc_config(
  calibration_error = 0.5    # Equipment measurement threshold (kg)
)
CALIBRATION_ERROR <- my_qc_config$calibration_error
MATCH_TOLERANCE <- 26  # seconds — same as replacement detection threshold

# ---- STEP 3: Combine Replacement Data With Date Lookup ----
# Combine all replacement events into one df for date-based lookup
all_rep <- do.call(rbind, replacements)
all_rep$date <- as.character(all_rep$date)
all_rep$reactor_cow <- as.integer(all_rep$reactor_cow)
rep_by_date <- split(all_rep, all_rep$date)

# ---- STEP 4: Label Each Non-Nutritive Visit ----
nn_visit_labeled <- list()

for (i in seq_along(clean_feed)) {
  day_feed <- clean_feed[[i]]

  # Filter non-nutritive visits (feed available but cow didn't eat)
  nn_visits <- day_feed |>
    dplyr::filter(intake < CALIBRATION_ERROR)

  if (nrow(nn_visits) == 0) {
    nn_visits$leave_reason <- character(0)
    nn_visit_labeled[[i]] <- nn_visits
    next
  }

  # Get date for this day
  current_date <- as.character(unique(nn_visits$date)[1])

  # Get replacement events for this day
  day_rep <- rep_by_date[[current_date]]

  # Default: all voluntary
  nn_visits$leave_reason <- "voluntary"

  if (!is.null(day_rep) && nrow(day_rep) > 0) {
    for (j in seq_len(nrow(nn_visits))) {
      visit <- nn_visits[j, ]

      # Find matching replacement: same cow as reactor, same bin,
      # replacement time within tolerance of visit end time
      matches <- day_rep |>
        dplyr::filter(
          reactor_cow == as.integer(visit$cow),
          bin == visit$bin,
          abs(as.numeric(difftime(time, visit$end, units = "secs"))) <= MATCH_TOLERANCE
        )

      if (nrow(matches) > 0) {
        nn_visits$leave_reason[j] <- "displaced"
      }
    }
  }

  nn_visit_labeled[[i]] <- nn_visits
}

# ---- STEP 5: Create Daily Summaries ----
nn_daily_summary <- list()

for (i in seq_along(nn_visit_labeled)) {
  day_data <- nn_visit_labeled[[i]]

  if (nrow(day_data) == 0) {
    nn_daily_summary[[i]] <- data.frame(
      cow = integer(0),
      date = character(0),
      total_nn_visits = integer(0),
      displaced_nn_visits = integer(0),
      voluntary_nn_visits = integer(0),
      prop_displaced = numeric(0)
    )
    next
  }

  nn_daily_summary[[i]] <- day_data |>
    dplyr::group_by(cow, date) |>
    dplyr::summarise(
      total_nn_visits = dplyr::n(),
      displaced_nn_visits = sum(leave_reason == "displaced"),
      voluntary_nn_visits = sum(leave_reason == "voluntary"),
      prop_displaced = displaced_nn_visits / total_nn_visits,
      .groups = "drop"
    )
}

# ---- STEP 6: Combine All Days ----
nn_visit_labeled_all <- do.call(rbind, nn_visit_labeled)
nn_daily_summary_all <- do.call(rbind, nn_daily_summary)

cat("Total non-nutritive visits:", nrow(nn_visit_labeled_all), "\n")
cat("Displaced:", sum(nn_visit_labeled_all$leave_reason == "displaced"), "\n")
cat("Voluntary:", sum(nn_visit_labeled_all$leave_reason == "voluntary"), "\n")

################################################################
# Save results
################################################################
out_dir <- paste0(output_path, "/6_non_nutritive_visit_analysis")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Visit-level labeled data (list of daily dfs)
save(nn_visit_labeled, file = paste0(out_dir, "/nn_visit_labeled.rda"))

# Combined visit-level data
write.csv(nn_visit_labeled_all,
          file = paste0(out_dir, "/nn_visit_labeled_all.csv"),
          row.names = FALSE)

# Daily summaries (list of daily dfs)
save(nn_daily_summary, file = paste0(out_dir, "/nn_daily_summary.rda"))

# Combined daily summaries
write.csv(nn_daily_summary_all,
          file = paste0(out_dir, "/nn_daily_summary_all.csv"),
          row.names = FALSE)
