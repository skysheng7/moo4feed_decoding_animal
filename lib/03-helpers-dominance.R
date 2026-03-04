#' Count Replacements Per Day
#'
#' This function counts the total number of Replcement records that appear per day in the given dataframe.
#' 
#' @param master_feed_replacement_all A dataframe containing replacement records with a date column.
#' @return A dataframe with the count of rows per day.
count_rows_per_day <- function(master_feed_replacement_all) {
  daily_counts <- as.data.frame(table(master_feed_replacement_all$date))
  colnames(daily_counts) <- c("date", "replacement_num")
  daily_counts$date <- ymd(daily_counts$date, tz="America/Los_Angeles")
  return(daily_counts)
}

#' Calculate ELO Ratings
#'
#' This function calculates the ELO ratings for a given list of replacements and saves the results.
#' The function orders replacements, calculates ELO scores using the `elo.seq` function, and saves the results
#' in both wide and long formats.
#'
#' @param temp_replacement_list A dataframe containing the replacement list with columns: "Actor_cow", "Reactor_cow", "Time", "Bin", "date".
#' @return A dataframe in long format with columns: "date", "cow", "Elo".
elo_rating_calculate <- function(temp_replacement_list) {
  # Order replacements and rename columns
  elo_repl_list <- temp_replacement_list[order(temp_replacement_list$Time), c("Actor_cow", "Reactor_cow", "Time", "Bin", "date")]
  colnames(elo_repl_list) <- c("winner", "loser", "time", "bin", "date")
  
  # Calculate the ELO scores
  elo_package_result <- elo.seq(
    winner = as.character(elo_repl_list$winner),
    loser = as.character(elo_repl_list$loser),
    Date = as.character(elo_repl_list$date),
    k = 20,
    runcheck = FALSE,
    progressbar = TRUE
  )
  
  # Prepare ELO matrix
  elo_mat <- as.data.frame(elo_package_result$mat)
  elo_mat <- cbind(as.Date(elo_package_result$truedates), elo_mat)
  colnames(elo_mat)[1] <- "date"
  
  # Transform data to long format
  elo_mat_long <- pivot_longer(
    elo_mat,
    cols = -date,
    names_to = "cow",
    values_to = "Elo"
  )
  
  # Remove rows with NA values
  elo_score <- na.exclude(elo_mat_long)
  elo_score$date <- ymd(elo_score$date, tz="America/Los_Angeles")
  elo_score$cow <- as.integer(elo_score$cow)
  
  # Save the ELO scores
  save(elo_package_result, file = here("data/results/elo_full_result.rdata"))
  save(elo_score, file = here("data/results/elo_score_daily.rdata"))
  
  return(elo_score)
}