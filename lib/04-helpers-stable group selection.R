#' Create Stable Groups DataFrame
#'
#' This function creates a dataframe to record group_start, group_end, and group_number in sequence.
#' 
#' @param regroup_date A dataframe recording the list of days where each regrouping event happens.
#' @return A dataframe with columns: group_start, group_end, and group_number.

create_stable_groups <- function(regroup_date) {
  # Add the start and end date to regroup_date
  regroup_date <- rbind(data.frame(date = trial_start, regroup = "Y"), regroup_date)
  regroup_date <- rbind(regroup_date, data.frame(date = trial_end, regroup = "Y"))
  
  # Calculate group_start and group_end
  group_start <- regroup_date[1:(nrow(regroup_date) - 1), "date"]
  group_end <- regroup_date[2:nrow(regroup_date), "date"] - lubridate::days(1)
  
  # Create the stable groups dataframe
  stable_groups <- data.frame(
    group_start = group_start,
    group_end = group_end,
    group_number = seq_along(group_start)
  )
  
  return(stable_groups)
}


#' Add Group Number to all_info_final dataframe
#'
#' This function adds a group_number column to all_info_final based on the stable_groups dataframe.
#' 
#' @param all_info_final A dataframe containing records of each cow and the dates they were present.
#' @param stable_groups A dataframe containing group_start, group_end, and group_number.
#' @return The all_info_final dataframe with an additional group_number column.
add_group_number <- function(all_info_final, stable_groups) {
  all_info_final$group_number <- NA
  for (i in seq_len(nrow(stable_groups))) {
    all_info_final$group_number[all_info_final$date >= stable_groups$group_start[i] & all_info_final$date <= stable_groups$group_end[i]] <- stable_groups$group_number[i]
  }
  return(all_info_final)
}

#' Calculate Days Per Group
#'
#' This function calculates the total number of days each cow has records in each stable group.
#' 
#' @param all_info_final A dataframe containing records of each cow and the dates they were present, with a group_number column.
#' @return A dataframe with columns: cow, group_number, days_count.
calculate_days_per_group <- function(all_info_final, stable_groups) {
  all_info_final <- all_info_final[order(all_info_final$cow, all_info_final$date),]
  days_per_group <- aggregate(date ~ cow + group_number, data = all_info_final, FUN = length)
  colnames(days_per_group)[3] <- "days_count"
  days_per_group <- merge(days_per_group, stable_groups)
  return(days_per_group)
}

#' Determine Max Days Group
#'
#' This function determines the stable group period where each cow has the most number of records.
#' 
#' @param days_per_group A dataframe containing the total number of days each cow has records in each stable group.
#' @return A dataframe with columns: cow, group_number, days_count.
determine_max_days_group <- function(days_per_group, stable_groups) {
  max_days_group <- do.call(rbind, lapply(split(days_per_group, days_per_group$cow), function(df) {
    df[which.max(df$days_count), ]
  }))
  max_days_group <- merge(max_days_group, stable_groups)
  return(max_days_group)
}

#' Summarize Group Frequency
#'
#' This function summarizes the frequency of each stable group being the period with the most records for each cow.
#' 
#' @param max_days_group A dataframe containing the stable group period where each cow has the most number of records.
#' @return A dataframe with columns: group_number, frequency.
summarize_group_frequency <- function(max_days_group, stable_groups) {
  group_frequency <- as.data.frame(table(max_days_group$group_number))
  colnames(group_frequency) <- c("group_number", "cow_num")
  group_frequency <- merge(group_frequency, stable_groups)
  group_frequency <- group_frequency[order(group_frequency$cow_num, decreasing = TRUE),]
  
  return(group_frequency)
}


#' Plot Histogram for a Specific Group
#'
#' This function plots a histogram of the days_count column for a specific group with breaks of 1 on the x-axis
#' and sets x-axis ticks to increment by 1. The title of the histogram is "Number of days of record available for each cow in group xx".
#' 
#' @param data A dataframe containing the max_days_group data.
#' @param group_number The specific group number to plot the histogram for.
#' @param max_days the max day range on the x axis
plot_histogram_for_group <- function(data, group_number, max_days, max_frequency) {
  group_data <- data[data$group_number == group_number, ]
  hist(group_data$days_count, 
       breaks = seq(1, max_days + 1, by = 1), 
       xlim = c(1, max_days + 1), 
       ylim = c(1, max_frequency + 1), 
       xaxt = 'n',
       main = paste("Number of days of record available for each cow in group", group_number),
       xlab = "Days Count",
       ylab = "Frequency")
  axis(1, at = seq(1, max_days + 1, by = 1))
}


#' Select groups of cows to be analyzed
#'
#' This function selects a specified number of top groups based on frequency, filters the cows and records
#' accordingly, saves the filtered data, and provides summary statistics.
#'
#' @param total_number_of_groups_selected The number of top groups to select based on frequency.
#' @param group_frequency A dataframe containing the frequency of each group.
#' @param max_days_group_selected A dataframe containing the maximum days of records for each group and cow.
#' @param all_info_final A dataframe containing the complete records of all cows.
#' @return A list containing summary statistics: total_cow_selected, min_days_per_cow, max_days_per_cow, min_cows_per_group, max_cows_per_group.
select_and_analyze_groups <- function(total_number_of_groups_selected, group_frequency, max_days_group_selected, all_info_final) {
  group_frequency_selected <- group_frequency[(1:total_number_of_groups_selected), ]
  selected_groups <- group_frequency_selected$group_number
  selected_cows <- max_days_group_selected[max_days_group_selected$group_number %in% selected_groups, "cow"]
  
  all_info_final_selected <- all_info_final[(all_info_final$group_number %in% selected_groups) & (all_info_final$cow %in% selected_cows), ]
  save(all_info_final_selected, file = "results/9_filter_problematic_days/all_info_final_selected.rdata")
  
  total_cow_selected <- length(unique(all_info_final_selected$cow))
  min_days_per_cow <- min(max_days_group_selected[max_days_group_selected$group_number %in% selected_groups, "days_count"])
  max_days_per_cow <- max(max_days_group_selected[max_days_group_selected$group_number %in% selected_groups, "days_count"])
  min_cows_per_group <- min(group_frequency_selected$cow_num)
  max_cows_per_group <- max(group_frequency_selected$cow_num)
  
  summary_text <- paste(
    total_number_of_groups_selected, "groups selected, with a total of",
    total_cow_selected, "cows. Each group has", min_cows_per_group, "to", max_cows_per_group,
    "cows. Each cow has", min_days_per_cow, "to", max_days_per_cow, "days of records available."
  )
  print(summary_text)
  
  return(all_info_final_selected)
}

