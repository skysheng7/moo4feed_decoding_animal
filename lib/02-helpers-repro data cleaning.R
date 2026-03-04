#' Update Reproduction Status
#'
#' This function processes the parlor dataframe to update the reproduction status for each cow,
#' keeping the status from the afternoon milking if there is a discrepancy within the same day.
#' 
#' @param parlor A dataframe containing parlor records with columns:
#' cowID, days_in_milk, lactation_number, pregnant_status, date
#' @param date_cow_list A dataframe containing all possible date and cow combinations.
#' @return A dataframe with updated reproduction status merged with date_cow_list.
update_reproduction_status <- function(parlor, date_cow_list) {
  # Add an index column to the parlor dataframe
  parlor$index <- seq(1, nrow(parlor), by = 1)
  
  # Create the reproduction_DIM dataframe
  reproduction_DIM <- unique(parlor[, c("cowID", "days_in_milk", "lactation_number", "pregnant_status", "date", "index")])
  names(reproduction_DIM)[names(reproduction_DIM) == 'cowID'] <- "cow"
  names(reproduction_DIM)[names(reproduction_DIM) == 'lactation_number'] <- "Parity"
  reproduction_DIM$date <- lubridate::ymd(reproduction_DIM$date, tz="America/Los_Angeles")
  reproduction_DIM$cow <- as.integer(reproduction_DIM$cow)
  
  # Sort the dataframe by cow and date
  reproduction_DIM <- reproduction_DIM[order(reproduction_DIM$cow, reproduction_DIM$date),]
  reproduction_DIM$updated_repro_status <- reproduction_DIM$pregnant_status
  
  # Update the reproduction status
  for (i in 2:nrow(reproduction_DIM)) {
    if ((reproduction_DIM$cow[i] == reproduction_DIM$cow[i-1]) & 
        (reproduction_DIM$date[i] == reproduction_DIM$date[i-1]) & 
        (reproduction_DIM$pregnant_status[i] != reproduction_DIM$pregnant_status[i-1])) {
      if (reproduction_DIM$index[i] > reproduction_DIM$index[i-1]) {
        reproduction_DIM$updated_repro_status[i] <- reproduction_DIM$pregnant_status[i]
        reproduction_DIM$updated_repro_status[i-1] <- reproduction_DIM$pregnant_status[i]
      } else {
        reproduction_DIM$updated_repro_status[i] <- reproduction_DIM$pregnant_status[i-1]
        reproduction_DIM$updated_repro_status[i-1] <- reproduction_DIM$pregnant_status[i-1]
      }
    }
  }
  
  # Create the DIM_parity dataframe
  DIM_parity <- unique(reproduction_DIM[, c("date", "cow", "days_in_milk", "Parity", "updated_repro_status")])
  
  # Merge with date_cow_list to ensure all date and cow combinations are present
  DIM_parity_focal <- merge(date_cow_list, DIM_parity, by = c("date", "cow"), all.x = TRUE)
  
  return(DIM_parity_focal)
}

#' Calculate Milk Production
#'
#' This function processes the parlor dataframe to calculate the daily milk production for each cow,
#' and merges the results with a date-cow list to ensure all date and cow combinations are present.
#' 
#' @param parlor A dataframe containing parlor records with columns:
#' cowID, date, morning_milking, afternoon_milking
#' @param date_cow_list A dataframe containing all possible date and cow combinations.
#' @return A dataframe with daily milk production merged with date_cow_list.
calculate_milk_production <- function(parlor, date_cow_list) {
  # Create the milk_prod dataframe
  milk_prod <- unique(parlor[, c("cowID", "date", "morning_milking", "afternoon_milking")])
  names(milk_prod)[names(milk_prod) == 'cowID'] <- "cow"
  milk_prod$date <- lubridate::ymd(milk_prod$date, tz = "America/Los_Angeles")
  milk_prod$cow <- as.integer(milk_prod$cow)
  
  # Calculate total milk production for each day
  milk_prod$milk_production <- milk_prod$morning_milking + milk_prod$afternoon_milking
  
  # Aggregate milk production by date and cow
  milk_prod2 <- aggregate(milk_prod$milk_production, 
                          by = list(milk_prod$date, milk_prod$cow), 
                          FUN = max, 
                          na.rm = TRUE, 
                          na.action = NULL)
  colnames(milk_prod2) <- c("date", "cow", "milk_production")
  
  # Merge with date_cow_list to ensure all date and cow combinations are present
  milk_prod3 <- merge(date_cow_list, milk_prod2, by = c("date", "cow"), all.x = TRUE)
  
  return(milk_prod3)
}

#' Process Cows in Heat Data
#'
#' This function processes the cows in heat data to identify the days when cows are in heat,
#' and merges the results with a date-cow list to ensure all date and cow combinations are present.
#' 
#' @param cows_in_heat A dataframe containing cows in heat records with columns:
#' cow, date, TrueEstrus
#' @param date_cow_list A dataframe containing all possible date and cow combinations.
#' @return A dataframe with an additional column `today_has_cow_in_heat` indicating if there is any cow in heat on that day.
process_cows_in_heat <- function(cows_in_heat, date_cow_list) {
  # Convert cow column to integer and date column to specific timezone
  cows_in_heat$cow <- as.integer(cows_in_heat$cow)
  cows_in_heat$date <- lubridate::ymd(cows_in_heat$date, tz = "America/Los_Angeles")
  
  # Create a current_cow_in_heat dataframe
  current_cow_in_heat <- cows_in_heat
  colnames(current_cow_in_heat) <- c("cow", "date", "this_cow_in_heat")
  
  # Aggregate cows in heat by date
  in_heat <- aggregate(cows_in_heat$TrueEstrus, by = list(cows_in_heat$date), FUN = sum, na.rm = TRUE, na.action = NULL)
  colnames(in_heat) <- c("date", "cows_in_heat")
  in_heat$date <- lubridate::ymd(in_heat$date, tz = "America/Los_Angeles")
  
  # Filter in_heat dataframe based on date range in date_cow_list
  in_heat2 <- in_heat[in_heat$date >= min(date_cow_list$date) & in_heat$date <= max(date_cow_list$date),]
  
  # Merge with date_cow_list
  in_heat3 <- merge(date_cow_list, in_heat2, by = "date", all.x = TRUE)
  
  # Fill NA values with 0
  in_heat3[is.na(in_heat3)] <- 0
  
  # Add today_has_cow_in_heat column
  in_heat3$today_has_cow_in_heat <- "N"
  in_heat3$today_has_cow_in_heat[in_heat3$cows_in_heat > 0] <- "Y"
  
  return(in_heat3)
}
