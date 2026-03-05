#' Update Lameness Database based on gait scoring note ("GSnotes")
#'
#' This function processes the lameness database to update the gait score (GS) 
#' based on the notes provided (plus or minus). It also removes duplicated records 
#' and ensures that all date and cow combinations are present.
#' 
#' @param lameness_database A dataframe containing lameness records with columns:
#' date, cow, GS, GSnotes
#' @param date_cow_list A dataframe containing all possible date and cow combinations.
#' @return A dataframe with updated gait scores and no duplicated records.
#' @examples
#' lameness_database <- data.frame(date = c('2023-07-01', '2023-07-01'), 
#'                                 cow = c('1001', '2002'), 
#'                                 GS = c(2, 3), 
#'                                 GSnotes = c('plus', 'minus'))
#' date_cow_list <- data.frame(date = c('2023-07-01', '2023-07-01', '2023-07-02'), 
#'                             cow = c('1001', '2002', '3003'))
#' updated_lameness <- update_lameness_database(lameness_database, date_cow_list)
update_GS <- function(lameness_database, date_cow_list) {
  # Initialize GS_change column
  lameness_database$GS_change <- 0
  
  # Set the GS points to increase or decrease
  plus_increment <- 0.2
  minus_decrease <- -0.2
  
  # Update GS_change based on GSnotes using vectorized operations
  lameness_database$GS_change[lameness_database$GSnotes == "plus"] <- plus_increment
  lameness_database$GS_change[lameness_database$GSnotes == "minus"] <- minus_decrease
  
  # Modify GS based on GSnotes
  lameness_database$GS_updated <- lameness_database$GS + lameness_database$GS_change
  
  # Delete duplicated records
  lameness_updated <- unique(lameness_database[, c("date", "cow", "GS", "GS_updated")])
  duplicated_rec <- lameness_updated[duplicated(lameness_updated[, c("date", "cow")]), ]
  duplicated_rec$to_delete <- 1
  lameness_updated2 <- merge(lameness_updated, duplicated_rec, all = TRUE)
  lameness_updated3 <- lameness_updated2[which(is.na(lameness_updated2$to_delete)), ]
  lameness_updated3$to_delete <- NULL
  
  # Merge with date_cow_list to ensure all date and cow combinations are present
  lameness <- merge(date_cow_list, lameness_updated3, by = c("date", "cow"), all.x = TRUE)
  
  return(lameness)
}


#' Fill NA Values for GS in Lameness Database
#'
#' This function fills in the NA values for the gait score (GS) in the lameness database 
#' by carrying forward and backward the known GS values for each cow within the specified date range.
#' 
#' @param lameness A dataframe containing lameness records with columns:
#' date, cow, GS, GS_updated
#' @return A dataframe with filled GS values.
#' @examples
#' lameness <- data.frame(date = as.Date(c('2023-07-01', '2023-07-01', '2023-07-02')), 
#'                        cow = c('Cow1', 'Cow2', 'Cow1'), 
#'                        GS = c(2, 3, 2), 
#'                        GS_updated = c(2.2, 3, 2))
#' filled_lameness <- fill_na_gs(lameness)
fill_na_gs <- function(lameness) {
  # Order by cow and date
  lameness <- lameness[order(lameness$cow, lameness$date),]
  lameness$GS_updated_fill <- -1
  
  # Get unique cows
  unique_cow <- unique(lameness$cow)
  
  for (i in 1:length(unique_cow)) {
    cur_cow <- unique_cow[i]
    temp <- lameness[which(lameness$cow == cur_cow),]
    start <- min(temp$date)
    end <- max(temp$date)
    temp_noNA <- na.omit(temp)
    temp_noNA <- temp_noNA[order(temp_noNA$date),]
    
    for (k in 1:nrow(temp_noNA)) {
      cur_date <- temp_noNA$date[k]
      cur_GS <- temp_noNA$GS_updated[k]
      if (k == 1) {
        lameness$GS_updated_fill[which((lameness$cow == cur_cow) & (lameness$date >= start) & (lameness$date <= cur_date))] <- cur_GS
      }
      
      if (k == nrow(temp_noNA)) {
        lameness$GS_updated_fill[which((lameness$cow == cur_cow) & (lameness$date >= cur_date) & (lameness$date <= end))] <- cur_GS
      } else {
        next_date <- temp_noNA$date[k + 1]
        lameness$GS_updated_fill[which((lameness$cow == cur_cow) & (lameness$date >= cur_date) & (lameness$date < next_date))] <- cur_GS
      }
    }
  }
  
  return(lameness)
}

#' Label Days When Cows Are Lame
#'
#' This function labels the days when cows are considered lame, which is defined 
#' as having a gait score (GS) greater than 2.5 for two consecutive weeks or more.
#' For cows that are GS_updated > 2.5 for just 1 week, label as "Q"
#' 
#' @param lameness A dataframe containing lameness records with columns:
#' date, cow, GS, GS_updated
#' @return A dataframe with an additional column `current_lame` indicating if the cow is lame.
label_lame_cows <- function(lameness) {
  # Order by cow and date
  lameness <- lameness[order(lameness$cow, lameness$date),]
  lameness$current_lame <- "N"
  
  # Get unique cows
  unique_cow <- unique(lameness$cow)
  
  for (i in 1:length(unique_cow)) {
    cur_cow <- unique_cow[i]
    temp <- lameness[which(lameness$cow == cur_cow),]
    start <- min(temp$date)
    end <- max(temp$date)
    temp_noNA <- na.omit(temp)
    temp_noNA <- temp_noNA[order(temp_noNA$date),]
    
    if (nrow(temp_noNA) > 1) {
      for (k in 2:nrow(temp_noNA)) {
        cur_date <- temp_noNA$date[k]
        cur_GS <- temp_noNA$GS_updated[k]
        prev_date <- temp_noNA$date[k-1]
        prev_GS <- temp_noNA$GS_updated[k-1]
        
        # if the cow is GS > 2.5 for 2 consecutive weeks, label the 1 week in between as lame, and label the 1 week after current day as lame
        if ((cur_GS > 2.5) & (prev_GS > 2.5)) {
          
          if (k == 2) {
            lameness$current_lame[which((lameness$cow == cur_cow) & (lameness$date >= start) & (lameness$date <= cur_date))] <- "Y"
            
          } else {
            lameness$current_lame[which((lameness$cow == cur_cow) & (lameness$date >= prev_date) & (lameness$date <= cur_date))] <- "Y"
            
          }
          
          if ((k + 1) <= nrow(temp_noNA)) {
            next_date <- temp_noNA$date[k + 1]
            lameness$current_lame[which((lameness$cow == cur_cow) & (lameness$date >= cur_date) & (lameness$date < next_date))] <- "Y"
          } else {
            lameness$current_lame[which((lameness$cow == cur_cow) & (lameness$date >= cur_date) & (lameness$date <= end))] <- "Y"
          }
        }
      }
    }
  }
  
  # for cows that are GS_updated > 2.5 for just 1 week, label as "Q"
  lameness$current_lame[which((lameness$GS_updated_fill > 2.5) & (lameness$current_lame == "N"))] <- "Q"
  
  return(lameness)
}

#' Label Cows as Ever Lame of not
#'
#' This function labels cows that have ever become lame
#' 
#' @param lameness A dataframe containing lameness records with columns:
#' date, cow, current_lame
#' @return A dataframe with additional column `ever_lame`
label_ever_lame <- function(lameness) {
  # Initialize columns
  lameness$ever_lame <- "N"

  # Get unique cows
  unique_cow <- unique(lameness$cow)
  
  for (i in 1:length(unique_cow)) {
    cur_cow <- unique_cow[i]
    temp <- lameness[which(lameness$cow == cur_cow),]
    has_Y <- temp[which(temp$current_lame == "Y"),]
    
    if (nrow(has_Y) > 0) {
      lameness$ever_lame[which(lameness$cow == cur_cow)] <- "Y"
    }
  }
  
  return(lameness)
}


#' Calculate Time Intervals Between Scoring Events
#'
#' This function converts the date column to a specific timezone, orders the lameness records by date,
#' and calculates the time intervals between consecutive scoring events.
#' 
#' @param lameness A dataframe containing lameness records with a date column and GS column.
#' @return A vector of time intervals in days between consecutive scoring events.
#' @examples
calculate_time_intervals <- function(lameness) {
  # Convert date column to specific timezone
  lameness$date <- lubridate::ymd(lameness$date, tz = "America/Los_Angeles")
  
  # Order by date
  lameness <- lameness[order(lameness$date),]
  
  # Filter out rows where GS is NA
  GS_exist <- lameness[which(!is.na(lameness$GS)),]
  
  # Calculate time intervals between consecutive scoring events
  time_dif <- c()
  unique_dates <- unique(GS_exist$date)
  for (i in 1:(length(unique_dates) - 1)) {
    time_dif <- c(time_dif, round(abs(difftime(unique_dates[i], unique_dates[i + 1], units = "days")), digits = 0))
  }
  
  return(time_dif)
}


#' Label Days Before Cow Becomes Lame
#'
#' This function iterates through every cow in the lameness dataframe, sorts by cow and date, 
#' and labels the days before the transition from healthy (current_lame == "N" or current_lame == "Q") 
#' to lame (current_lame == "Y") as -1, -2, -3... days before the cow becomes lame. 
#' If the cow is always healthy or always lame, the days_bf_lame is set to 0.
#' 
#' @param lameness A dataframe containing lameness records with columns:
#' date, cow, GS, GS_updated, GS_updated_fill, current_lame, ever_lame
#' @return A dataframe with an additional column `days_bf_lame` indicating the days before the cow becomes lame.
label_days_bf_lame <- function(lameness) {
  # Order by cow and date
  lameness <- lameness[order(lameness$cow, lameness$date),]
  lameness$days_bf_lame <- 0
  
  unique_cows <- unique(lameness$cow)
  
  for (cow_id in unique_cows) {
    cow_data <- lameness[lameness$cow == cow_id,]
    
    if (any(cow_data$current_lame == "Y")) {
      for (i in 2:nrow(cow_data)) {
        if (cow_data$current_lame[i] == "Y" && (cow_data$current_lame[i - 1] == "N" || cow_data$current_lame[i - 1] == "Q")) {
          transition_date <- cow_data$date[i]
          days_before <- 0
          for (j in (i - 1):1) {
            if (cow_data$current_lame[j] == "N" || cow_data$current_lame[j] == "Q") {
              days_before <- days_before + as.numeric(difftime(cow_data$date[j + 1], cow_data$date[j], units = "days"))
              lameness$days_bf_lame[lameness$cow == cow_id & lameness$date == cow_data$date[j]] <- -days_before
            } else {
              break
            }
          }
        }
      }
    }
  }
  
  return(lameness)
}

#' Identify 7 days before and after noted sickness (exclude lameness events) for each cow
#'
#' @param sick_cow_no_lame A dataframe containing records of sick cows with columns:
#' cow, date
#' @return A dataframe with the periods around sickness removed.
identify_sickness_periods <- function(sick_cow_no_lame) {
  # Order by cow and date
  sick_cow_no_lame <- sick_cow_no_lame[order(sick_cow_no_lame$cow, sick_cow_no_lame$date),]
  
  # Initialize an empty dataframe for sick periods
  sick_period <- data.frame(date = as.POSIXct(character()), cow = integer())
  
  for (i in 1:nrow(sick_cow_no_lame)) {
    cur_cow <- sick_cow_no_lame$cow[i]
    cur_day <- sick_cow_no_lame$date[i]
    cur_day_seq <- seq((cur_day-days(7)),(cur_day+days(7)) , by = "days")
    cur_day_df <- data.frame(cur_day_seq)
    colnames(cur_day_df) <- c("date")
    cur_day_df$cow <- cur_cow
    
    if (i == 1) {
      sick_period <- cur_day_df
    } else {
      sick_period <- merge(sick_period, cur_day_df, all = TRUE)
    }
  }
  
  # Remove duplicate rows in sick_period
  sick_period <- unique(sick_period)
  
  return(sick_period)
}