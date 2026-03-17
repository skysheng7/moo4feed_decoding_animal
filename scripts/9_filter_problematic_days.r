###################################################################################################
################################## data loading ###################################################
###################################################################################################
# External packages
library(lubridate)
library(moo4feed)  # for merge_list_df() function
library(EloRating)  # for elo.seq() function
library(tidyr)      # for pivot_longer() function

# Load global variables and helper functions from lib directory
source("lib/globals.R")
source("lib/01-helpers-lameness and sick data cleaning.R")
source("lib/02-helpers-repro data cleaning.R")
source("lib/03-helpers-dominance.R")
source("lib/04-helpers-stable group selection.R")

# Create output directory if it doesn't exist
output_dir <- "results/9_filter_problematic_days"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load data files
load("results/1_data_cleaning/clean_feed.rda")
master_feeding <- moo4feed::merge_list_df(clean_feed)
load("data/regrouping.rdata")
load("data/warning_days.rdata")
load("data/parlor.rdata")
load("data/cows_in_heat.rdata")
load("data/lameness_database.rdata")
load("data/thi.rdata")
load("data/sick_cow_no_lame.rdata")
load("data/enroll_exclude_track.rdata")
load("data/master_feed_replacement_all_with_feeder_occupancy.rdata")

master_feed_replacement_all <- moo4feed::read_data_safely("results/4_replacement_detection/all_replacements.csv", header=TRUE, sep = ",")
# Convert data types using lubridate
master_feed_replacement_all$date <- lubridate::ymd(master_feed_replacement_all$date, tz="America/Los_Angeles")
master_feed_replacement_all$time <- lubridate::ymd_hms(master_feed_replacement_all$time, tz="America/Los_Angeles")
master_feed_replacement_all$bout_interval <- lubridate::period_to_seconds(lubridate::period(master_feed_replacement_all$bout_interval))  # Convert "13s" to numeric seconds

# listing the presence of each cow on each day
master_feeding$date <- lubridate::ymd(master_feeding$date, tz="America/Los_Angeles")
date_cow_list <- unique(master_feeding[, c("date", "cow")])
date_cow_list$cow <- as.integer(date_cow_list$cow)
save(date_cow_list, file = "results/9_filter_problematic_days/date_cow_list.rda")

###################################################################################################
##################################### lameness ####################################################
###################################################################################################
# Update Lameness Database based on gait scoring note ("GSnotes")
lameness <- update_GS(lameness_database, date_cow_list)

# fill in the NA for GS for the days that gait scoring did not happen
lameness <- fill_na_gs(lameness)

# label the days when the cows are lame: consecutive GS_updated > 2.5 for 2 weeks and more
# for cows that are GS_updated > 2.5 for just 1 week, label as "Q"
lameness <- label_lame_cows(lameness)

# label cows as ever become lame or not
lameness<- label_ever_lame(lameness)

# see what's the time interval between every 2 scoring events
time_dif <- calculate_time_intervals(lameness)

# Label Days Before Cow Becomes Lame as -1, -2...
lameness_processed <- label_days_bf_lame(lameness)
save(lameness_processed, file = "results/9_filter_problematic_days/lameness_processed.rda")
###################################################################################################
#################################### Sick cows ####################################################
###################################################################################################
# this is a lameness record, need to delete
# Note: cow column should already be lowercase from conversion above
sick_cow_no_lame <- sick_cow_no_lame[-which((sick_cow_no_lame$cow == 4001) & (sick_cow_no_lame$date == "2020-10-28")),]
sick_cow_no_lame <- sick_cow_no_lame[-which((sick_cow_no_lame$cow == 5145) & (sick_cow_no_lame$date == "2020-11-08")),]
sick_cow_no_lame <- sick_cow_no_lame[-which((sick_cow_no_lame$cow == 5145) & (sick_cow_no_lame$date == "2020-11-09")),]

# identify sick period for each cow as 7 days before and after recorded sickness
sick_period <- identify_sickness_periods(sick_cow_no_lame)
###################################################################################################
########################## DIM, parity, milk production ###########################################
###################################################################################################
# process reproduction data to get parity, DIM, pregnancy status etc
DIM_parity <- update_reproduction_status(parlor, date_cow_list)
save(DIM_parity, file = "results/9_filter_problematic_days/DIM_parity.rda")

# get milk production data
milk_production <- calculate_milk_production(parlor, date_cow_list)
save(milk_production, file = "results/9_filter_problematic_days/milk_production.rda")

# get cows in heat
in_heat_processed <- process_cows_in_heat(cows_in_heat, date_cow_list)
save(in_heat_processed, file = "results/9_filter_problematic_days/in_heat_processed.rda")

###################################################################################################
#################################### regrouping ###################################################
###################################################################################################
regrouping$regroup <- "Y"
regroup_date <- regrouping[, c("date", "regroup")]
regroup_date2 <- merge(date_cow_list, regroup_date)
regroup_date3 <- merge(date_cow_list, regroup_date2, all = TRUE)
regroup_date3[is.na(regroup_date3)] <- "N"


###################################################################################################
################################## warning days ###################################################
###################################################################################################
warning_days$date <- lubridate::ymd(warning_days$date, tz="America/Los_Angeles")
red_days <- warning_days[, c("date", "Red_warning")]
red_days2 <- red_days[which(red_days$Red_warning != ""),]
orange_to_delete <- c("2021-02-11", "2021-02-12", "2021-03-16", "2021-03-23", "2021-04-15", "2021-05-17")
orange_to_delete <- lubridate::ymd(orange_to_delete, tz="America/Los_Angeles")

###################################################################################################
####################################### Dominance #################################################
###################################################################################################
repl_per_day <- count_rows_per_day(master_feed_replacement_all)
save(repl_per_day, file = "results/9_filter_problematic_days/repl_per_day.rda")
repl_per_day_merged <- merge(date_cow_list, repl_per_day, all.x = TRUE)

# calculate dominance hierarchy based on replacements happened
colnames(master_feed_replacement_all_with_feeder_occupancy)[colnames(master_feed_replacement_all_with_feeder_occupancy) == "resource_occupancy"] <- "feeder_occupancy"
master_feed_replacement_all_with_feeder_occupancy$date <- lubridate::ymd(master_feed_replacement_all_with_feeder_occupancy$date, tz="America/Los_Angeles")
repl <- master_feed_replacement_all_with_feeder_occupancy[-which(master_feed_replacement_all_with_feeder_occupancy$date %in% red_days2$date | master_feed_replacement_all_with_feeder_occupancy$date %in% orange_to_delete),]
repl_low_med_fo <- repl[which(repl$feeder_occupancy <=0.75),]
elo_score <- elo_rating_calculate(repl_low_med_fo)

###################################################################################################
######################################## THI ######################################################
###################################################################################################
thi <- thi[, c("date", "temperature(C)_mean", "temperature(C)_standard_deviation", "relative_humidity(%)_mean", "relative_humidity(%)_standard_deviation", "THI_mean", "THI_standard_deviation")]
names(thi)[names(thi) == 'temperature(C)_mean'] <- "temperature_C_mean"
names(thi)[names(thi) == 'temperature(C)_standard_deviation'] <- "temperature_C_standard_deviation"
names(thi)[names(thi) == 'relative_humidity(%)_mean'] <- "relative_humidity_mean"
names(thi)[names(thi) == 'relative_humidity(%)_standard_deviation'] <- "relative_humidity_standard_deviation"

thi$date <- lubridate::ymd(thi$date, tz="America/Los_Angeles")
thi_used <- merge(date_cow_list, thi[, c("date", "THI_mean")], all.x = TRUE)
###################################################################################################
#################################### merge data ###################################################
########################### Remove pregnant cows and estrus cows ##################################
###################################################################################################
all_info <- Reduce(function(x, y) merge(x, y, by = c("date", "cow"), all = TRUE), 
                      list(date_cow_list, lameness_processed, DIM_parity, milk_production, in_heat_processed, regroup_date3, repl_per_day_merged, elo_score, thi_used))


# delete the entry and exit day of each cow
all_info2 <- merge(all_info, enroll_exclude_track, all.x = TRUE)
all_info3 <- all_info2[which(is.na(all_info2$entry_exit_status)),]
removed_days <- all_info2[which(!is.na(all_info2$entry_exit_status)),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info3$entry_exit_status <- NULL

# delete red warning days, and some orange days with data lost
all_info4 <- merge(all_info3, red_days2, all.x = TRUE)
# red warning days
removed_days <- all_info4[which(!is.na(all_info4$Red_warning)),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info5 <- all_info4[is.na(all_info4$Red_warning),]
all_info5$Red_warning <- NULL

# orange warning days
removed_days <- all_info5[which(all_info5$date %in% orange_to_delete),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info6 <- all_info5[-which(all_info5$date %in% orange_to_delete),]

# delete regrouping days
removed_days <- all_info6[which(all_info6$regroup == "Y"),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info7 <- all_info6[which(all_info6$regroup == "N"),]
all_info7$regroup <- NULL

# delete records of some cows have special issues on certain days
cow <- c(5120,5120, 5120, 7064, 7064, 5096, 5096, 4038, 4038, rep(7146, 9), 6130, 6130, 5041, 5041, 6028, 3150)
date <- c("2021-02-16", "2021-02-17", "2021-02-18", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11", "2020-08-11", "2020-08-17", "2020-09-2",  "2020-09-3", "2020-09-4", "2020-09-5", "2020-09-6", "2020-09-7", "2020-09-8", "2020-09-9", "2020-09-10", "2020-11-02","2020-11-03", "2020-09-5", "2020-09-10", "2020-11-09", "2020-12-18")
special_cow_delete <- data.frame(cow, date)
special_cow_delete$to_delete <- 1
special_cow_delete$date <- lubridate::ymd(special_cow_delete$date, tz="America/Los_Angeles")
length(unique(special_cow_delete$cow))
length(unique(special_cow_delete$date))
nrow(special_cow_delete)
all_info8 <- merge(all_info7, special_cow_delete, all.x = TRUE)
all_info8 <- all_info8[which(is.na(all_info8$to_delete)),]
all_info8$to_delete <- NULL

# delete cows that got sick
sick_period$to_delete <- 1
all_info9 <- merge(all_info8,sick_period, all.x = TRUE )
removed_days <- all_info9[which(!is.na(all_info9$to_delete)),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info9 <- all_info9[which(is.na(all_info9$to_delete)),]
all_info9$to_delete <- NULL

# delete cows that are lame, and 7 days before they are lame
removed_days <- all_info9[which(all_info9$current_lame == "Y"),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info10 <- all_info9[which(all_info9$current_lame != "Y"),]
all_info10 <- all_info10[-which((all_info10$current_lame == "N") & (all_info10$days_bf_lame >= -7) & (all_info10$days_bf_lame < 0)),]
columns_to_remove <- c("GS", "GS_updated", "GS_updated_fill", "current_lame", "ever_lame", "days_bf_lame")
all_info10 <- all_info10[, !(names(all_info10) %in% columns_to_remove)]

# delete days when there are cows in heat
removed_days <- all_info10[which(all_info10$today_has_cow_in_heat == "Y"),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info11 <- all_info10[which(all_info10$today_has_cow_in_heat == "N"),]
all_info11$today_has_cow_in_heat <- NULL
all_info11$cows_in_heat <- NULL

# delete cows that are bred, fresh, no bred, ok/open, only keep those that are pregnant
removed_days <- all_info11[which(all_info11$updated_repro_status != "PREG"),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info12 <- all_info11[which(all_info11$updated_repro_status == "PREG"),]
all_info12$updated_repro_status <- NULL

# delete cows with fewer than 10 data points
cow_counts <- table(all_info12$cow)
cows_to_keep <- names(cow_counts[cow_counts >= 10])
removed_days <- all_info12[which(!(all_info12$cow %in% cows_to_keep)),]
removed_cow_days <- nrow(removed_days)
removed_days_unique <- length(unique(removed_days$date))
removed_cows_unique <- length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days (cows with < 10 data points)", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info_final <- all_info12[which(all_info12$cow %in% cows_to_keep),]
all_info_final$month <- month(all_info_final$date)
save(all_info_final, file = "results/9_filter_problematic_days/all_info_final.rda")



###################################################################################################
#################################### merge data ###################################################
########################### Keep pregnant cows and estrus cows ####################################
###################################################################################################
all_info <- Reduce(function(x, y) merge(x, y, by = c("date", "cow"), all = TRUE), 
                      list(date_cow_list, lameness_processed, DIM_parity, milk_production, in_heat_processed, regroup_date3, repl_per_day_merged, elo_score, thi_used))


# delete the entry and exit day of each cow
all_info2 <- merge(all_info, enroll_exclude_track, all.x = TRUE)
all_info3 <- all_info2[which(is.na(all_info2$entry_exit_status)),]
removed_days <- all_info2[which(!is.na(all_info2$entry_exit_status)),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info3$entry_exit_status <- NULL

# delete red warning days, and some orange days with data lost
all_info4 <- merge(all_info3, red_days2, all.x = TRUE)
# red warning days
removed_days <- all_info4[which(!is.na(all_info4$Red_warning)),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info5 <- all_info4[is.na(all_info4$Red_warning),]
all_info5$Red_warning <- NULL

# orange warning days
removed_days <- all_info5[which(all_info5$date %in% orange_to_delete),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info6 <- all_info5[-which(all_info5$date %in% orange_to_delete),]

# delete regrouping days
removed_days <- all_info6[which(all_info6$regroup == "Y"),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info7 <- all_info6[which(all_info6$regroup == "N"),]
all_info7$regroup <- NULL

# delete records of some cows have special issues on certain days
cow <- c(5120,5120, 5120, 7064, 7064, 5096, 5096, 4038, 4038, rep(7146, 9), 6130, 6130, 5041, 5041, 6028, 3150)
date <- c("2021-02-16", "2021-02-17", "2021-02-18", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11", "2020-08-11", "2020-08-17", "2020-09-2",  "2020-09-3", "2020-09-4", "2020-09-5", "2020-09-6", "2020-09-7", "2020-09-8", "2020-09-9", "2020-09-10", "2020-11-02","2020-11-03", "2020-09-5", "2020-09-10", "2020-11-09", "2020-12-18")
special_cow_delete <- data.frame(cow, date)
special_cow_delete$to_delete <- 1
special_cow_delete$date <- lubridate::ymd(special_cow_delete$date, tz="America/Los_Angeles")
length(unique(special_cow_delete$cow))
length(unique(special_cow_delete$date))
nrow(special_cow_delete)
all_info8 <- merge(all_info7, special_cow_delete, all.x = TRUE)
all_info8 <- all_info8[which(is.na(all_info8$to_delete)),]
all_info8$to_delete <- NULL

# delete cows that got sick
sick_period$to_delete <- 1
all_info9 <- merge(all_info8,sick_period, all.x = TRUE )
removed_days <- all_info9[which(!is.na(all_info9$to_delete)),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info9 <- all_info9[which(is.na(all_info9$to_delete)),]
all_info9$to_delete <- NULL

# delete cows that are lame, and 7 days before they are lame
removed_days <- all_info9[which(all_info9$current_lame == "Y"),]
removed_cow_days <-nrow(removed_days)
removed_days_unique <-length(unique(removed_days$date))
removed_cows_unique <-length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info10 <- all_info9[which(all_info9$current_lame != "Y"),]
all_info10 <- all_info10[-which((all_info10$current_lame == "N") & (all_info10$days_bf_lame >= -7) & (all_info10$days_bf_lame < 0)),]
columns_to_remove <- c("GS", "GS_updated", "GS_updated_fill", "current_lame", "ever_lame", "days_bf_lame")
all_info10 <- all_info10[, !(names(all_info10) %in% columns_to_remove)]

# keep the days when there are cows in heat
# delete cows with fewer than 10 data points
cow_counts <- table(all_info10$cow)
cows_to_keep <- names(cow_counts[cow_counts >= 10])
removed_days <- all_info10[which(!(all_info10$cow %in% cows_to_keep)),]
removed_cow_days <- nrow(removed_days)
removed_days_unique <- length(unique(removed_days$date))
removed_cows_unique <- length(unique(removed_days$cow))
print(sprintf("Removed %d cow-days from %d cows on %d days (cows with < 10 data points)", removed_cow_days, removed_cows_unique, removed_days_unique))
all_info_final_with_heat <- all_info10[which(all_info10$cow %in% cows_to_keep),]

all_info_final_with_heat$month <- month(all_info_final_with_heat$date)
save(all_info_final_with_heat, file = "results/9_filter_problematic_days/all_info_final_with_heat_repro_status.rda")

