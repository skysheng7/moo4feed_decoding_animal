###################################################################################################
################################## data loading ###################################################
###################################################################################################
library(lubridate)
library(plyr)
library(here)
library(roxygen2)
library(EloRating)
library(tidyverse)

# load data available from published R project: https://github.com/skysheng7/competition_dominance_analysis.git
load("../competition_dominance_analysis/data/results/Cleaned_feeding_original_data_combined.rdata")
load("../competition_dominance_analysis/data/results/regrouping.rdata")
load("../competition_dominance_analysis/data/warning_days.rdata")
load("../competition_dominance_analysis/data/results/master_feed_replacement_all.rdata")

# listing the presence of each cow on each day
master_feeding$date <- ymd(master_feeding$date, tz="America/Los_Angeles")
date_cow_list <- unique(master_feeding[, c("date", "Cow")])
date_cow_list$Cow <- as.integer(date_cow_list$Cow)
cache("date_cow_list")

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
cache("lameness_processed")
###################################################################################################
#################################### Sick cows ####################################################
###################################################################################################
# this is a lameness record, need to delete
sick_cow_no_lame <- sick_cow_no_lame[-which((sick_cow_no_lame$Cow == 4001) & (sick_cow_no_lame$date == "2020-10-28")),]
sick_cow_no_lame <- sick_cow_no_lame[-which((sick_cow_no_lame$Cow == 5145) & (sick_cow_no_lame$date == "2020-11-08")),]
sick_cow_no_lame <- sick_cow_no_lame[-which((sick_cow_no_lame$Cow == 5145) & (sick_cow_no_lame$date == "2020-11-09")),]

# identify sick period for each cow as 7 days before and after recorded sickness
sick_period <- identify_sickness_periods(sick_cow_no_lame)
###################################################################################################
########################## DIM, parity, milk production ###########################################
###################################################################################################
# process reproduction data to get parity, DIM, pregnancy status etc
DIM_parity <- update_reproduction_status(parlor, date_cow_list)
cache("DIM_parity")

# get milk production data
milk_production <- calculate_milk_production(parlor, date_cow_list)
cache("milk_production")

# get cows in heat
in_heat_processed <- process_cows_in_heat(cows_in_heat, date_cow_list)
cache("in_heat_processed")

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
warning_days$date <- ymd(warning_days$date, tz="America/Los_Angeles")
red_days <- warning_days[, c("date", "Red_warning")]
red_days2 <- red_days[which(red_days$Red_warning != ""),]
orange_to_delete <- c("2021-02-11", "2021-02-12", "2021-03-16", "2021-03-23", "2021-04-15", "2021-05-17")
orange_to_delete <- ymd(orange_to_delete, tz="America/Los_Angeles")

###################################################################################################
####################################### Dominance #################################################
###################################################################################################
repl_per_day <- count_rows_per_day(master_feed_replacement_all)
cache("repl_per_day")  
repl_per_day_merged <- merge(date_cow_list, repl_per_day, all.x = TRUE)

# calculate dominance hierarchy based on replacements happened
colnames(master_feed_replacement_all_with_feeder_occupancy)[colnames(master_feed_replacement_all_with_feeder_occupancy) == "resource_occupancy"] <- "feeder_occupancy"
master_feed_replacement_all_with_feeder_occupancy$date <- ymd(master_feed_replacement_all_with_feeder_occupancy$date, tz="America/Los_Angeles")
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

thi$date <- ymd(thi$date, tz="America/Los_Angeles")
thi_used <- merge(date_cow_list, thi[, c("date", "THI_mean")], all.x = TRUE)
###################################################################################################
#################################### merge data ###################################################
###################################################################################################
all_info <- Reduce(function(x, y) merge(x, y, by = c("date", "Cow"), all = TRUE), 
                      list(date_cow_list, lameness_processed, DIM_parity, milk_production, in_heat_processed, regroup_date3, repl_per_day_merged, elo_score, thi_used))


# delete the entry and exit day of each cow
all_info2 <- merge(all_info, enroll_exclude_track, all.x = TRUE)
all_info3 <- all_info2[which(is.na(all_info2$entry_exit_status)),]
all_info3$entry_exit_status <- NULL

# delete red warning days, and some orange days with data lost
all_info4 <- merge(all_info3, red_days2, all.x = TRUE)
all_info5 <- all_info4[is.na(all_info4$Red_warning),]
all_info5$Red_warning <- NULL
all_info6 <- all_info5[-which(all_info5$date %in% orange_to_delete),]

# delete regrouping days
#all_info7 <- all_info6
all_info7 <- all_info6[which(all_info6$regroup == "N"),]
all_info7$regroup <- NULL

# delete records of some cows have special issues on certain days
Cow <- c(5120,5120, 5120, 7064, 7064, 5096, 5096, 4038, 4038, rep(7146, 9), 6130, 6130, 5041, 5041, 6028, 3150)
date <- c("2021-02-16", "2021-02-17", "2021-02-18", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11", "2020-08-11", "2020-08-17", "2020-09-2",  "2020-09-3", "2020-09-4", "2020-09-5", "2020-09-6", "2020-09-7", "2020-09-8", "2020-09-9", "2020-09-10", "2020-11-02","2020-11-03", "2020-09-5", "2020-09-10", "2020-11-09", "2020-12-18")
special_cow_delete <- data.frame(Cow, date)
special_cow_delete$to_delete <- 1
special_cow_delete$date <- ymd(special_cow_delete$date, tz="America/Los_Angeles")
all_info8 <- merge(all_info7, special_cow_delete, all.x = TRUE)
all_info8 <- all_info8[which(is.na(all_info8$to_delete)),]
all_info8$to_delete <- NULL

# delete cows that got sick
sick_period$to_delete <- 1
all_info9 <- merge(all_info8,sick_period, all.x = TRUE )
all_info9 <- all_info9[which(is.na(all_info9$to_delete)),]
all_info9$to_delete <- NULL

# delete cows that are lame, and 7 days before they are lame
all_info10 <- all_info9[which(all_info9$current_lame != "Y"),]
all_info10 <- all_info10[-which((all_info10$current_lame == "N") & (all_info10$days_bf_lame >= -7) & (all_info10$days_bf_lame < 0)),]
columns_to_remove <- c("GS", "GS_updated", "GS_updated_fill", "current_lame", "ever_lame", "days_bf_lame")
all_info10 <- all_info10[, !(names(all_info10) %in% columns_to_remove)]

# delete days when there are cows in heat
all_info11 <- all_info10[which(all_info10$today_has_cow_in_heat == "N"),]
all_info11$today_has_cow_in_heat <- NULL
all_info11$cows_in_heat <- NULL

# delete cows that are bred, fresh, no bred, ok/open, only keep those that are pregnant
all_info12 <- all_info11[which(all_info11$updated_repro_status == "PREG"),]
all_info12$updated_repro_status <- NULL

# delete the days when there are more than 1000 replacements per day, indicating herd level anomalies
all_info_final <- all_info12[which(all_info12$replacement_num < 900),]
save(all_info_final, file = here("data/results/all_info_final.rdata"))

###################################################################################################
####################### identify stable groups based on regrouping data ###########################
###################################################################################################
# record the start and end date of each stable group between regrouping events
stable_groups <- create_stable_groups(regroup_date)
cache("stable_groups")

# create a unique grou_number for each stable group, and label it at all_info_final
all_info_final <- add_group_number(all_info_final, stable_groups)
save(all_info_final, file = here("data/results/all_info_final.rdata"))

# calculate the total number of days each cow has record for in each stable group
days_per_group <- calculate_days_per_group(all_info_final, stable_groups)
cache("days_per_group")
# determine in which stable group, each cow has the most number of records
max_days_group <- determine_max_days_group(days_per_group, stable_groups) 
cache("max_days_group")
max_days_group_selected <- max_days_group[which(max_days_group$days_count >= 13), ]
# determine for which stable group, they have the most number of cows with the most number of days of records available
group_frequency <- summarize_group_frequency(max_days_group_selected, stable_groups)
cache("group_frequency")

# visualize the top stable groups, within each of the top stable groups, how many days of record does each cow has
group_frequency <- group_frequency[order(group_frequency$cow_num, decreasing = TRUE),]
for (i in 1:nrow(group_frequency)) {
  cur_group <- group_frequency$group_number[i]
  plot_histogram_for_group(max_days_group_selected, group_number = cur_group, max_days = max(max_days_group_selected$days_count), max_frequency=max(group_frequency$cow_num))
}

# assess cows that have >=13 days of records in different groups
days_per_group_processed <- days_per_group[which(days_per_group$days_count >= 13), ]
cow_count <- table(days_per_group_processed$Cow)
cows_with_multiple_rows <- names(cow_count[cow_count > 1])
cows_in_multiple_groups <- days_per_group_processed[days_per_group_processed$Cow %in% cows_with_multiple_rows, ]
###################################################################################################
################################# selected cows in selected groups ################################
###################################################################################################
total_number_of_groups_selected <- 3
all_info_final_selected <- select_and_analyze_groups(total_number_of_groups_selected, group_frequency, max_days_group_selected, all_info_final)