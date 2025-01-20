library(tidyverse)

c24_08 <- read.csv("202408-divvy-tripdata.csv")
c24_09 <- read.csv("202409-divvy-tripdata.csv")
c24_10 <- read.csv("202410-divvy-tripdata.csv")

#vertically merge dataset from 12 months
bike_trips <- bind_rows(c24_08, c24_09, c24_10)

bike_trips_1 <- drop_na(bike_trips) %>% #drop rows with na
  distinct() #drop duplicate value

skim_without_charts(bike_trips_1) #summary for table

bike_trips_1<- bike_trips_1 %>%
  mutate(
    member_casual = as.factor(member_casual),
    rideable_type = as.factor(rideable_type)) #change column to data factor data type because the option are exact

bike_trips_2 <- bike_trips_1%>%
  filter(grepl("^2024", started_at),
         grepl("^2024", ended_at), #clean data that is not match year of report
         rowSums(!is.na(.) & . != "") >0) 

bike_trips_3 <- bike_trips_2 %>% 
  mutate( # mutate data to datetime format
    started_at_parsed = ymd_hms(started_at, quiet = FALSE),
    ended_at_parsed = ymd_hms(ended_at, quiet = FALSE)) %>%
  mutate( # separate parsed column into date and time columns
    started_date = as.Date(started_at_parsed),
    started_time = format(started_at_parsed, "%H:%M:%S"),
    ended_date = as.Date(ended_at_parsed),
    ended_time = format(ended_at_parsed, "%H:%M:%S")) %>%
  mutate( # removing milisecond on every row from started_at and ended_at column
    started_time = sub("\\.\\d{3}$", "", started_time),
    ended_time = sub("\\.\\d{3}$", "", ended_time)) %>%
  mutate( # format data as time and date
    started_time = hms::as_hms(started_time),
    ended_time = hms::as_hms(ended_time),
    started_date = ymd(started_date),
    ended_date = ymd(ended_date))

bike_trips_4 <- bike_trips_3 %>% # convert gap time between ended and started as seconds
  mutate( # removing milisecond on every row from started_at and ended_at column
    started_at_parsed = sub("\\.\\d{3}$", "", started_at_parsed),
    ended_at_parsed = sub("\\.\\d{3}$", "", ended_at_parsed)) %>%
  mutate( # convert gap time between ended and started as seconds
    spend_time = as.numeric(difftime(ymd_hms(ended_at_parsed), ymd_hms(started_at_parsed), units = "secs")))

invalid_rows <- bike_trips_3 %>% # check for invalid rows created by code above
  filter(is.na(ymd_hms(started_at_parsed, quiet = FALSE)) |
         is.na(ymd_hms(ended_at_parsed, quiet = FALSE)))

bike_trips_4 <- bike_trips_3 %>%
  mutate( # Combine date and time into full datetime strings
    started_at_combined = paste(started_date, started_time),
    ended_at_combined = paste(ended_date, ended_time),
    # Parse the combined strings into datetime format
    started_at_parsed = ymd_hms(started_at_combined, quiet = FALSE),
    ended_at_parsed = ymd_hms(ended_at_combined, quiet = FALSE)) %>%
  mutate( # calculate the spend time between starting and ending the ride
    spend_time = as.numeric(difftime(ended_at_parsed, started_at_parsed), units = "secs")) %>%
  mutate( # mutate the data to include where in the days of the week people using the bike
    start_day = wday(started_date, label = FALSE, week_start =  1),
    end_day = wday(ended_date, label = FALSE, week_start = 1),
    month_of_ride =  month(started_date, label = TRUE, abbr = FALSE)) # mutate the data to include month people using the bike

invalid_rows_2 <- bike_trips_4 %>% #based on 1st invalid rows check if the same data still invalid
  filter(ended_date == "2024-10-23", ended_time == 00:00:00)

bike_trips_5 <- bike_trips_4 %>% # remove the unnecessary column
  select(-started_at, -ended_at, -started_at_combined, -ended_at_combined)

invalid_rows_3 <- bike_trips_5 %>% # removing unsuitable data for analysis
  filter(month_of_ride == "July", month_of_ride == "November")

bike_trips_6 <- bike_trips_5 %>% # rename column
  rename(
    started_at = started_at_parsed,
    ended_at = ended_at_parsed,
    ride_time = spend_time,
    user_type = member_casual,
    bike_type = rideable_type) %>%
  drop_na() %>% # clean row from NA value and blank space after mutation
  filter( # filter for missing value and blank space
    rowSums(!is.na(.)) >0,
    start_station_name != "",
      end_station_name != "",
    month_of_ride != "July",
      ride_time >= 60) # remove ride_time below 60 seconds

# Identify rows with missing values
missing_rows <- bike_trips_6[!complete.cases(bike_trips_6), ]

# Identify duplicate rows
duplicate_rows <- bike_trips_6[duplicated(bike_trips_6), ]

# check the structure of dataset
str(bike_trips_6)

# confirm columns have appropriate data types
summary(bike_trips_6)

# Identify character columns only
character_cols <- sapply(bike_trips_6, is.character)

# Filter rows with blanks in character columns
blank_rows <- bike_trips_6[rowSums(bike_trips_6[, character_cols] == "" | 
                                     bike_trips_6[, character_cols] == " ") > 0, ]

# check for inconsistencies
inconsistent_rows <- bike_trips_6 %>%
  filter(started_at > ended_at)

##ANALYSIS 
##1
bike_ride_median <- bike_trips_6 %>% #find median of ride time for both type of membership
  aggregate(ride_time ~ user_type, FUN = median) %>%
  set_names(c("user", "ride_time(in secs)"))

##2
bike_ride_min <- bike_trips_6 %>% # find min ride time for both type of membership
  aggregate(ride_time ~ user_type, FUN = min) %>%
  set_names(c("user", "ride_time(in secs)"))

##3
bike_ride_max <- bike_trips_6 %>% # find max ride time for both type of membership
  aggregate(ride_time ~ user_type, FUN = max) %>%
  set_names(c("user", "ride_time(in secs)"))

##4
bike_ride_mean <- bike_trips_6 %>% # find average ride time for both type of membership
  aggregate(ride_time ~ user_type, FUN = mean) %>%
  set_names(c("user", "ride_time(in secs)"))

##5
bike_ride_length_time <- bike_trips_6 %>% # find total ride time in hour for both type of membership
  aggregate(ride_time ~ user_type, 
            FUN = function(ride_time) round(sum(ride_time) / 3600, 1)) %>%
  set_names(c("user", "total_ride_time_in_hour"))

ride_time_spent <- bike_trips_6 %>% # Combine the data for rides over and under 1 hour
  mutate(ride_time_type = ifelse(ride_time >= 800, "longer_trip", "short_trip")) %>%
  group_by(user_type, ride_time_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(
    user_ride_time = paste(user_type, ride_time_type, sep = "_"),
    total_count = sum(count),
    proportion = count / total_count * 100) %>%
  arrange("user_type, ride_time_type")

avg_time_bike_type <- bike_trips_6 %>% # Combine the data for rides over and under 1 hour
  group_by(user_type, bike_type) %>%
  summarize(avg_ride_time = mean(ride_time),
            median_ride_time = median(ride_time),
            count = n(), .groups = "drop") %>%
  mutate(user_bike_type = paste(user_type, bike_type, sep = "_")) %>%
  arrange("user_type, bike_type")

##6
bike_ride_count_user <- bike_trips_6 %>% # count total number of membership for each type of membership
  group_by(user_type) %>% 
  summarize(count = n(), .groups = "drop") %>%
  mutate(
    total_count = sum(count),
    proportion = count / total_count * 100) %>%
  arrange("user_type", "count")

##7
bike_ride_count_month <- bike_trips_6 %>% # count total ride for both type of membership for each month
  group_by(user_type, month_of_ride) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(user_type, month_of_ride)

##8
starting_days_mean <- bike_trips_6 %>% # find average days of the week for both type of membership
  group_by(user_type) %>%
  summarise(avg_starting_days = mean(start_day), .groups = "drop")
 
starting_days_count <- bike_trips_6 %>%
  group_by(user_type, start_day) %>%
  summarise(count = n(), .groups = "drop")

##9
ending_days_mean <- bike_trips_6 %>% # find average days of the week for both type of membership
  group_by(user_type) %>%
  summarise(avg_ending_days = mean(end_day), .groups = "drop")

ending_days_count <- bike_trips_6 %>%
  group_by(user_type, end_day) %>%
  summarise(count = n(), .groups = "drop")

##10
bike_type_count <- bike_trips_6 %>%
  group_by(user_type, bike_type) %>%
  summarise(count = n(), .groups = "drop")

##11
end_station_count <- bike_trips_6 %>%
  group_by(user_type, end_station_id) %>%
  summarise(unique_station_id = n_distinct(end_station_id), .groups = "drop")

freq_end_station <- bike_trips_6 %>%
  group_by(user_type, end_station_id) %>%
  summarize(count = n(),
            .groups = "drop") %>%
  arrange(desc(count))

freq_end_station_summary <- freq_end_station %>%
  filter(user_type == "member") %>%
  mutate(count_type = case_when(
    count >= 1201 ~ "too_freq_trips",
    count > 600 & count <= 1200 ~ "frequent_trips",
    count > 200 & count <= 600 ~ "medium_trips",
    count <= 200 ~ "less_trips")) %>%
  group_by(user_type, count_type) %>%
  summarise(max_count = max(count),
          min_count = min(count),
          total_count = sum(count),
          avg_count = mean(count), .groups = "drop")

freq_end_station_count_850 <- freq_end_station %>%
  group_by(user_type, end_station_id, count) %>%
  filter(count >= 850, 
         user_type == "member") %>%
  summarise(count_unique_id = n_distinct(end_station_id), .groups = "drop")
  
##12
start_station_count <- bike_trips_6 %>%
  group_by(user_type, start_station_id) %>%
  summarise(unique_station_id = n_distinct(end_station_id), .groups = "drop")

freq_start_station <- bike_trips_6 %>%
  group_by(user_type, start_station_id) %>%
  summarize(count = n(),
            .groups = "drop") %>%
  arrange(desc(count))

freq_start_station_summary <- freq_start_station %>%
  filter(user_type == "member") %>%
  mutate(count_type = case_when(
    count >= 1201 ~ "too_freq_trips",
    count > 600 & count <= 1200 ~ "frequent_trips",
    count > 200 & count <= 600 ~ "medium_trips",
    count <= 200 ~ "less_trips")) %>%
  group_by(user_type, count_type) %>%
  summarise(max_count = max(count),
            min_count = min(count),
            total_count = sum(count),
            avg_count = mean(count), .groups = "drop")

freq_start_station_count_870 <- freq_start_station %>%
  group_by(user_type, start_station_id, count) %>%
  filter(count >= 870, 
         user_type == "member") %>%
  summarise(count_unique_id = n_distinct(start_station_id), .groups = "drop")

##13
freq_full_station <- bike_trips_6 %>%
  unite(full_station_id, start_station_id, end_station_id, sep = "_") %>%
  group_by(user_type, full_station_id) %>%
  summarize(count = n(), .groups = "drop") %>% # count data based on similar array 
  arrange(desc(count)) 

freq_full_station_summary <- freq_full_station %>%
  filter(user_type == "member") %>%
  mutate(count_type = case_when(
    count >= 1201 ~ "too_freq_trips",
    count > 600 & count <= 1200 ~ "frequent_trips",
    count > 200 & count <= 600 ~ "medium_trips",
    count <= 200 ~ "less_trips")) %>%
  group_by(user_type, count_type) %>%
  summarise(max_count = max(count),
            min_count = min(count),
            total_count = sum(count),
            avg_count = mean(count), .groups = "drop")

freq_full_station_count_870 <- freq_full_station %>%
  group_by(user_type, full_station_id, count) %>%
  filter(count >= 870, 
         user_type == "member") %>%
  summarise(count_unique_id = n_distinct(full_station_id), .groups = "drop")

##14
ride_time_overview <- bike_trips_6 %>%
  group_by(user_type) %>%
  summarize(avg_ride_time = mean(ride_time),
            sum_ride_time = sum(ride_time),
            count_of_rides = n(),
            median_ride_time = median(ride_time)) %>%
  arrange(user_type, count_of_rides, avg_ride_time, sum_ride_time, median_ride_time)

##15
started_count_24_hours <- bike_trips_6 %>%
  mutate(
    started_time = hms::parse_hms(started_time),   # Parse start_time into hms format
    started_hour = lubridate::hour(started_time)) %>%  # Extract the hour (0-23)
  group_by(user_type, started_hour) %>%  # Group by user_type, bike_type, and start_hour
  summarise(
    count = n(),  # Count the number of rides for each hour
    .groups = "drop")

##16
ended_count_24_hours <- bike_trips_6 %>%
  mutate(
    ended_time = hms::parse_hms(ended_time),   # Parse start_time into hms format
    ended_hour = lubridate::hour(ended_time)) %>%  # Extract the hour (0-23)
  group_by(user_type, ended_hour) %>%  # Group by user_type, bike_type, and start_hour
  summarise(
    count = n(),  # Count the number of rides for each hour
    .groups = "drop")

##17
## Final Analysis
casual_matching <- bike_trips_6 %>%
  mutate(started_time = hms::parse_hms(started_time),   
    started_hour = lubridate::hour(started_time),
    ended_time = hms::parse_hms(ended_time),
    ended_hour = lubridate::hour(ended_time),
    full_station_id = paste(start_station_id, end_station_id, sep = "_")) %>%
  filter(user_type == "casual", 
         !(start_day %in% c("6", "7")),
         ride_time <= 800,
         started_hour >= 7 & started_hour <= 19,
         ended_hour >= 8 & ended_hour <= 19) %>%
  right_join(freq_full_station_count_870, by = "full_station_id") %>%
  group_by(full_station_id, ride_time, start_day, started_hour, ended_hour) %>%
  drop_na() %>%
  summarise(count = n(), .groups = "drop")

target_market_matching <- bike_trips_6 %>%
  filter(user_type == "casual") %>%
  mutate(started_time = hms::parse_hms(started_time),   
         started_hour = lubridate::hour(started_time),
         ended_time = hms::parse_hms(ended_time),
         ended_hour = lubridate::hour(ended_time),
         full_station_id = paste(start_station_id, end_station_id, sep = "_")) %>%
  left_join(freq_full_station_count_870, by = "full_station_id") %>%
  mutate(description = ifelse(!is.na(full_station_id) &
         !(start_day %in% c("6", "7")) &
         ride_time <= 800 &
         started_hour >= 7 & started_hour <= 19 &
         ended_hour >= 8 & ended_hour <= 19,
         "target_market",
         "non_target_market")) %>%
  group_by(description) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    total_count = sum(count),
    proportion = count / total_count * 100)
  
casual_matching_summary <- casual_matching %>%
  summarise(min_ride_time = min(ride_time),
            max_ride_time = max(ride_time),
            avg_ride_time = mean(ride_time),
            med_ride_time = median(ride_time),
            min_start_day = min(start_day),
            max_start_day = max(start_day),
            min_started_hour = min(started_hour),
            max_started_hour = max(started_hour),
            min_ended_hour = min(ended_hour),
            max_ended_hour = max(ended_hour),
            count = n())

  
##############VISUALIZE
##1 
plot_count_user_type <- ggplot(bike_ride_count_user, aes(x = "", y = proportion, fill = user_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = paste0(count, " (", round(proportion, 1), "%)")),
    position = position_stack(vjust = 0.5)) + # Position labels in the middle of each slice
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of User Type",
    fill = "User Type",
    x = NULL,
    Y = NULL) +
  theme_void() +
  theme(legend.position = "bottom")

##2A
plot_avg_ride_bike_type <- ggplot(data = avg_time_bike_type,
                                  aes(x = bike_type, y = count, fill = user_type)) + # Create the stacked bar chart
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(avg_ride_time, 2),
                group = user_type),
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            color = "black", 
            size = 4) +
  labs(
    x = "Bike Type",
    y = "Count of Bike Type",
    fill = "User Type",
    title = "Average Ride Time Chart by Bike Type & User Type "
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme_void() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##2B
plot_spent_time <- ggplot(ride_time_spent, aes(x = "", y = proportion, fill = user_ride_time)) + # Create the stacked bar chart
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = paste0(count, " (", round(proportion, 1), "%)")), 
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white") +
  coord_polar(theta = "y") +
  labs(
    x = NULL,
    y = NULL,
    fill = "User Type",
    title = "Proportion of Ride Time Spent by User Type and Duration") +
  theme_minimal() +
  theme_void() +
  theme(legend.position = "bottom")

##3
plot_starting_day <- ggplot(data = starting_days_count) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(x = start_day, y = count, fill = user_type)) +
  geom_text(aes(x = start_day, 
                y = count,
                label = count, 
                group = user_type),
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            color = "black", 
            size = 4) +
  labs(
    x = "Starting Day",
    y = "Count of Starting Day",
    fill = "User Type",
    title = "Daily Cyclistic Usage by User Type") +
  theme_minimal() +
  theme_void() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##4A
plot_starting_24_hours <- ggplot(data = started_count_24_hours) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(x = started_hour, y = count, fill = user_type)) +
  geom_text(aes(x = started_hour, 
                y = count,
                label = count,
                group = user_type),
            position = position_dodge(width = 0.9),
            vjust = -1.0,
            angle = 90,
            color = "black", 
            size = 4) +
  labs(
    x = "24 Hours",
    y = "Count of Trips",
    fill = "User Type",
    title = "Cyclistic: Starting Hour by User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##4B
plot_ending_24_hours <- ggplot(data = ended_count_24_hours) +
  geom_bar(stat = "identity", position = "dodge", 
           aes(x = ended_hour, y = count, fill = user_type)) +
  geom_text(aes(x = ended_hour, 
                y = count,
                label = count,
                group = user_type),
            position = position_dodge(width = 0.9),
            vjust = -1.0,
            angle = 90,
            color = "black", 
            size = 4) +
  labs(
    x = "24 Hours",
    y = "Count of Trips",
    fill = "User Type",
    title = "Cyclistic: Ending Hour by User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##5
plot_target_market <- ggplot(target_market_matching, 
                             aes(x = "", y = proportion, fill = description)) + # Create the stacked bar chart
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = paste0(count, " (", round(proportion, 1), "%)")), 
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white") +
  coord_polar(theta = "y") +
  labs(
    x = NULL,
    y = NULL,
    fill = "",
    title = "Percentage of Target Market From Total Casual User",
    caption = "Data from Sep - Nov 2024") +
  theme_minimal() +
  theme_void() +
  theme(legend.position = "bottom")
