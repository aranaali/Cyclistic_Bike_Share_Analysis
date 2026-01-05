# Google Data Analytics Professional Certificate
# Capstone Project: Divvy Trips Analysis (2019 Q1 vs 2020 Q1)

# STEP 1: Installing and Loading Required Packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
library(tidyverse)
library(lubridate)

# STEP 2: Importing Required Data

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


# STEP 3: Renaming 2019 columns to match the 2020 standard

q1_2019 <- rename(q1_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)

# Convert ride_id and rideable_type to character so they stack correctly with 2020 data

q1_2019 <- mutate(q1_2019, 
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type)) 

# Merge into a single dataframe
all_trips <- bind_rows(q1_2019, q1_2020)


# STEP 4: Clean Up and Add Data
# Consolidate member_casual labels to "member" and "casual"

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Add columns for date, month, day, year, and day_of_week
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$date, "%m")
all_trips$day <- format(all_trips$date, "%d")
all_trips$year <- format(all_trips$date, "%Y")
all_trips$day_of_week <- format(all_trips$date, "%A")

# Add ride_length calculation in MINUTES
all_trips$ride_length <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at, units = "mins"))

# Remove "bad" data (rides <= 0 minutes or maintenance/test entries)
all_trips_v2 <- all_trips %>% 
  filter(ride_length > 0)

# STEP 5: Conduct Descriptive Analysis
# Compare mean and median ride_length for members and casual riders

print("Mean Ride Length by User Type:")
aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = mean)

print("Median Ride Length by User Type:")
aggregate(ride_length ~ member_casual, data = all_trips_v2, FUN = median)

# Analyze ridership by day of the week
# Order days for better readability

all_trips_v2$day_of_week <- factor(all_trips_v2$day_of_week, 
                                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

summary_stats <- all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), 
            .groups = 'drop') %>% 
  arrange(member_casual, day_of_week)

print(summary_stats)

# STEP 6: Export Summary File for Visualization

write.csv(summary_stats, file = 'cyclistic_summary_analysis.csv', row.names = FALSE)

