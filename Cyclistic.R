library(tidyverse)
library(skimr)
library(janitor)
library(readxl)
library(ggplot2)
library(dplyr)

#We import data for this case in excel format
q1_2019 <- read_excel("D:/Portfolio/R PROGRAMMING/Cyclistic/Data/Divvy_Trips_2019_Q1.xls")
q1_2020 <- read_excel("D:/Portfolio/R PROGRAMMING/Cyclistic/Data/Divvy_Trips_2020_Q1.xls")

table(q1_2019$member_casual)
table(q1_2020$member_casual)


#Rename columns for consistency when the tables are combined
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

#Check my data structure
str(q1_2019)
str(q1_2020)

# Convert ride_id and rideable_type to character so that they can stack correctly
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))

#Clean Up and Add Up data to prepare for analysis
colnames(all_trips)
head(all_trips) 

#Change the user type from for names to two
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(all_trips$member_casual)

all_trips$ride_length
all_trips$day_of_week
mean(all_trips$ride_length)
max(all_trips$ride_length)
mode(all_trips$day_of_week)

install.packages("lubridate")
library(lubridate)

#Compare member and casual users
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)

#Let's order the days of the week
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Mean of member casual per day of the week
aggregate(all_trips$ride_length ~ all_trips$member_casual+all_trips$day_of_week, FUN = mean)

#Now let's visualise the nuumber of rides by rider type
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Visualise for average duration
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Export data for further analysis

library(writexl)
write_xlsx(all_trips, "all_trips.xlsx")