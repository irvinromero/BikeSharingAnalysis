rm(list = ls())
library(data.table)
library(tidyverse)  
library(lubridate)  
library(ggplot2)  
library(ggthemes)
library(plotly)
setwd("~/Documents/bikeSharing")


file_names <- dir("data", pattern = "csv", full.names = TRUE) 
all_trips <- rbindlist(lapply(file_names,fread))

###### Header QA check 
y <- lapply(file_names, function(x) {
 x1 = read.csv(x, nrows = 10) 
 data.frame(file_name = gsub("-.*|data/", "", x),  col_names = names(x1))
})

y1 <- do.call(rbind,y)
table(y1$col_names)
y2 <- table(y1$col_names, y1$file_name)
y2
table(y2)


######

#### Data extract and clean 
all_trips$ride_time_secs <- difftime(all_trips$ended_at, all_trips$started_at, units = "secs")
all_trips <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_time_secs < 0),]
all_trips$wday <- wday(all_trips$started_at)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$month <- format(as.Date(all_trips$date), "%m")
#all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
#all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))



#### Analysis



all_trips %>% 
  summarise(number_of_rides = n(), total_ride_sec = sum(ride_time_secs), total_ride_hr = sum(ride_time_hrs), start_date =min(date), end_date = max(date))

# Compare members and casual user
table(all_trips$member_casual)


all_trips %>%
  group_by(member_casual) %>% 
  summarise(mean_ride_time_secs = mean(ride_time_secs),
            median_ride_time_secs = median(ride_time_secs)) %>%  
  data.frame()



x <- all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_time_secs)
            ,total_duration = sum(ride_time_secs)) ##%>% 		
  #arrange(member_casual, weekday)	%>% 
  
ggplot(x) +
  geom_col(aes(x = weekday, y = number_of_rides, fill = member_casual), position = "dodge") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("dodgerblue2", "green4")) + ggtitle("Total Rides by Day")+
  xlab("Weekday") + ylab("Number of Rides") + theme_classic()
  
#  theme_fivethirtyeight() + scale_fill_fivethirtyeight()

ggplot(x)  +
  geom_col(aes(x = weekday, y = average_duration, fill = member_casual),position = "dodge")+
  scale_fill_manual(values = c("dodgerblue2", "green4")) + ggtitle("Average Duration by Day")+
  xlab("Weekday") + ylab("Avg Duration") + theme_classic()


all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_time_secs)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("dodgerblue2", "green4")) + ggtitle("Count of rides by Type of Transport")+
  xlab("Transportation Type") + ylab("Count of Rides") + theme_classic()

all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_time_secs)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("dodgerblue2", "green4")) + ggtitle("Avg Duration by Type of Transport")+
  xlab("Transportation Type") + ylab("Avg duration (secs)") + theme_classic()

dateRides <- all_trips %>% 
  group_by( member_casual, month) %>% 
  summarise( total_ride_dura = sum(ride_time_secs)) 
x <- ggplot(dateRides) + 
  geom_col(aes( x = month, y = as.numeric(total_ride_dura), fill= member_casual), position = "dodge")

ggplotly(x)

