#loaded tidyverse and lubridate

library(tidyverse)
library(lubridate)

#changed working directory
setwd("/Users/SAYYAM NAHAR/Desktop/Projects/Case Study for Portfolio/Google DA- CS1 Cyclistic/Dataset")

#uploaded dataset into respective variables(or dataframes)
dec_2022 <- read.csv("202212-divvy-tripdata.csv")
jan_2023 <- read.csv("202301-divvy-tripdata.csv")
feb_2023 <- read.csv("202302-divvy-tripdata.csv")
mar_2023 <- read.csv("202303-divvy-tripdata.csv")
apr_2023 <- read.csv("202304-divvy-tripdata.csv")
may_2023 <- read.csv("202305-divvy-tripdata.csv")
jun_2023 <- read.csv("202306-divvy-tripdata.csv")
jul_2023 <- read.csv("202307-divvy-tripdata.csv")
aug_2023 <- read.csv("202308-divvy-tripdata.csv")
sep_2023 <- read.csv("202309-divvy-tripdata.csv")
oct_2023 <- read.csv("202310-divvy-tripdata.csv")
nov_2023 <- read.csv("202311-divvy-tripdata.csv")


#viewed dataframes
View(dec_2022)
View(jan_2023)
View(feb_2023)
View(mar_2023)
View(apr_2023)
View(may_2023)
View(jun_2023)
View(jul_2023)
View(aug_2023)
View(sep_2023)
View(oct_2023)
View(nov_2023)

#checked column name to compare all column name of dataframes
colnames(dec_2022)
colnames(jan_2023)
colnames(feb_2023)
colnames(mar_2023)
colnames(apr_2023)
colnames(may_2023)
colnames(jun_2023)
colnames(jul_2023)
colnames(aug_2023)
colnames(sep_2023)
colnames(oct_2023)
colnames(nov_2023)

#inspected dataframes for any inconguencies
str(dec_2022)
str(jan_2023)
str(feb_2023)
str(mar_2023)
str(apr_2023)
str(may_2023)
str(jun_2023)
str(jul_2023)
str(aug_2023)
str(sep_2023)
str(oct_2023)
str(nov_2023)


#combined all the dataframes into single frame
all_trips <- bind_rows(dec_2022,jan_2023,feb_2023,mar_2023,apr_2023,may_2023,jun_2023,jul_2023,aug_2023,sep_2023,oct_2023,nov_2023)


#removed latitude and longitude from dataframe
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#inspecting new dataframe
colnames(all_trips)  #shows list of column names
nrow(all_trips)  #shows how many rows are there in dataframe
dim(all_trips)  #shows dimensions of the dataframe
head(all_trips)  #gives the first 6 rows of dataframe
str(all_trips)  #see list of columns and its data types
summary(all_trips)  #provides statistical summary of dataframe


#to check how many observation falls under usertype
table(all_trips$member_casual)

# casual  member 
#2052401 3625209

#to add columns in the dataframe for day, month, year and day of the week
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#adding ride_length column which is time difference between ride started at(started_at) and ended_at(ended_at)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#convert ride_length from factor into numeric for further calculation
is.factor(all_trips$ride_length) #o/p: False
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length) #o/p: True

#removing ride_length data which is have time in negative as well as data from start_station_name that has name HQ
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]


#calculating mean, median, min and max for ride length
mean(all_trips_v2$ride_length)  # 1093.471
median(all_trips_v2$ride_length)  # 572
max(all_trips_v2$ride_length)  # 5909344
min(all_trips_v2$ride_length)  # 0

#to check how many observation falls under usertype
table(all_trips_v2$member_casual)
# casual  member 
#2052261 3625087

#to check no of type of bike
table(all_trips_v2$rideable_type)
#classic_bike   docked_bike electric_bike 
#     2664736         80212       2932400 

#to check bike no of bike type by member type
table(all_trips_v2$rideable_type, all_trips_v2$member_casual)
#               casual  member
#classic_bike   869046 1795690
#docked_bike     80212       0
#electric_bike 1103003 1829397

#to check the no of rides during weekday
all_trips_v2 %>% 
  group_by(day_of_week) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(day_of_week)
#  day_of_week no_of_rides
#  <ord>             <int>
#1 Sunday           736112
#2 Monday           726560
#3 Tuesday          822196
#4 Wednesday        826673
#5 Thursday         859263
#6 Friday           831474
#7 Saturday         875070

#to check no of member type during weekday
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(member_casual, day_of_week)
#   member_casual day_of_week no_of_rides
#   <chr>         <ord>             <int>
# 1 casual        Sunday           333535
# 2 casual        Monday           234709
# 3 casual        Tuesday          246809
# 4 casual        Wednesday        247852
# 5 casual        Thursday         271135
# 6 casual        Friday           309552
# 7 casual        Saturday         408669
# 8 member        Sunday           402577
# 9 member        Monday           491851
#10 member        Tuesday          575387
#11 member        Wednesday        578821
#12 member        Thursday         588128
#13 member        Friday           521922
#14 member        Saturday         466401

#to check the no of rides during a month
all_trips_v2 %>% 
  group_by(month) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(month)
#   month no_of_rides
#   <chr>       <int>
# 1 01         190301
# 2 02         190444
# 3 03         258678
# 4 04         426586
# 5 05         604817
# 6 06         719611
# 7 07         767620
# 8 08         771633
# 9 09         666321
#10 10         537077
#11 11         362454
#12 12         181806

#to check the no of rides during a month by member type
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(no_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  print(n = 24)

#compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)  #calculate mean of members and casual users
#  all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                1699.6293
#2                     member                 750.3075

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)  #calculate median of members and casual users
#  all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                      711
#2                     member                      511

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)     #calculate max of members and casual users
#  all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                  5909344
#2                     member                    93580

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)    #calculate min of members and casual users
#  all_trips_v2$member_casual all_trips_v2$ride_length
#1                     casual                        0
#2                     member                        0

#arranging dataframe in proper order
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#check avg time by each day for member vs casual user
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
#  all_trips_v2$member_casual all_trips_v2$day_of_week all_trips_v2$ride_length
#1                      casual                   Sunday                1971.2017
#2                      member                   Sunday                 837.8305
#3                      casual                   Monday                1662.3683
#4                      member                   Monday                 711.8670
#5                      casual                  Tuesday                1502.2028
#6                      member                  Tuesday                 720.0697
#7                      casual                Wednesday                1469.1514
#8                      member                Wednesday                 716.7113
#9                      casual                 Thursday                1487.6438
#10                     member                 Thursday                 718.0708
#11                     casual                   Friday                1655.6933
#12                     member                   Friday                 748.4099
#13                     casual                 Saturday                1932.3231
#14                     member                 Saturday                 837.0706

#analyze the ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%  #the above function or line calculates the number of rides and average duration
  arrange(member_casual, weekday)	

#   member_casual weekday number_of_rides average_duration
#   <chr>         <ord>             <int>            <dbl>
# 1 casual        Sun              333535            1971.
# 2 casual        Mon              234709            1662.
# 3 casual        Tue              246809            1502.
# 4 casual        Wed              247852            1469.
# 5 casual        Thu              271135            1488.
# 6 casual        Fri              309552            1656.
# 7 casual        Sat              408669            1932.
# 8 member        Sun              402577             838.
# 9 member        Mon              491851             712.
#10 member        Tue              575387             720.
#11 member        Wed              578821             717.
#12 member        Thu              588128             718.
#13 member        Fri              521922             748.
#14 member        Sat              466401             837.

#visualizing no of member and casual riders.
all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(cnt = n()) %>%
  mutate(percentage = cnt/sum(cnt)) %>% 
  ggplot(aes(x = "", y = cnt, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start=0) +
  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
  labs(title = 'Distribution of Annual and Casual Riders(in percentage)')+
  theme_void()

#Visualizing n0 of rides by bike type and member type.
all_trips_v2 %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(cnt2 = n()) %>% 
  ggplot(aes(x = rideable_type, y = cnt2, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity", color = "white", width = 0.7) +
  labs(title = "Total Number of Rides by Bike Types and Member Types", x = 'Type of Bikes', y = 'Number of Rides')

#visualizing no of rides by member type and week of the day
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity", color = "white", width = 0.7) +
  labs(title = "Total Number Of Rides by Member Type and Week of Day", x = "Weekday", y = "Number of rides")

#visualization no of rides by member casual and month
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y = number_of_rides, fill=member_casual)) +
  geom_bar(position = "dodge", stat = "identity",color = "white", width = 0.7)+
  labs(title = 'Total Number of Rides by Member Types and Months',x = 'Month',y = 'Number of Rides')

#visualization of average duration 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity", color = "white", width = 0.7)+
  labs(title ='Average Duration Of Rides During The Week By Member Type', x = 'Weekday', y = 'Average Duration')


#export data
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Divvy_Exercise/avg_ride_length.csv')

#if not working
write.csv(counts, file = "avg_ride_length", sep = ",")