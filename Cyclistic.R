library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

options(scipen=999)

data_21_01 <- read_csv('202101-divvy-tripdata.csv')
data_21_02 <- read_csv('202102-divvy-tripdata.csv')
data_21_03 <- read_csv('202103-divvy-tripdata.csv')
data_21_04 <- read_csv('202104-divvy-tripdata.csv')
data_21_05 <- read_csv('202105-divvy-tripdata.csv')
data_21_06 <- read_csv('202106-divvy-tripdata.csv')
data_21_07 <- read_csv('202107-divvy-tripdata.csv')
data_21_08 <- read_csv('202108-divvy-tripdata.csv')
data_21_09 <- read_csv('202109-divvy-tripdata.csv')
data_21_10 <- read_csv('202110-divvy-tripdata.csv')
data_21_11 <- read_csv('202111-divvy-tripdata.csv')
data_21_12 <- read_csv('202112-divvy-tripdata.csv')

total <- rbind(data_21_01, data_21_02, data_21_03, data_21_04, data_21_05, data_21_06, data_21_07, data_21_08, data_21_09, data_21_10, data_21_11, data_21_12)

totalv2 <- total %>% 
  select(rideable_type, started_at, ended_at, member_casual, start_station_name, end_station_name) %>%
  mutate(time_ride = as.numeric(abs(ended_at - started_at) / 60)) %>% 
  mutate(w = started_at, m_n = started_at, h = started_at) %>% 
  filter(time_ride > 1 & (time_ride > 2 | start_station_name != end_station_name) & time_ride < 1440)

Sys.setlocale("LC_TIME", "C")

totalv2$w <- wday(totalv2$w, label = TRUE)
totalv2$m_n <- format(totalv2$m_n, "%m")
totalv2$h <- hour(totalv2$h)

 
##Number of rides per weekday
number_rides_day <- totalv2 %>% 
  group_by(w, member_casual, rideable_type) %>% 
  count(member_casual) %>% 
  filter(n > 1)

ggplot(number_rides_day, aes(x = w, y = n, fill = rideable_type)) + 
  geom_col(position = 'dodge') + 
  facet_wrap(~member_casual) + 
  labs(title = 'Number of Rides per Weekday and Rider Type', subtitle = '2021', x = 'Weekday', y = 'Number of Rides') + 
  scale_fill_discrete(name = 'Rideable Type', labels = c('Classic Bike', 'Docked Bike', 'Electric Bike'))


##Number of rides per hour on weekends
number_rides_hour_wnd <- totalv2 %>% 
  filter(w == 'Sat' | w == 'Sun') %>% 
  group_by(member_casual, rideable_type, h) %>% 
  count(member_casual) %>% 
  filter(n > 1)

ggplot(number_rides_hour_wnd, aes(x = h, y = n, color = rideable_type)) + 
  geom_line() + 
  facet_wrap(~member_casual) + 
  labs(title = 'Number of Rides per Hour and Rider Type on Weekends', subtitle = '2021', x = 'Hour', y = 'Number of Rides') +
  scale_color_discrete(name = 'Ridetable Type', labels = c('Classic Bike', 'Docked Bike', 'Electric Bike')) 


##Number of rides per hour during the week
number_rides_wk <- totalv2 %>% 
  filter(w != 'Sat' & w != 'Sun') %>% 
  group_by(h, member_casual, rideable_type) %>% 
  count(member_casual) %>% 
  filter(n > 1)

ggplot(number_rides_wk, aes(x = h, y = n, color = rideable_type)) + 
  geom_line() + 
  facet_wrap(~member_casual) + 
  labs(title = 'Number of Rides per Hour and Rider Type during the Week', subtitle = '2021', x = 'Hour', y = 'Number of Rides') + 
  scale_color_discrete(name = 'Ridetable Type', labels = c('Classic Bike', 'Docked Bike', 'Electric Bike')) 

  
##Number of rides per month
number_rides <- totalv2 %>% 
  group_by(m_n, member_casual) %>% 
  count(member_casual) %>% 
  filter(n > 1)

ggplot(number_rides, aes(x = m_n, y = n, fill = m_n)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~member_casual) + 
  labs(title = 'Number of Rides per Month and Rider Type', subtitle = '2021', x = 'Month', y = 'Number of Rides')

##Ride time per hour
time_ride_hour <- totalv2 %>% 
  group_by(h, member_casual, rideable_type) %>% 
  summarise(mean_tr = mean(time_ride))

ggplot(time_ride_hour) + 
  geom_line(aes(x = h, y = mean_tr, color = rideable_type)) + 
  facet_wrap(~member_casual) +
  labs(title = 'Number of Rides per Hour and Rider Type', subtitle = '2021', x = 'Hour', y = 'Number of Rides') + 
  scale_color_discrete(name = 'Ridetable Type', labels = c('Classic Bike', 'Docked Bike', 'Electric Bike')) 

