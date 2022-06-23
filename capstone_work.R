#COnnect all necessary libraries
library(tidyverse)
library(dplyr)
library(skimr)
library(janitor)
library(lubridate)

#Collecting all data
apr_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202004-divvy-tripdata.csv")
may_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202005-divvy-tripdata.csv")
jun_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202006-divvy-tripdata.csv")
jul_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202007-divvy-tripdata.csv")
aug_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202008-divvy-tripdata.csv")
sep_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202009-divvy-tripdata.csv")
oct_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202010-divvy-tripdata.csv")
nov_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202011-divvy-tripdata.csv")
dec_21 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202012-divvy-tripdata.csv")
jan_22 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202101-divvy-tripdata.csv")
feb_22 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202102-divvy-tripdata.csv")
mar_22 <- read.csv("G:\\Data Analytics Course\\Capstone project\\202103-divvy-tripdata.csv")


# Before biniding them all, there a need to compare the data type columns
#https://www.rdocumentation.org/packages/janitor/versions/2.1.0/topics/compare_df_cols
compare_df_cols(apr_21,may_21,jun_21,jul_21,aug_21,sep_21,oct_21,nov_21,dec_21,jan_22,feb_22,mar_22, return = "mismatch")


# The function shows the difference in the types of values of the columns end_station_id and start_station_id.  
#in some table values are integer, while in others - character. 
# there is a need to convert numeric values to character in these columns
#before that i will add trips from month 4 to month 11 into one table 
#then will mutate the columns from numeric to character. In this way I will avoid writing mutate function to each table

apr_nov <- bind_rows(apr_21,may_21,jun_21,jul_21,aug_21,sep_21,oct_21,nov_21)
dec_may <- bind_rows(dec_21,jan_22,feb_22,mar_22)

#https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types
apr_nov <- mutate(apr_nov, end_station_id = as.character(end_station_id), start_station_id = as.character(start_station_id))
str(apr_nov)
#comparing the tables to make sure there are no inconsistencies
compare_df_cols(apr_nov, dec_may, return = "mismatch") #no mismatches and ready for merging

all_trips <- bind_rows(apr_nov,dec_may) #binding rows of to tables in to one
str(all_trips)

#let's get rid ofthe tables that not needed to the goals of this analysis
#https://www.tutorialspoint.com/how-to-remove-a-column-from-an-r-data-frame
all_trips<-subset(all_trips, select = -c(start_lat,start_station_name, start_station_id, start_lng,end_lat,end_station_name,end_station_id, end_lng))
colnames(all_trips)

# time to change the tables name to make it mo easier to understand
all_trips <-rename(all_trips, usertype = member_casual, ride_type = rideable_type)
colnames(all_trips)

#Add a column ride_length that will show the time of rent for the further analysis
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")
View(all_trips)

#extract days, months, day of week and year form the df
#before that we add column Date to have general information abt the start day,month and year
#https://www.statology.org/extract-month-from-date-in-r/
all_trips$date <- as.Date(all_trips$started_at) #Converts all data in started_at column to Date type
#now we can extract from date column the month, day and day of week
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$year <- format(as.Date(all_trips$date), "%Y")

#let's remove negative values from the table by filtering the data leaving only postitive values
#stackoverflow.com/questions
all_trips <-all_trips %>% filter(ride_length>0)
nrow(all_trips)

#Everything is ready for the analysis. Let's start it. First, let's get the general picture of rent time by client types
#https://r-coder.com/aggregate-r/
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = sum)

#Now let's see mean, median and max by clients types
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = mean)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = median)
aggregate(all_trips$ride_length ~ all_trips$usertype, FUN = max)

#agregagation of data by months
aggregate_1 <- aggregate((all_trips$ride_length ~ all_trips$usertype + all_trips$month), FUN = sum)

all_trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(usertype, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(usertype, weekday) # sorts



all_trips %>% group_by(usertype,ride_type)


all_trips %>%
  group_by(usertype) %>%  #groups by usertype and weekday
  arrange(ride_type)

nrow(all_trips[all_trips$ride_type == "classic_bike",])
nrow(all_trips[all_trips$ride_type == "docked_bike",])
nrow(all_trips[all_trips$ride_type == "electric_bike",])


classic_bike <- all_trips %>% filter(ride_type == "classic_bike")

nrow(classic_bike[classic_bike$usertype == "casual",])
nrow(classic_bike[classic_bike$usertype == "member",])

docked_bike <- all_trips %>% filter(ride_type =="docked_bike")
nrow(docked_bike[docked_bike$usertype == "casual",])
nrow(docked_bike[docked_bike$usertype == "member",])

electric_bike <- all_trips %>% filter(ride_type == "electric_bike")



nrow(electric_bike[electric_bike$usertype == "casual",])

nrow(electric_bike[electric_bike$usertype == "member",])

