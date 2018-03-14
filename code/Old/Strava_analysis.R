setwd("C:/Users/Neil/Desktop/Strava")

#install.packages("rgdal")
#install.packages("maptools")
#install.packages("httr")
#install.packages("httpuv")

library(maptools)
library(rgdal)   
library(httr)
library(httpuv)
library(tidyverse)

my_app <- oauth_app("strava",
                    key = "21055",
                    secret = "5017dd6afa6bfab93bf3cef07961193961a5cdf1"
)


my_endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

sig <- oauth2.0_token(my_endpoint, my_app, scope = "view_private",  type = NULL, use_oob = FALSE, as_header = FALSE,   use_basic_auth = FALSE, cache = FALSE)

#install.packages("jsonlite")
library(jsonlite)
jsonData <- fromJSON("https://www.strava.com/api/v3/athlete/activities?access_token=5017dd6afa6bfab93bf3cef07961193961a5cdf1&per_page=200", flatten = TRUE)

nrow(jsonData) ##returns the number of records retrieved
names(jsonData) ##returns the column names of list vector returned
head(jsonData, n=3) ## returns the first three full records

jsonData[c(6,7,12,29,31)]


jsonData_p2 <- fromJSON("https://www.strava.com/api/v3/athlete/activities?access_token=5017dd6afa6bfab93bf3cef07961193961a5cdf1&per_page=200&page=2", flatten = TRUE)

jsonData_p2[c(6,7,12,29,31)]

jsonData_p3 <- fromJSON("https://www.strava.com/api/v3/athlete/activities?access_token=5017dd6afa6bfab93bf3cef07961193961a5cdf1&per_page=200&page=3", flatten = TRUE)

jsonData_p3[c(6,7,12,29,31)]


alltimeData <-  jsonData     #rbind.pages(list(jsonData, jsonData_p2, jsonData_p3)) ##combines the three list vectors into one

nrow(alltimeData) ##show your full strava activity count for all time

alltimeData[c(6,7,12,29,31)] ##display the summary data for ALL records



###STATISTICS FOR 2016

#filter just 2017 runs

data2016 <- alltimeData %>% 
  filter(grepl("2016-", start_date)) %>% 
  filter(type == "Run")

nrow(data2016) ##count the number of records from 2016

data2016[c(5,6,11,27,29)] ## display the summary data of these 2016 rides



sum(data2016$distance)/1609.34 ##total 2016 distance, converted from meters to miles

sum(data2016[which(data2016[,27]==TRUE),6])/1609.34 ##total distance where Commute = True, also converted from meters to miles

sum(data2016[which(data2016[,27]==FALSE),6])/1609.34 ##total distance where Commute = False, again converted from meters to miles


mean(data2016[which(data2016[,27]==FALSE),6])/1609.34



###STATISTICS FOR 2017

#filter just 2017 runs
data2017 <- alltimeData %>% 
  filter(grepl("2017-", start_date)) %>% 
  filter(type == "Run")
  


nrow(data2017) ##count the number of records from 2017

data2017[c(5,6,11,27,29)] ## display the summary data of these 2016 rides



sum(data2017$distance)/1609.34 ##total 2016 distance, converted from meters to miles

sum(data2017[which(data2017[,27]==TRUE),6])/1609.34 ##total distance where Commute = True, also converted from meters to miles

sum(data2017[which(data2017[,27]==FALSE),6])/1609.34 ##total distance where Commute = False, again converted from meters to miles



#mean distance for runs by year
trip_2016 <- mean(data2016[which(data2016[,27]==FALSE),6])/1609.34
trip_2017 <- mean(data2017[which(data2017[,27]==FALSE),6])/1609.34

mean(data2017[which(data2017$commute==FALSE),6]/1609.34)




#ttest
t.test(data2016$distance, data2017$distance) #t-test comparing distance
t.test(data2016$average_speed, data2017$average_speed) #t-test comparing average speed
t.test(data2016$total_elevation_gain, data2017$total_elevation_gain) #elevation gain t-test




