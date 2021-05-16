require(ggplot2)
#install.packages("GGally")
require(GGally)
require(ggmap)
#install.packages("revgeo")
require(revgeo)
require(maptools)
require(sf)
library(dplyr)


#Got the cleaned data
preTaxiData <- read.csv('boro_TaxiData_pick_drop.csv')
summary(preTaxiData)
str(preTaxiData)

#Fixed the date time entries
finalTaxiData$pickup_datetime <- as.POSIXlt(x = finalTaxiData$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
finalTaxiData$dropoff_datetime <- as.POSIXlt(x = finalTaxiData$dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")

na_count <- data.frame(sapply(finalTaxiData, function(y) sum(length(which(is.na(y))))))
colnames(na_count)[1] <- "NULL_Count"

na_count

finalTaxiData2 <- na.omit(finalTaxiData)

summary(finalTaxiData2)


na_count <- data.frame(sapply(finalTaxiData2, function(y) sum(length(which(is.na(y))))))
colnames(na_count)[1] <- "NULL_Count"

na_count


#tip outliers
tipOutliers <- boxplot(finalTaxiData2$tip_amount)
tipOutliers$stats
finalTaxiData3 <- finalTaxiData2[finalTaxiData2$tip_amount <= 5, ]


#distance outliers
distOutliers <- boxplot(finalTaxiData3$trip_distance)
distOutliers$stats
finalTaxiData_2 <- finalTaxiData[finalTaxiData$trip_distance <= 5.75, ]


#time outliers
timeOutliers <- boxplot(finalTaxiData3$trip_time_in_secs)
timeOutliers$stats
finalTaxiData4 <- finalTaxiData3[!(finalTaxiData3$trip_time_in_secs < 1 |
                                     finalTaxiData3$trip_time_in_secs > 1560),]

#amount outliers
chargeOutliers <- boxplot(finalTaxiData$total_amount)
chargeOutliers$stats
tempDF <- finalTaxiData[!(finalTaxiData$total_amount > 22.5),]

summary(finalTaxiData4)


finalTaxiData4 <- select(finalTaxiData4, -surcharge, -mta_tax, -tolls_amount, -fare_amount)

summary(finalTaxiData4)

finalTaxiData5 <- finalTaxiData4[!(finalTaxiData4$total_amount < 0),]

summary(finalTaxiData5)

write.csv(finalTaxiData5, 'final_taxi_data.csv', row.names = FALSE)
finalTaxiData6 <- read.csv('final_taxi_data.csv')


summary(finalTaxiData6)


#finalTaxiData5$pickup_hour = format(as.POSIXct(finalTaxiData5$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%H")
#finalTaxiData5$pickup_date = format(as.POSIXct(finalTaxiData5$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%d")
#finalTaxiData5$pickup_month = format(as.POSIXct(finalTaxiData5$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%m")
#finalTaxiData5$pickup_day = format(as.POSIXct(finalTaxiData5$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%A")

#finalTaxiData5$dropoff_hour = format(as.POSIXct(finalTaxiData5$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%H")
#finalTaxiData5$dropoff_date = format(as.POSIXct(finalTaxiData5$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%d")
#finalTaxiData5$dropoff_month = format(as.POSIXct(finalTaxiData5$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%m")
#finalTaxiData5$dropoff_day = format(as.POSIXct(finalTaxiData5$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%A")


finalTaxiData7 <- finalTaxiData6[!(finalTaxiData6$pickup_boro == ''), ]

write.csv(finalTaxiData7, 'final_taxi_data_V2.csv', row.names = FALSE)


#START HERE


finalTaxiData <- read.csv('final_taxi_data_V2.csv')

summary(finalTaxiData)
str(finalTaxiData8)


names(finalTaxiData)

tip_matrix <- finalTaxiData[, c("tip_amount", "total_amount", "trip_distance", "trip_time_in_secs",
                                "passenger_count", "pickup_hour", "pickup_date",
                                "pickup_month", "pickup_day")]

tip_temp <- finalTaxiData_2[, c("tip_amount", "total_amount", "trip_distance", "trip_time_in_secs",
                                "passenger_count", "pickup_hour", "pickup_date",
                                "pickup_month", "pickup_day")]



names(tip_matrix)
head(tip_matrix)


#Tip Vs Total Charge
ggplot(tip_matrix, aes(total_amount, tip_amount)) + geom_point() +
  
  theme_dark(base_size = 16) +
  ylim(0,5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = 'red', size = 1.5) +
  annotate(x=100, y=1.5, 
           label=paste("R = ", round(cor(tip_matrix$total_amount, tip_matrix$tip_amount),2)), 
           geom="text", size=8) +
  ggtitle('Tip Amount Vs Total Amount') +
  xlab('Total Charge') +
  ylab('Tip')



#Tip Vs Time of day
ggplot(tip_matrix, aes(passenger_count, tip_amount)) + geom_point() +
  
  theme_dark(base_size = 16) +
  ggtitle('Tip Amount Vs Passenger Count') +
  xlab('Number of Passengers') +
  ylab('Tip')


#Tip Vs Passenger Count
ggplot(tip_matrix, aes(pickup_hour, tip_amount)) + geom_point() +
  
  theme_dark(base_size = 16) +
  ggtitle('Tip Amount Vs Time of day') +
  xlab('Time of Day') +
  ylab('Tip')


#Tip Vs Distance Travelled
ggplot(tip_matrix, aes(trip_distance, tip_amount)) + geom_point() +
  
  xlim(0,5.75) +
  
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = 'red', size = 1.5) +
  annotate(x = 5.75, y = 2.4, 
           label=paste("R = ", round(cor(tip_matrix$trip_distance, tip_matrix$tip_amount),2)), 
           geom="text", size=5) +
  
  theme_dark(base_size = 15) +
  ggtitle('Tip Amount Vs Trip Distance') +
  xlab('Trip Distance') +
  ylab('Tip')




#Tip Vs Day of week
ggplot(tip_matrix, aes(pickup_day, tip_amount)) + geom_point() +
  
  theme_dark(base_size = 12) +
  ggtitle('Tip Vs Day of Week') +
  xlab('Day of Week') +
  ylab('Tip')


#Tip Vs Day of week
ggplot(tip_matrix, aes(pickup_hour, tip_amount)) + geom_point() +
  
  theme_dark(base_size = 12) +
  ggtitle('Tip Vs Hour of Day') +
  xlab('Hour of Day') +
  ylab('Tip')


#Tip Vs Month
ggplot(tip_matrix, aes(pickup_month, tip_amount)) + geom_point() +
  
  theme_dark(base_size = 12) +
  ggtitle('Tip Vs Month') +
  xlab('Month') +
  ylab('Tip')



nyc_map <- get_map(location = c(lon = -74.00, lat = 40.77), maptype = "terrain", zoom = 12)
