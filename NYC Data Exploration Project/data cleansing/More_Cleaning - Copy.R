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
finalTaxiData <- read.csv('boro_TaxiData_pick_drop.csv')


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
finalTaxiData3 <- finalTaxiData3[finalTaxiData3$trip_distance <= 5.75, ]


#time outliers
timeOutliers <- boxplot(finalTaxiData3$trip_time_in_secs)
timeOutliers$stats
finalTaxiData4 <- finalTaxiData3[!(finalTaxiData3$trip_time_in_secs < 1 |
                                     finalTaxiData3$trip_time_in_secs > 1560),]

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


finalTaxiData8 <- read.csv('final_taxi_data_V2.csv')

summary(finalTaxiData8)
str(finalTaxiData8)


names(finalTaxiData8)

tip_matrix <- finalTaxiData8[, c("tip_amount", "total_amount", "trip_time_in_secs",
                                 "passenger_count", "pickup_hour", "pickup_date",
                                 "pickup_month", "pickup_day")]

tip_matrix <- finalTaxiData8[, c("tip_amount", "total_amount", "trip_time_in_secs",
                                 "passenger_count")]

head(tip_matrix)

ggpairs(tip_matrix)
