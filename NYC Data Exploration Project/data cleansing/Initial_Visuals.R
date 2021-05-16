library(ggplot2)
install.packages("GGally")
library(GGally)
library(ggmap)

#Got the cleaned data
cleanTaxiData <- read.csv('cleanTaxiData.csv')

#Fixed the date time entries
cleanTaxiData$pickup_datetime <- as.POSIXlt(x = cleanTaxiData$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
cleanTaxiData$dropoff_datetime <- as.POSIXlt(x = cleanTaxiData$dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")

str(cleanTaxiData)


na_count <- data.frame(sapply(cleanTaxiData, function(y) sum(length(which(is.na(y))))))
colnames(na_count)[1] <- "NULL_Count"

na_count


summary(cleanTaxiData)

names(cleanTaxiData)


#passengers v tip
ggplot(cleanTaxiData, aes(passenger_count, tip_amount)) + geom_point()

ggplot(data=cleanTaxiData, aes(cleanTaxiData$tip_amount)) + geom_boxplot()

summary(cleanTaxiData$tip_amount)


#tip outliers
tipOutliers <- boxplot(cleanTaxiData$tip_amount)

tipOutliers$stats

tempDF <- cleanTaxiData[cleanTaxiData$tip_amount <= 5, ]

ggplot(tempDF, aes(passenger_count, tip_amount)) + geom_point()

ggplot(tempDF, aes(tip_amount)) + geom_histogram(binwidth = 0.1)



#distance outliers
distOutliers <- boxplot(cleanTaxiData$trip_distance)

distOutliers$stats

tempDF2 <- cleanTaxiData[cleanTaxiData$trip_distance <= 6.42, ]


ggplot(tempDF2, aes(trip_distance, tip_amount)) + geom_point()


names(cleanTaxiData)

str(cleanTaxiData)

cleanTaxiData$pickup_hour = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%H")
cleanTaxiData$pickup_date = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%d")
cleanTaxiData$pickup_month = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%m")
cleanTaxiData$pickup_day = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%A")

cleanTaxiData$dropoff_hour = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%H")
cleanTaxiData$dropoff_date = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%d")
cleanTaxiData$dropoff_month = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%m")
cleanTaxiData$dropoff_day = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%A")


head(cleanTaxiData)

ggplot(tempDF, aes(pickup_hour, tip_amount)) + geom_point()
ggplot(tempDF, aes(pickup_day, tip_amount)) + geom_point()

names(cleanTaxiData)

write.csv(cleanTaxiData, 'cleanTaxiData_26.csv', row.names = FALSE)


names(cleanTaxiData)
