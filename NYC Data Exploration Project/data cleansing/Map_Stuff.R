require(ggplot2)
install.packages("GGally")
require(GGally)
require(ggmap)
install.packages("revgeo")
require(revgeo)
require(maptools)
require(sf)


#Got the cleaned data
cleanTaxiData <- read.csv('cleanTaxiData.csv')

#Fixed the date time entries
cleanTaxiData$pickup_datetime <- as.POSIXlt(x = cleanTaxiData$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
cleanTaxiData$dropoff_datetime <- as.POSIXlt(x = cleanTaxiData$dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")

na_count <- data.frame(sapply(cleanTaxiData, function(y) sum(length(which(is.na(y))))))
colnames(na_count)[1] <- "NULL_Count"

na_count


summary(cleanTaxiData)

names(cleanTaxiData)







#tip outliers
tipOutliers <- boxplot(cleanTaxiData$tip_amount)

tipOutliers$stats

tempDF <- cleanTaxiData[cleanTaxiData$tip_amount <= 5, ]


#distance outliers
distOutliers <- boxplot(cleanTaxiData$trip_distance)

distOutliers$stats

tempDF2 <- cleanTaxiData[cleanTaxiData$trip_distance <= 6.42, ]



cleanTaxiData$pickup_hour = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%H")
cleanTaxiData$pickup_date = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%d")
cleanTaxiData$pickup_month = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%m")
cleanTaxiData$pickup_day = format(as.POSIXct(cleanTaxiData$pickup_datetime,format="%Y-%m-%d %H:%M:%S"),"%A")

cleanTaxiData$dropoff_hour = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%H")
cleanTaxiData$dropoff_date = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%d")
cleanTaxiData$dropoff_month = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%m")
cleanTaxiData$dropoff_day = format(as.POSIXct(cleanTaxiData$dropoff_datetime,format="%Y-%m-%d %H:%M:%S"),"%A")

names(cleanTaxiData)


