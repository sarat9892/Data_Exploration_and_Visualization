rm(list = ls())

install.packages("dplyr")

library(dplyr)



nycData <- read.csv('nyc_data.csv')
nycFare <- read.csv('nyc_fare.csv')

names(nycData)
names(nycFare)

nycData <- select(nycData, -rate_code, -store_and_fwd_flag, -vendor_id)
nycFare <- select(nycFare, -vendor_id)


nycTaxiData <- merge(nycData, nycFare, by = c("medallion","hack_license", 'pickup_datetime'))

uniqRowCount <- nrow(unique(nycTaxiData))


na_count <- data.frame(sapply(nycTaxiData, function(y) sum(length(which(is.na(y))))))
colnames(na_count)[1] <- "NULL_Count"

na_count

na_count

nycTaxiData <- na.omit(nycTaxiData)

nycTaxiData <- nycTaxiData[order(nycTaxiData$pickup_datetime), ]

write.csv(nycTaxiData, 'cleanTaxiData.csv', row.names = FALSE)


