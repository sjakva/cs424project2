bigData = read.csv("./CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")

# turn overall excel sheet into a list of lists subsetted by station names
dataset <- split(bigData, bigData$stationname)
# View(dataset)

# each row is a list (station/stop); dataset is a list of lists
for (row in 1:NROW(dataset)) {
  # print(dataset[[row]]$stationname[[1]][1])
  write.csv(dataset[[row]], paste("./station_", gsub("/", "--",dataset[[row]]$stationname[[1]][1]), ".csv", sep = ""), row.names = FALSE)
}




