bigData = read.csv("./CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")
#mpExpenses2012 is the large dataframe containing data for each MP

dataset <- split(bigData, bigData$stationname)
View(dataset)


# #Get the list of unique MP names
for (name in levels(bigData$stationname)){
  #Subset the data by MP
  tmp=subset(bigData,stationname==name)
  #Create a new filename for each MP - the folder 'mpExpenses2012' should already exist
  fn=paste('bigData/',gsub(' ','',name),sep='')
  #Save the CSV file containing separate expenses data for each MP
  write.csv(tmp,fn,row.names=FALSE)
}





