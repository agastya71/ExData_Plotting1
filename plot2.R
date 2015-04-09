plot2 <- function(pathToDataFile)
{
  ## Read data file from specified location
  household_power_consumption <- read.csv(pathToDataFile, sep=";", stringsAsFactors=FALSE)
  
  ## Transform the Date and Time Strings into new column dateTimeStr and then convert into dateTime object
  ecdata_transformed <- transform(household_power_consumption,dateTimeStr = paste(household_power_consumption$Date,household_power_consumption$Time,sep=" "))
  ecdata_transformed <- transform(ecdata_transformed,dateTimeStr=strptime(dateTimeStr,"%d/%m/%Y %H:%M:%S"))

  ##Filter data buy subsetting to get only the two required days (2007-02-01 and 2007-02-02)
  filtered_ecdata <- subset(ecdata_transformed,as.Date(ecdata_transformed$dateTimeStr) == c(as.Date("2007-02-01","%Y-%m-%d"), as.Date("2007-02-02","%Y-%m-%d")))

  ## Create PNG file - stores PNG file in working directory
  png(filename="plot2.png",width=480,height=480,units="px")
  plot(x=filtered_ecdata$dateTimeStr,y=filtered_ecdata$Global_active_power,type="l",ylab="Global Active Power(kilowatts)",xlab="")
  dev.off()
}
