plot4 <- function(pathToDataFile)
{
  ## Read data file from specified location
  household_power_consumption <- read.csv(pathToDataFile, sep=";", stringsAsFactors=FALSE)
  
  ## Transform the Date and Time Strings into new column dateTimeStr and then convert into dateTime object
  ecdata_transformed <- transform(household_power_consumption,dateTimeStr = paste(household_power_consumption$Date,household_power_consumption$Time,sep=" "))
  ecdata_transformed <- transform(ecdata_transformed,dateTimeStr=strptime(dateTimeStr,"%d/%m/%Y %H:%M:%S"))

  ##Filter data buy subsetting to get only the two required days (2007-02-01 and 2007-02-02)
  filtered_ecdata <- subset(ecdata_transformed,as.Date(ecdata_transformed$dateTimeStr) == c(as.Date("2007-02-01","%Y-%m-%d"), as.Date("2007-02-02","%Y-%m-%d")))

  ## Create PNG file - stores PNG file in working directory
  png(filename="plot4.png",width=480,height=480,units="px")
  par(mfrow=c(2,2))
  plot(x=filtered_ecdata$dateTimeStr,y=filtered_ecdata$Global_active_power,type="l",ylab="Global Active Power(kilowatts)",xlab="")
  plot(x=filtered_ecdata$dateTimeStr,y=filtered_ecdata$Voltage,xlab="datetime",ylab="Voltage",type="l")  
  plot(x=filtered_ecdata$dateTimeStr,y=filtered_ecdata$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering",col="black",lwd=1.0)
  lines(filtered_ecdata$dateTimeStr,filtered_ecdata$Sub_metering_2,col="red",lwd=1.0)
  lines(filtered_ecdata$dateTimeStr,filtered_ecdata$Sub_metering_3,col="blue",lwd=1.0)
  legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),lwd=c(1,1,1),col=c("black","red","blue"),cex=0.5)
  plot(x=filtered_ecdata$dateTimeStr,y=filtered_ecdata$Global_reactive_power,xlab="datetime",ylab="Global_reactive_power",type="l")
  dev.off()
}
