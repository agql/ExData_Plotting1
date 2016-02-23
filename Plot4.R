## Code by Agustin González-Quel - February 2016
## 
## This assignment is solved by 2 functions
##    readAndCleanData - Accepts 1 local filename in CSV format, read it and convert required 
##        fields to the suitable types. In this case:
##           Date                  - type Date
##           Global_active_power   - numeric
##           Global_reactive_power - numeric
##           Voltage               - numeric
##           Sub_metering_1(2 & 3) - numeric
##           timestamp             - POSIXct
##
##    plot4 - prints four charts in a matrix
##

# plot3 function
plot4 <- function(filename) {

  dataToPlot <- readAndCleanData(filename)

  png(filename = "plot4.png", width = 480, height = 480, units = "px", pointsize = 12)
  par(mfrow=c(2,2))
  
  # 
  plot(dataToPlot$timestamp,dataToPlot$Global_active_power, type="l", xlab="", ylab="Global Active Power")
  #
  plot(dataToPlot$timestamp,dataToPlot$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  # Plot 3
  plot(dataToPlot$timestamp,dataToPlot$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(dataToPlot$timestamp,dataToPlot$Sub_metering_2,col="red")
  lines(dataToPlot$timestamp,dataToPlot$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), 
         c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),
         cex=.5, lty=c(1,1), lwd=c(1,1)) 

  #Plot 4
  plot(dataToPlot$timestamp,dataToPlot$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
  
  dev.off()
}

readAndCleanData <- function(filename)
{
  powCom = read.csv(filename, header=TRUE, sep =";", quote = "\"", na.strings = "?")
  powCom$Date <- as.Date(powCom$Date, format="%d/%m/%Y")
  powComSel <- powCom[(powCom$Date=="2007-02-01") | (powCom$Date=="2007-02-02"),]
  powComSel$Global_active_power <- as.numeric(as.character(powComSel$Global_active_power))
  powComSel$Global_reactive_power <- as.numeric(as.character(powComSel$Global_reactive_power))
  powComSel$Voltage <- as.numeric(as.character(powComSel$Voltage))
  powComSel$Sub_metering_1 <- as.numeric(as.character(powComSel$Sub_metering_1))
  powComSel$Sub_metering_2 <- as.numeric(as.character(powComSel$Sub_metering_2))
  powComSel$Sub_metering_3 <- as.numeric(as.character(powComSel$Sub_metering_3))
  powComSel <- transform(powComSel, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  powComSel
}
