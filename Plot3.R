
plot3 <- function(filename) {

  dataToPlot <- readAndCleanData(filename)
  
  plot(dataToPlot$timestamp,dataToPlot$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(dataToPlot$timestamp,dataToPlot$Sub_metering_2,col="red")
  lines(dataToPlot$timestamp,dataToPlot$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()
}

readAndCleanData <- function(filename)
{
  powCom = read.csv(filename, header=TRUE, sep =";", quote = "\"", na.strings = "?")
  powCom$Date <- as.Date(powCom$Date, format="%d/%m/%Y")
  powComSel <- powCom[(powCom$Date=="2007-02-01") | (powCom$Date=="2007-02-02"),]
  powComSel$Sub_metering_1 <- as.numeric(as.character(powComSel$Sub_metering_1))
  powComSel$Sub_metering_2 <- as.numeric(as.character(powComSel$Sub_metering_2))
  powComSel$Sub_metering_3 <- as.numeric(as.character(powComSel$Sub_metering_3))
  powComSel <- transform(powComSel, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  powComSel
}
