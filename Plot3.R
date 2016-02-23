## Code by Agustin González-Quel - February 2016
## 
## This assignment is solved by 2 functions
##    readAndCleanData - Accepts 1 local filename in CSV format, read it and convert required 
##        fields to the suitable types. In this case:
##           Date                  - type Date
##           Sub_metering_1(2 & 3) - numeric
##           timestamp             - POSIXct
##
##    plot3 - prints the three lines in a chart to screen and to a .PNG file
##

# plot3 function
plot3 <- function(filename) {

  # Call to auxiliary fucntion to read and clean data.
  dataToPlot <- readAndCleanData(filename)
  
  png(filename = "plot3.png", width = 480, height = 480, units = "px", pointsize = 12)
  par(mar=c(2,4,1,1))
  plot(dataToPlot$timestamp,dataToPlot$Sub_metering_1, type="l", xlab="", 
       ylab="Energy sub metering", cex.lab = 0.8, cex.axis = 0.8)
  lines(dataToPlot$timestamp,dataToPlot$Sub_metering_2,col="red")
  lines(dataToPlot$timestamp,dataToPlot$Sub_metering_3,col="blue")
  legend("topright", col=c("black","red","blue"), 
         c("Sub_metering_1","Sub_metering_2", "Sub_metering_3") )
  
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
