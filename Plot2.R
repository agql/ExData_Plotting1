## Code by Agustin González-Quel - February 2016
## 
## This assignment is solved by 2 functions
##    readAndCleanData - Accepts 1 local filename in CSV format, read it and convert required 
##        fields to the suitable types. In this case:
##           Date                - type Date
##           Global_active_power - numeric
##           timestamp           - POSIXct
##
##    plot2 - prints a plot and saves the chart to a .PNG file
##

# plot2 function
plot2 <- function(filename) {

  # Call to auxiliary fucntion to read and clean data.
  dataToPlot <- readAndCleanData(filename)
  
  plot(dataToPlot$Timestamp, dataToPlot$Global_active_power, type="l", xlab="", 
       ylab="Global Active Power (Kilowatts)", cex.lab = 0.8)
  dev.copy(png, file="plot2.png", width=480, height=480)
  dev.off()
}

readAndCleanData <- function(filename)
{
  powCom = read.csv(filename, header=TRUE, sep =";", quote = "\"", na.strings = "?")
  powCom$Date <- as.Date(powCom$Date, format="%d/%m/%Y")
  powComSel <- powCom[(powCom$Date=="2007-02-01") | (powCom$Date=="2007-02-02"),]
  powComSel$Global_active_power <- as.numeric(as.character(powComSel$Global_active_power))
  powComSel <- transform(powComSel, Timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  powComSel
}
