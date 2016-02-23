## Code by Agustin González-Quel - February 2016
## 
## This assignment is solved by 2 functions
##    readAndCleanData - Accepts 1 local filename in CSV format, read it and convert required 
##        fields to the suitable types. In this case:
##           Date                - type Date
##           Global_active_power - numeric
##           timestamp           - POSIXct
##
##    plot1 - print a histogram and saves the chart to a .PNG file
##

# plot1 function
plot1 <- function(filename) {
  
  # Call to auxiliary fucntion to read and clean data.
  dataToPlot <- readAndCleanData(filename)

  hist(dataToPlot$Global_active_power, main = paste("Global Active Power"), 
       col="red", xlab="Global Active Power (Kilowatts)", 
       cex.lab = 0.8, cex.main = 0.9
  )
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
}

# readAndCleanData function
readAndCleanData <- function(filename)
{
# reads and convert Date
  powCom = read.csv(filename, header=TRUE, sep =";", quote = "\"", na.strings = "?")
  powCom$Date <- as.Date(powCom$Date, format="%d/%m/%Y")

  # Select the suitable dates so further calculation takes less time.
  powComSel <- powCom[(powCom$Date=="2007-02-01") | (powCom$Date=="2007-02-02"),]
  
  powComSel$Global_active_power <- as.numeric(as.character(powComSel$Global_active_power))
  powComSel <- transform(powComSel, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  # Returns data ready to be printed.
  powComSel
}
