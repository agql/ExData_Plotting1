
plot1 <- function(filename) {
  
  dataToPlot <- readAndCleanData(filename)

  hist(dataToPlot$Global_active_power, main = paste("Global Active Power"), col="blue", xlab="Global Active Power (KW)")
  dev.copy(png, file="plot1.png", width=480, height=480)
  dev.off()
}

readAndCleanData <- function(filename)
{
  powCom = read.csv(filename, header=TRUE, sep =";", quote = "\"", na.strings = "?")
  powCom$Date <- as.Date(powCom$Date, format="%d/%m/%Y")
  powComSel <- powCom[(powCom$Date=="2007-02-01") | (powCom$Date=="2007-02-02"),]
  powComSel$Global_active_power <- as.numeric(as.character(powComSel$Global_active_power))
  powComSel <- transform(powComSel, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  powComSel
}