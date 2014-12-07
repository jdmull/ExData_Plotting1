## Creating a time series plot for Exploratory Data Analysis project 1 broken apart by submetering
## requirements
##   All code for loading and printing this plot to a file is contained within this file

library("dplyr")  ## using for database functions
library("lubridate") ## using for date functions
library("datasets")
library("tidyr")

## reading in data for power usage ____________________________________________________

## readDataSet rows for Thursday 2007_02_01 and Friday 2007_02_02
readPowerThursdayAndFriday<-function()
{
  ## assignment of constants to variables, datafile & applicable dates
  dataFile<-"C:\\Users\\Internet\\Documents\\Coursara\\Exploratory Data Analysis\\Week1\\CourseProject1\\household_power_consumption.txt"
  selectedThursday <-  mdy("2/1/2007")
  selectedFriday <- mdy("2/2/2007")
  
  ## load the applicable rows from the table
  tblPower <- read.table(dataFile,sep=";",header=TRUE,na.strings="?",stringsAsFactors=FALSE)  ## this table has a non-default seperator string & na char
  ## filtering on rows containing data with given thursday & friday dates(
  tblPower <- mutate(tblPower,
                     Global_active_power=as.numeric(Global_active_power), ## coercing the data types
                     Date=dmy(Date)) 
  tblPower<-filter(tblPower,(Date == selectedThursday | Date == selectedFriday) & !is.na(Global_active_power))
  tblPower<-mutate(tblPower,dt=as.POSIXct(paste(Date, hms(Time)), format="%Y-%m-%d %H:%M:%S"))
  tblPower
}

## Plotting ____________________________________________________________________________
printGlobalActivePower<-function(powerTable)
{
  png(file="plot3.png") ## Open png device with plot2 file in working directory

  plot(powerTable$dt,powerTable$Sub_metering_1,type="l",xlab="  ",ylab="Energy Sub Metering")
  lines(powerTable$dt,powerTable$Sub_metering_2,col="red")
  lines(powerTable$dt,powerTable$Sub_metering_3,col="blue")

  
  legend( x="topright", 
          legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
          col=c("black","red","blue"), lwd=1, lty=1, 
          merge=FALSE )
  dev.off() ## Close png device
}

##plot3_data<-readPowerThursdayAndFriday()
##plot3_data<-mutate(plot3_data,dt=as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))
##printGlobalActivePower(plot3_data)
