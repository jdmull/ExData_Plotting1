## Creating a time series plot for Exploratory Data Analysis project 1
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
                     Date=dmy(Date),
                     Time=hms(Time)) 
  tblPower<-filter(tblPower,(Date == selectedThursday | Date == selectedFriday) & !is.na(Global_active_power))
  tblPower<-mutate(tblPower,dt=as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))
  tblPower
}

## Plotting ____________________________________________________________________________
printGlobalActivePower<-function(powerTable)
{
  png(file="plot2.png") ## Open png device with plot2 file in working directory
  plot(p2d$dt,p2d$Global_active_power,type="l",xlab="  ",ylab="Global Active Power (killowatts)")
  dev.off() ## Close png device
}

plot2_data<-readPowerThursdayAndFriday()
plot2_data<-mutate(plot2_data,dt=as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))
printGlobalActivePower(plot2_data)

