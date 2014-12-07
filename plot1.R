## Creating a histogram plot for Exploratory Data Analysis project 1
## requirements
##   All code for loading and printing this plot to a file is contained within this file

library("dplyr")  ## contains mutate function
library("lubridate")
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
  tblPower <- tblPower %>% 
    ## filtering on rows containing data with given thursday & friday dates
    mutate(Global_active_power=as.numeric(Global_active_power), ## coercing the data types
         Date=dmy(Date)) %>% ## ,
         ## Time=hms(Time),
         ## DateTime=update(date,hours=hours(Time),minutes(Time),seconds(Time)))
    filter((Date == selectedThursday | Date == selectedFriday) & !is.na(Global_active_power)) 
  tblPower
}

## Plotting ____________________________________________________________________________
printHistogram<-function(columnPower)
{
  png(file="plot1.png") ## Open png device with plot1 file in working directory
  
  ## Histogram
  hist(x=columnPower,
       main="Global Active Power",
       xlab="Global Active Power (killowatts)",
       ylab="Frequency",
       col=rgb(1,0,0,1),
       ylim=c(0,1200)
       ) ## Draw a new plot
  
  dev.off() ## Close png device
}

this_data<-readPowerThursdayAndFriday()
printHistogram(this_data$Global_active_power)
