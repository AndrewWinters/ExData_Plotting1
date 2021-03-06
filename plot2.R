#This code recreates Plot 2 from the Exploratory Data Analysis Week 1 assignment. 

####LOAD THE DATA
#(data tables used for faster processing)

setwd("../Rcourse/ExploratoryAnalysis/")
require(data.table)
#read an initial sample to determine column classes
initial <- read.table("./data/exdata-data-household_power_consumption/household_power_consumption.txt", 
                      header=TRUE, sep=";", nrows=50)
classes <- sapply(initial, class)

#read full dataset (warning suppression is needed because fread does not handle custom 
#NAs correctly and throws up warnings about conversion of some numeric columns to string)
suppressWarnings({
    powerAll<-fread("./data/exdata-data-household_power_consumption/household_power_consumption.txt", 
                  header=TRUE, sep=";", na.strings=c("NA","N/A","","?"), colClasses=classes)
})
#convert date variable into Date class and select only the two days required for charts
powerAll[, Date:=(as.Date(Date, format="%d/%m/%Y"))]
power<-powerAll[(powerAll$Date==as.Date("2007-02-01")|powerAll$Date==as.Date("2007-02-02")), ]          
powerAll<-NULL

#correct classes of variables containings NAs back to numeric
power <- power[, lapply(.SD, as.numeric), by="Date,Time"]

#create a full timestamp variable from Date and Time
power <- data.frame(power)
power$fulldate <- strptime(paste(as.character(power$Date),power$Time, sep=" "), "%Y-%m-%d %H:%M:%S")                             

#CREATE THE PLOT

png(file="plot2.png", width=480, height=480)
with(power, plot(fulldate, Global_active_power, type="l", 
                 xlab="", ylab="Global Active Power(kilowatts)"))
dev.off()