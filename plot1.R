#This code recreates Plot 1 from the Exploratory Data Analysis Week 1 assignment. 

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


#CREATE THE PLOT

png(file="plot1.png", width=480, height=480)
hist(power$Global_active_power, col="red", 
     xlab="Global Active Power(kilowatts)", main="Global Active Power")
dev.off()