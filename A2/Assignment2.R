library("rstudioapi")
require(dplyr)
require(tidyr)
library(forecast)

#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

#get txt file data
table <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
#isolate global intensity
Global_Intensity <- table$Global_intensity

#10080 minutes in a week so group every 10080 minutes to form the week groups.
n <- 10080
nr <- nrow(table)
weeks <- split(Global_Intensity, rep(1:ceiling(nr/n), each=n, length.out=nr))

#moving average for a week
#v <- ma(weeks$`1`,7,centre = TRUE)
#print(v)

#turn all the datapoints into smoothened datapoints using ma() (moving average)
weeklist <- list()
for (i in weeks){
 q <- ma(i,7,centre = TRUE)
 weeklist <- append(weeklist, q)
}

