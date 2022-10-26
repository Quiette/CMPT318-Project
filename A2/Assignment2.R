library("rstudioapi")
require(dplyr)
require(tidyr)
library(forecast)
library(zoo)


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
smoothenedWeeks <- list()
for (i in weeks){
 q <- list(rollmean(i,7,centre = TRUE))
 smoothenedWeeks <- append(smoothenedWeeks, q)
}
#this week is incomplete so we can ignore it
smoothenedWeeks[53] <- NULL
sum <- 0
averageSmoothenedWeek <- list()

# each row is a week, each column is minute i
Testdf <- as.data.frame(do.call("rbind", smoothenedWeeks))
ncol(Testdf)
nrow(Testdf)

# average by each col to get avg per min
TestResults <- colMeans(Testdf, na.rm = TRUE)
TestResults[10]

mean(Testdf[,10], na.rm = TRUE)
#calculating average smoothened week
for (i in 4:10077){
  temp <- list()
  for (j in 1:52){
    c <- smoothenedWeeks[j]
    d <- as.data.frame(c)
    e <- d[,1]
    temp <- append(temp, e[i])
  }
  sum <- sum(unlist(temp), na.rm = TRUE)
  sum <- sum / 52
  #print(sum)
  averageSmoothenedWeek <- append(averageSmoothenedWeek, sum)
  sum <- 0
}
