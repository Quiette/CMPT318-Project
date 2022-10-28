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
# each row is a week, each column is minute i
Testdf <- as.data.frame(do.call("rbind", smoothenedWeeks))


RowMinColWeek <- do.call(rbind.data.frame, Testdf)
AvgSmoothedWeek <- rowMeans(RowMinColWeek, na.rm = TRUE)

typeof(RowMinColWeek[1,])
RowMinColWeek[10,]
mean(unlist(RowMinColWeek[1,]), na.rm = TRUE)

length(unlist(RowMinColWeek[,1]))
MSES <- list()

# unlist(RowMinColWeek[,i] is all 10074 datapoint for week i
# unlist(RowMinColWeek[j, ] is all weekly datapoint for minute j
for(i in 1:52){
  MSE <- mean((unlist(RowMinColWeek[,i]) - AvgSmoothedWeek)^2, na.rm = TRUE)
  cat("MSE for Week ", i, " - ")
  cat(MSE)
  cat("\n")
  MSES <- append(MSES,MSE)
}
MSEtable <- do.call(rbind.data.frame, Map('c', c(1:52), MSES))
colnames(MSEtable)[1] <- "Week"
colnames(MSEtable)[2] <- "MSE"
print(smoothenedWeeks[1])

plot(AvgSmoothedWeek,type="l",lwd=2,xaxt='n',xlab="Day of the week", ylab = "Global_Intensity (amps)",main="Least anomalous week vs average smoothened week",col="blue",ylim=c(0,20))
lines(unlist(smoothenedWeeks[30]),type="l",col="red")
legend(500,20,title="Legend",text.font=3,legend=c("Average smoothened week","week 30"),col=c("blue","red"),lty=1:1, cex=0.8)
axis(1, at = seq(round(min(1)),round(max(10080)), by = 1440), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


plot(AvgSmoothedWeek,type="l",lwd=2,xaxt='n',xlab="Day of the week", ylab = "Global_Intensity (amps)",main="Most anomalous week vs average smoothened week",col="blue",ylim=c(0,40))
lines(unlist(smoothenedWeeks[52]),type="l",col="red")
legend(500,35,title="Legend",text.font=3,legend=c("Average smoothened week","week 52"),col=c("blue","red"),lty=1:1, cex=0.8)
axis(1, at = seq(round(min(1)),round(max(10080)), by = 1440), labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
seq(round(min(1)),round(max(301)), by = 60)
#calculating average smoothened week
view(dff)
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
