library("rstudioapi")
require(dplyr)
require(tidyr)
library(forecast)
library(zoo)
library(quantmod)
library(plotly)
library(depmixS4)
#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

#get txt file data
table <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")

# scale all numeric columns
#scaledTable <- table %>% mutate(across(where(is.numeric), scale))

scaledTable <- table 
for(i in 1:ncol(scaledTable)){
  cat(i, "\n")
  if (is.numeric(scaledTable[,i])){
    scaledTable[,i] <- scale(scaledTable[,i])
  }
}


identical(table,scaledTable)

# Split data into weeks. Each row index = 1 minute
n <- 10080
nr <- nrow(table)
weeks <- split(scaledTable$Voltage, rep(1:ceiling(nr/n), each=n, length.out=nr))
scaledWeeks <- split(scaledTable, rep(1:ceiling(nr/n), each=n, length.out=nr))

unscaledWeeks <- split(table, rep(1:ceiling(nr/n), each=n, length.out=nr))
# remove incomplete week of week 53
weeks <- weeks[- 53]
scaledWeeks <- scaledWeeks [- 53]

# Possible starts: 1300, 1700
trendStart = 9000

### JUST CODE FOR GRAPHING TRENDS TO FIND THEM
cl <- rainbow(52)
#for (week in as.character(1:52)){
#  cat(week, "\n")
#  weekData <- weeks[[week]]

# Remove 2880 last entries, weekend days
#  weekData <- head(weekData, -2880)
#  trend = weekData[trendStart:(trendStart + 180)]
#  if (week == "1"){
#    plot(trend, type="l", ylim = c(-1, 5), col = cl[as.numeric(week)])
#  } else {
#    lines(trend,type="l", col = cl[as.numeric(week)])
#  }
#}
numPoints = 200
HMMTrainTest <- list()
for (week in 1:52){
  weekData <- scaledWeeks[[week]]
  trend <-weekData[trendStart:(trendStart + numPoints),]
  trend <- trend[ -c(1,2) ]
  trend$weekID = week
  typeof(trend)
  HMMTrainTest <- append(HMMTrainTest, list(trend))
}

test <- do.call(rbind, HMMTrainTest)
identical(table$Voltage ,scaledTable$Voltage)
# Add data into training dataframe/set
HMMTrain <- list()
for (week in as.character(1:52)){
  weekData <- weeks[[week]]
  trend = weekData[trendStart:(trendStart + 180)]
  typeof(trend)
  HMMTrain <- append(HMMTrain, list(trend))
}

df <- data.frame(unlist(bicList),unlist(llList))
names(df) = c("BIC","ll")
df$absDist <- abs(df$BIC - df$ll)

set.seed(1)
testWithoutWeekID = test[ -c(8)]
times <- rep(201, 52)

bicList = list()
llList = list()
for (num in 3:16){
  model <- depmix(response =Voltage ~ 1, data = testWithoutWeekID, nstates = num, ntimes = times)
  fitModel <- fit(model)
  
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  
  l <- logLik(fitModel)
  llList <- append(llList, l)
}

df <- data.frame(unlist(bicList),unlist(llList))
names(df) = c("BIC","ll")
#make log values negative (change later)

print (df)
# HMMTrainDF[1,] is 1st row values, HMMTrainDF[,1] is first column values
# Timeframe is from Tuesday 4:19 am to Tuesday 7:19 am

###############################################################################
GraphPlot <- plot(x = c(3:16), y = df$BIC, 
                  main = "BIC/LogLike Graph", xlab = "NStates", 
                  ylab = "BIC/LogLik Scoring",
                  ylim = c(-10000, 15000), type = "l", lty = 1, 
                  lwd= 0.5, col = "red")
points(x = c(3:16), y = df$ll, type = "l", lty = 1, lwd=0.5, col = "blue")
abline(h = 0,lty="dashed")
legend("topleft", legend=c("LogLik", "BIC"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)

