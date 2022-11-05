library("rstudioapi")
require(dplyr)
require(tidyr)
library(forecast)
library(zoo)
library(quantmod)
library(plotly)
install.packages("depmixS4")
library(depmixS4)
library(HMM)
#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

#get txt file data
table <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")

# scale all numeric columns
scaledTable <- table %>% mutate(across(where(is.numeric), scale))

scaledTableTwo <- table %>% mutate_if(is.numeric, scale)
#isolate global intensity
Global_Intensity <- scaledTable$Global_intensity
Global_IntensityTwo <- scaledTableTwo$Global_intensity

identical(Global_IntensityTwo,Global_Intensity)

# Split data into weeks. Each row index = 1 minute
n <- 10080
nr <- nrow(table)
weeks <- split(Global_Intensity, rep(1:ceiling(nr/n), each=n, length.out=nr))
scaledWeeks <- split(scaledTable, rep(1:ceiling(nr/n), each=n, length.out=nr))
# remove incomplete week of week 53
weeks <- weeks[- 53]
scaledWeeks <- scaledWeeks [- 53]

# Possible starts: 1300, 1700
trendStart = 1700

### JUST CODE FOR GRAPHING TRENDS TO FIND THEM
#cl <- rainbow(52)
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

HMMTrainTest <- list()
for (week in 1:52){
  weekData <- scaledWeeks[[week]]
  trend <-weekData[trendStart:(trendStart + 180),]
  trend <- trend[ -c(1,2) ]
  trend$weekID = week
  typeof(trend)
  HMMTrainTest <- append(HMMTrainTest, list(trend))
}

test <- do.call(rbind, HMMTrainTest)

# Add data into training dataframe/set
HMMTrain <- list()
for (week in as.character(1:52)){
  weekData <- weeks[[week]]
  trend = weekData[trendStart:(trendStart + 180)]
  typeof(trend)
  HMMTrain <- append(HMMTrain, list(trend))
}


# Row = week of data, column = number
HMMTrainDF <- as.data.frame(do.call(rbind, HMMTrain))
typeof(df)
typeof(as.data.frame(HMMTrain))

set.seed(1)
testWithoutWeekID = test[ -c(8)]
times <- rep(181, 52)
model <- depmix(Global_intensity ~ 1, data = testWithoutWeekID, nstates = 3, ntimes = times)
model2 <- depmix(Global_intensity ~ 1, data = testWithoutWeekID, nstates = 7, ntimes = times)
model3 <- depmix(Global_intensity ~ 1, data = testWithoutWeekID, nstates = 8, ntimes = times)
model4 <- depmix(Global_intensity ~ 1, data = testWithoutWeekID, nstates = 9, ntimes = times)
model5 <- depmix(Global_intensity ~ 1, data = testWithoutWeekID, nstates = 10, ntimes = times)

fitModel <- fit(model)
fitModel2 <- fit(model2)
fitModel3 <- fit(model3)
fitModel4 <- fit(model4)
fitModel5 <- fit(model5)

bic <- BIC(fitModel)
bic2 <- BIC(fitModel2)
bic3 <- BIC(fitModel3)
bic4 <- BIC(fitModel4)
bic5 <- BIC(fitModel5)

l <- logLikHmm(fitModel)
l2 <- logLik(fitModel2)
l3 <- logLik(fitModel3)
l4 <- logLik(fitModel4)
l5 <- logLik(fitModel5)

str(test)
bic <- c(bic,bic2,bic3,bic4,bic5)
ll <- c(l,l2,l3,l4,l5)

df <- data.frame(bic,ll)

print (df)
# HMMTrainDF[1,] is 1st row values, HMMTrainDF[,1] is first column values
# Timeframe is from Tuesday 4:19 am to Tuesday 7:19 am
