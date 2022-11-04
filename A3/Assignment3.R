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

# remove incomplete week of week 53
weeks <- weeks[- 53]

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

# Add data into training dataframe/set
HMMTrain <- list()
for (week in as.character(1:52)){
  weekData <- weeks[[week]]
  trend = weekData[trendStart:(trendStart + 180)]
  typeof(trend)
  HMMTrain <- append(HMMTrain, list(trend))
}

# Row = week of data, column = number
HMMTrainDF <- do.call(rbind, HMMTrain)  
typeof(HMMTrainDF)

# HMMTrainDF[1,] is 1st row values, HMMTrainDF[,1] is first column values

