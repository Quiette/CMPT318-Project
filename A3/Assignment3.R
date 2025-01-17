library("rstudioapi")
require(dplyr)
require(tidyr)
library(forecast)
library(zoo)
library(quantmod)
library(plotly)
<<<<<<< HEAD
=======
#install.packages("depmixS4")
>>>>>>> 688bf69362b30c23cc2e41422c45545898529a82
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

trendStart = 5501 # Start @ 19:40 on Thursdays
numPoints = 200 # End @ 23:00 on Thursdays

table[(trendStart + 100), ]

### JUST CODE FOR GRAPHING TRENDS TO FIND THEM
cl <- rainbow(52)
for (week in as.character(1:52)){
  #  cat(week, "\n")
  weekData <- unscaledWeeks[[week]]$Global_active_power
  trend = weekData[trendStart:(trendStart + numPoints)]
  if (week == "1"){
    plot(trend, type="l", ylim = c(0, 9.5), col = cl[as.numeric(week)])
  } else {
    lines(trend,type="l", col = cl[as.numeric(week)])
  }
}


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

testWithoutWeekID = test[ -c(8)]
times <- rep(numPoints + 1, 52)

names(test)
set.seed(10)
bicList = list()
llList = list()
for (num in 3:16){
  model <- depmix(Global_active_power ~ 1, data = testWithoutWeekID, nstates = num, ntimes = times)
  fitModel <- fit(model)
  
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
}

df <- data.frame(unlist(bicList),unlist(llList))
names(df) = c("BIC","ll")
df$absDist <- abs(df$BIC - df$ll)
df$x = c(3:16)


<<<<<<< HEAD
set.seed(1)
testWithoutWeekID = test[ -c(8)]
times <- rep(181, 52)

bicList = list()
llList = list()
for (num in 3:16){
  model <- depmix(Global_intensity ~ 1, data = testWithoutWeekID, nstates = num, ntimes = times)
  fitModel <- fit(model)
  
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  
  l <- logLik(fitModel)
  llList <- append(llList, l)
}

df <- data.frame(unlist(bicList),unlist(llList))
names(df) = c("BIC","ll")
#make log values negative (change later)
df$ll <- df$ll*(-1)

print (df)
=======
print(df)
>>>>>>> 688bf69362b30c23cc2e41422c45545898529a82
# HMMTrainDF[1,] is 1st row values, HMMTrainDF[,1] is first column values
# Timeframe is from Tuesday 4:19 am to Tuesday 7:19 am

###############################################################################
<<<<<<< HEAD
GraphPlot <- plot(x = c(3:7), y = df$bic, 
                     main = "BIC/LogLike Graph", xlab = "NStates", 
                     ylab = "BIC/LogLik Scoring",
                     ylim = c(-5000, -300000), type = "l", lty = 1, 
                     lwd= 0.5, col = "red")
points(x = c(3:16), y = df$ll, type = "l", lty = 1, lwd=0.5, col = "blue")
legend("topleft", legend=c("LogLik", "BIC"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)
=======
GraphPlot <- plot(x = df$x, y = df$BIC, 
                  main = "BIC/LogLike Graph", xlab = "NStates", 
                  ylab = "BIC/LogLik Scoring",
                  ylim = c(-9000, 18000), type = "l", lty = 1, 
                  lwd= 2, col = "red")
points(x = df$x, y = df$ll, type = "l", lty = 1, lwd=2, col = "blue")
legend("topright", legend=c("LogLik", "BIC"),col=c("blue", "red"), cex=0.6,title="Data Legend", text.font=4, lty = 1:1)
lines(x = df$x, y = rep(0, 14), type = "l", lty = 1, lwd=2, col = "black")

####
coeff <- -0.35
ggplot(df, aes(x=df$x)) +
  
  geom_line(aes(y=df$BIC, color="red"), size=2) + 
  geom_line(aes(y=df$ll/coeff, color="blue"), size=2) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "BIC",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="LogLike")
  ) + 
  
  theme(
    axis.title.y = element_text(color = "red", size=13),
    axis.title.y.right = element_text(color = "blue", size=13),
    legend.position = c(0.8, 0.8)
  ) +
  
  ggtitle("LogLike/BIC per NStates") + labs(x = "Number of States") +
  scale_color_identity(name = "Scoring Functions",
                       breaks = c("red", "blue"),
                       labels = c("BIC", "Log-Likelihood"),
                       guide = "legend")
                                                                       
                                                                                       
>>>>>>> 688bf69362b30c23cc2e41422c45545898529a82
