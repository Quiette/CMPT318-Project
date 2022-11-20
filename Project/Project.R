library("rstudioapi")
require(dplyr)
require(tidyr)
library(forecast)
library(zoo)
library(quantmod)
library(plotly)
#install.packages("depmixS4")
library(depmixS4)
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#install.packages("gginnards")
library(gginnards)
library(ggforce)

#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

#get txt file data
table <- read.table("TermProjectData.txt", header = TRUE, sep = ",")

################################################################################
## PART 1: PCA ANALYSIS AND SELECTION

# scale all numeric columns using standardization
scaledTable <- table 
for(i in 1:ncol(scaledTable)){
  cat(i, "\n")
  if (is.numeric(table[,i])){
    scaledTable[,i] <- scale(table[,i])
  }
}

# Data ranges for Saturday, December 16, 2006 to Monday, January 12, 2009
pca <- prcomp(na.exclude(scaledTable[, -c(1,2)]))

summary(pca)

# Creating simple yet comprehensive biplot. Excluding plotting of points as useless for our needs.
biplot <- ggbiplot(pca)

biplot <- delete_layers(biplot, "GeomPoint")

# https://stackoverflow.com/questions/73786208/ggbiplot-how-to-change-the-colour-of-the-arrows-and-text-using-a-function-for
seg <- which(sapply(biplot$layers, function(x) class(x$geom)[1] == 'GeomSegment'))
txt <- which(sapply(biplot$layers, function(x) class(x$geom)[1] == 'GeomText'))

biplot$layers[[seg]]$aes_params$colour <- c('red', 'orange', 'black', 'green', 'blue', 'purple', 'maroon')
biplot$layers[[seg]]$aes_params$size <- 1

biplot$layers[[txt]] <- geom_label(aes(x = xvar, y = yvar, label = varname,
                                       angle = angle, hjust = hjust), 
                                   label.size = NA,
                                   data = biplot$layers[[txt]]$data, 
                                   fill = '#dddddd80')
biplot + theme_minimal() + xlim(-2.5, 0.1)

# Take only values which are important to PC1 and PC2. 
# PCA 1 == Global Intensity = 6, and GlobalActive = 3
# PCA 2 == SubMeter 3 = 9 and GlobalReactive = 4
postPCATable <- scaledTable[,c(1, 2, 3, 4, 6, 9)]
################################################################################
## PART 2: TRAINING AND TESTING MULTIVAR HMM

trendStart = 100
postPCATable[trendStart,]
numPoints = 200
postPCATable[trendStart + numPoints,]
n <- 10080
nr <- nrow(postPCATable)
weeks <- split(postPCATable, rep(1:ceiling(nr/n), each=n, length.out=nr))
weeks <- weeks[- 155]

HMMTrain <- list()
HMMTest <- list()
for (week in 1:115){
  weekData <- weeks[[week]]
  trend <-weekData[trendStart:(trendStart + numPoints),]
  trend <- trend[ -c(1,2) ]
  trend$weekID = week
  typeof(trend)
  HMMTrain <- append(HMMTrain, list(trend))
}
for (week in 116:154){
  weekData <- weeks[[week]]
  trend <-weekData[trendStart:(trendStart + numPoints),]
  trend <- trend[ -c(1,2) ]
  trend$weekID = week
  typeof(trend)
  HMMTest <- append(HMMTest, list(trend))
}

testingData <- do.call(rbind, HMMTest)
testingData = testingData[ -c(8)]
trainingData <- do.call(rbind, HMMTrain)
trainingData = trainingData[ -c(8)]
set.seed(1)
times <- rep(numPoints+1, 115)


bicList = list()
llList = list()
xList = list()


trainedModels = list()
 for (num in 4:14){
    cat(num, ":\n")
    set.seed(1)
    model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list( gaussian(),gaussian(), gaussian(),gaussian()), data = trainingData, nstates = num, ntimes = times)

    fitModel <- fit(model)
    trainedModels[num] <- fitModel

    bic <- BIC(trainedModels[[num]])
    bicList <- append(bicList, bic)

    l <- logLik(trainedModels[[num]])
    llList <- append(llList, l)

    xList <- append(xList, num)
 }

 df <- data.frame(unlist(bicList),unlist(llList), unlist(xList))
 names(df) = c("BIC","ll", "X")
 df$absDist <- abs(df$BIC - df$ll)


testNTimes <- rep(numPoints+1, 39)
set.seed(1)

###############################################################################
GraphPlot <- plot(x = df$X, y = df$BIC, 
                  main = "BIC/LogLike Graph", xlab = "NStates", 
                  ylab = "BIC/LogLik Scoring",
                  ylim = c(min(df$ll), max(df$BIC)), type = "l", lty = 1, 
                  lwd= 0.5, col = "red")
points(x =  df$X, y = df$ll, type = "l", lty = 1, lwd=0.5, col = "blue")
abline(h = 0,lty="dashed")
legend("topright", legend=c("LogLik", "BIC"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)


coeff <- -0.45
ggplot(df, aes(x=df$X)) +
  
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

####
coeff <- -0.45
ggplot(df, aes(x=df$X)) +
  
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
    legend.position = c(0.8, 0.6)
  ) +
  coord_cartesian(xlim = c(11, 14), ylim = c(78000, 85000)) + 
  ggtitle("LogLike/BIC per NStates Between 11 and 14") + labs(x = "Number of States") + 
  scale_color_identity(name = "Scoring Functions",
                     breaks = c("red", "blue"),
                     labels = c("BIC", "Log-Likelihood"),
                     guide = "legend")


#####
testNTimes <- rep(numPoints+1, 39)

testLikelihood <- function(states) {
  model2 <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = testingData, nstates = states, ntimes = testNTimes)
  model2 <- setpars(model2,getpars(trainedModels[[states]]))
  fb <- forwardbackward(model2)
  print(fb$logLike)
  return(fb$logLike)
}

testLL <- list()
for(i in 4:14){
  testLL <- append(testLL, (testLikelihood(i)/ 39))
}

df$testLL <- as.numeric(unlist(testLL))

df$NormLL <- (df$ll / 115)

df$absDistTT <- abs(df$testLL - df$NormLL)

################################################################################
## PART 3: ANOMOLY DETECTION

anomTable1 <- read.table("DataWithAnomalies1.txt", header = TRUE, sep = ",")
anomTable2 <- read.table("DataWithAnomalies2.txt", header = TRUE, sep = ",")
anomTable3 <- read.table("DataWithAnomalies3.txt", header = TRUE, sep = ",")

anomalyDetect <- function(anomTable) {
scaledAnomTable <- anomTable

scaledAnomTable$Global_active_power <- scale(anomTable$Global_active_power, center=1.23, scale=1.06)
scaledAnomTable$Global_reactive_power <- scale(anomTable$Global_reactive_power, center=0.122, scale=0.112)
scaledAnomTable$Global_intensity <- scale(anomTable$Global_intensity, center=4.64, scale=4.58)
scaledAnomTable$Sub_metering_3 <- scale(anomTable$Sub_metering_3, center=6.17, scale=8.32)

postAnomTable <- scaledAnomTable[,c(1, 2, 3, 4, 6, 9)]
optStates = 9
anomTrendStart = 6057

nr2 <- nrow(postAnomTable)
anomWeeks <- split(postAnomTable, rep(1:ceiling(nr2/n), each=n, length.out=nr2))
anomWeeks <- anomWeeks[- 52]

anomHMMTest <- list()

for (week in 1:51){
  anomWeekData <- anomWeeks[[week]]
  anomTrend <- anomWeekData[anomTrendStart:(anomTrendStart + numPoints),]
  anomTrend <- anomTrend[ -c(1,2) ]
  anomTrend$weekID = week
  typeof(anomTrend)
  anomHMMTest <- append(anomHMMTest, list(anomTrend))
}

anomTestingData <- do.call(rbind, anomHMMTest)
anomTestingData = anomTestingData[ -c(8)]

anomTestNTimes <- rep(numPoints+1, length(anomWeeks))

model2 <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = anomTestingData, nstates = optStates, ntimes = anomTestNTimes)
model2 <- setpars(model2,getpars(trainedModels[[optStates]]))
fb <- forwardbackward(model2)
print(fb$logLike)
}

anomalyDetect(anomTable1)
anomalyDetect(anomTable2)
anomalyDetect(anomTable3)
