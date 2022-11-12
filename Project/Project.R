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

#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

#get txt file data
table <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")

################################################################################
## PART 1: PCA ANALYSIS AND SELECTION

# scale all numeric columns using standardization
scaledTable <- table 
for(i in 1:ncol(scaledTable)){
  cat(i, "\n")
  if (is.numeric(scaledTable[,i])){
    scaledTable[,i] <- scale(scaledTable[,i])
  }
}

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
trendStart = 7001
numPoints = 200
# Take only values which are important to PC1 and PC2. 
# PCA 1 == Global Intensity = 6, and GlobalActive = 3
# PCA 2 == SubMeter 3 = 9 and GlobalReactive = 4
postPCATable <- scaledTable[,c(1, 2, 3, 4, 6, 9)]
################################################################################
## PART 2: TRAINING AND TESTING MULTIVAR HMM

trendStart = 7001
postPCATable[trendStart,]
numPoints = 200
postPCATable[trendStart + numPoints,]
n <- 10080
nr <- nrow(postPCATable)
weeks <- split(postPCATable, rep(1:ceiling(nr/n), each=n, length.out=nr))
weeks <- weeks[- 53]

HMMTrain <- list()
HMMTest <- list()
for (week in 1:39){
  weekData <- weeks[[week]]
  trend <-weekData[trendStart:(trendStart + numPoints),]
  trend <- trend[ -c(1,2) ]
  trend$weekID = week
  typeof(trend)
  HMMTrain <- append(HMMTrain, list(trend))
}
for (week in 40:52){
  weekData <- weeks[[week]]
  trend <-weekData[trendStart:(trendStart + numPoints),]
  trend <- trend[ -c(1,2) ]
  trend$weekID = week
  typeof(trend)
  HMMTest <- append(HMMTest, list(trend))
}

trainingData <- do.call(rbind, HMMTrain)
trainingData = trainingData[ -c(8)]
times <- rep(numPoints+1, 39)


bicList = list()
llList = list()
set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 4, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
  set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 8, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
  
  set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 10, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
  set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 11, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
  set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 13, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
  set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 14, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
  set.seed(1)
  model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = 16, ntimes = times)
  fitModel <- fit(model,emcontrol=em.control(classification="hard"))
  bic <- BIC(fitModel)
  bicList <- append(bicList, bic)
  l <- logLik(fitModel)
  llList <- append(llList, l)
  
# for (num in seq(4,16,4)){
#   set.seed(1)
#   model <- depmix(response =list(Global_intensity ~ 1,Global_active_power ~ 1, Global_reactive_power ~ 1, Sub_metering_3 ~ 1),family=list(gaussian(), gaussian(), gaussian(), gaussian()), data = trainingData, nstates = num, ntimes = times)
# 
#   fitModel <- fit(model,emcontrol=em.control(classification="hard"))
# 
# 
# bic <- BIC(fitModel)
# bicList <- append(bicList, bic)
# 
# l <- logLik(fitModel)
# llList <- append(llList, l)
# }

df <- data.frame(unlist(bicList),unlist(llList))
names(df) = c("BIC","ll")
#make log values negative (change later)

print (df)

###############################################################################
GraphPlot <- plot(x = c(4,8,10,11,14,15), y = df$BIC, 
                  main = "BIC/LogLike Graph", xlab = "NStates", 
                  ylab = "BIC/LogLik Scoring",
                  ylim = c(-17000, 35000), type = "l", lty = 1, 
                  lwd= 0.5, col = "red")
points(x = c(4,8,10,11,14,15), y = df$ll, type = "l", lty = 1, lwd=0.5, col = "blue")
abline(h = 0,lty="dashed")
legend("topright", legend=c("LogLik", "BIC"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)
################################################################################
## PART 3: ANOMOLY DETECTION