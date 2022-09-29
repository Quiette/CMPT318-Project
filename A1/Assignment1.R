#install.packages("lubridate")
library("rstudioapi")

#install.packages("lubridate")
library("lubridate")

#install.packages("psych")
library("psych") 


#install.packages("Hmisc")
library("Hmisc")

#install.packages("corrplot")
library("corrplot")
#install.packages("modeest")
library("modeest") 

# Set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

# Create table information
table <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
table$Date <- as.POSIXct(table$Date, format= "%d/%m/%Y")

# EXTRACT 2ND WEEKS OF INFO
start <- as.POSIXct("08/01/2007",format= "%d/%m/%Y")
end <- as.POSIXct("14/01/2007",format= "%d/%m/%Y")
table <- table[table$Date >=  start & table$Date <= end,]

dayTimeStart = "09:00:00" # is == nightTimeEnd, day = [9am,5pm]
nightTimeStart = "17:00:00" # is == dayTimeEnd, night = (5pm,9am)

#############################################
# 1 - done by Nicole
# Create weekday column, 1 if weekday, 0 if weekend
# Sun = 1, Mon = 1, T = 3, W = 4, Th = 5 F = 6, Sat = 7
weekdayCol <- wday(table$Date)
weekdayCol[weekdayCol == 1 | weekdayCol == 7 ] <- 0
weekdayCol[weekdayCol != 0] <- 1
table$WeekdayBool <- weekdayCol

# Means, median, mode and SD results
pwVoltageData <- table[,c(3,4,5)]
sapply(pwVoltageData, function(x) mean(x, na.rm=TRUE))
sapply(pwVoltageData, function(x) geometric.mean(x, na.rm=TRUE))
sapply(pwVoltageData, function(x) median(x, na.rm=TRUE))
sapply(pwVoltageData, mfv)
sapply(pwVoltageData, function(x) sd(x, na.rm=TRUE))

###
PowerTimeDate <- table[,c(2,3,4,10)]

# Create subDFs for day and night
PowerTimeNight <- PowerTimeDate[PowerTimeDate$Time < dayTimeStart | PowerTimeDate$Time > nightTimeStart,]
PowerTimeDay <- PowerTimeDate[PowerTimeDate$Time >= dayTimeStart & PowerTimeDate$Time <= nightTimeStart,]

# Create subDFs for weekday and weekend
WeekSplitNight <- split (PowerTimeNight, PowerTimeNight$WeekdayBool)
WeekSplitDay <- split (PowerTimeDay, PowerTimeDay$WeekdayBool)

# Calc min/max for weekend/day for night
i = 0
for(nTable in WeekSplitNight){
  if (i == 0){
    cat("WEEKEND\n")
    i=1
  }
  else{
    cat("WEEKDAY\n")
    i=0
  }
  nMin <- sapply(nTable[,c(2,3)], function(x) min(x, na.rm=TRUE))
  nMax <- sapply(nTable[,c(2,3)], function(x) max(x, na.rm=TRUE))
  cat("Min for (Active, Reactive) during Night", nMin)
  cat("\nMax for (Active, Reactive) during Night", nMax)
  cat("\n\n") # 1st value is weekdayBool = 0, so weekend
}

# Calc min/max for weekend/day for night
for(dTable in WeekSplitDay){
  if (i == 0){
    cat("WEEKEND\n")
    i=1
  }
  else{
    cat("WEEKDAY\n")
    i=0
  }
  dMin <- sapply(dTable[,c(2,3)], function(x) min(x, na.rm=TRUE))
  dMax <- sapply(dTable[,c(2,3)], function(x) max(x, na.rm=TRUE))
  cat("Min for (Active, Reactive) during Day", dMin)
  cat("\nMax for (Active, Reactive) during Day", dMax)
  cat("\n\n") # 1st value is weekdayBool = 0, so weekend
}


#############################################
# 2 - done by Kirby

table2 <- subset(table, select = -c(Date,Time,WeekdayBool))
res <- corr.test(table2, y = NULL, use = "complete",method="pearson",adjust="holm", 
                 alpha=.01)
corrplot(res$r, type="upper", order="hclust", p.mat = res$P, sig.level = 0.01, addCoef.col = 'black', is.corr = FALSE, insig = "label_sig")


#############################################
# 3 - done by Gavin