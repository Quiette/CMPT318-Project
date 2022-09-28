install.packages("lubridate")
library("rstudioapi")

install.packages("lubridate")
library("lubridate")

install.packages("psych")
library("psych") 

install.packages("modeest")
library("modeest") 

# Set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

# Create table information
table <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
table$Date <- as.POSIXct(table$Date, format= "%d/%m/%Y")

# EXTRACT 2ND WEEKS OF INFO
start <- as.POSIXct("07/01/2007",format= "%d/%m/%Y")
end <- as.POSIXct("14/01/2007",format= "%d/%m/%Y")
table <- table[table$Date >=  start & table$Date <= end,]

dayTimeStart = "09:00:00" # is == nightTimeEnd
nightTimeStart = "17:00:00" # is == dayTimeEnd

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

# Create subDFs for weekday and weekend
PowerTimeWeekDay <- PowerTimeDate[PowerTimeDate$WeekdayBool == 1,]
PowerTimeWeekEnd <- PowerTimeDate[PowerTimeDate$WeekdayBool == 0,]
  # TODO Daytime min weekdays for i = 3 4
  # TODO Daytime min weekends for i = 3 4
  
  # TODO Daytime max weekdays for i = 3 4
  # TODO Daytime max weekends for i = 3 4
  
  # TODO Ntime min weekdays for i = 3 4
  # TODO Ntime min weekends for i = 3 4
  
  # TODO Ntime max weekdays for i = 3 4
  # TODO Ntime max weekends for i = 3 4
  cat("\n")
}


#############################################
# 2 - done by Kirby

#############################################
# 3 - done by Gavin