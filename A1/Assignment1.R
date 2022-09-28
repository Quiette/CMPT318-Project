install.packages("lubridate")
library("rstudioapi")

install.packages("lubridate")
library("lubridate")
# Set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

# Create table information
table <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
table$Date <- as.POSIXct(table$Date, format= "%d/%m/%Y")

# EXTRACT 2ND WEEKS OF INFO TODO
start <- as.POSIXct("07/01/2007",format= "%d/%m/%Y")
end <- as.POSIXct("14/01/2007",format= "%d/%m/%Y")
table <- table[table$Date >=  start & table$Date <= end,]

#############################################
# 1 - done by Nicole
# Create weekday column, 1 if weekday, 0 if weekend
# Sun = 1, Mon = 1, T = 3, W = 4, Th = 5 F = 6, Sat = 7
weekdayCol <- wday(table$Date)
weekdayCol[weekdayCol == 1 | weekdayCol == 7 ] <- 0
weekdayCol[weekdayCol != 0] <- 1
table$WeekdayBool <- weekdayCol

for (i in 1:3){
  print(colnames(table)[i])
  print(tail(table[,i]))
  cat("\n")
}


#############################################
# 2 - done by Kirby

#############################################
# 3 - done by Gavin