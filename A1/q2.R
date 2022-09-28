library("rstudioapi")

setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

# Create table information
table <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
table$Date <- as.POSIXct(table$Date, format= "%d/%m/%Y")

# EXTRACT 2ND WEEKS OF INFO
start <- as.POSIXct("08/01/2007",format= "%d/%m/%Y")
end <- as.POSIXct("15/01/2007",format= "%d/%m/%Y")
table <- table[table$Date >=  start & table$Date <= end,]
table <- subset(table, select = -c(Date,Time))
res <- cor(table, use = "complete.obs")