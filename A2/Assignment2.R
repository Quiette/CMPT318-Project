library("rstudioapi")
require(dplyr)
require(tidyr)


#set working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

#get txt file data
table <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
#isolate global intensity
Global_Intensity <- table$Global_intensity

#10080 minutes in a week so group every 10080 minutes to form the week groups.
n <- 10080
nr <- nrow(table)
weeks <- split(Global_Intensity, rep(1:ceiling(nr/n), each=n, length.out=nr))

#this is how u index a minute in the week u want
print(weeks$`1`[1])