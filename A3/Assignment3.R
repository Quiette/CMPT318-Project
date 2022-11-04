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
#isolate global intensity
Global_Intensity <- scaledTable$Global_intensity

# Split data into weeks
n <- 10080
nr <- nrow(table)
weeks <- split(Global_Intensity, rep(1:ceiling(nr/n), each=n, length.out=nr))

# remove incomplete week of week 53
weeks <- weeks[- 53]
