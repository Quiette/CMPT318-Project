library("rstudioapi")

setwd(dirname(getActiveDocumentContext()$path)) 
getwd()
table <- read.table("Group_Assignment_2_Dataset.txt", header = TRUE, sep = ",")
