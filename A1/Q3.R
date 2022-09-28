library("rstudioapi")

setwd("~/CMPT318-Project/A1")
getwd()

D <- read.csv("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",", dec = ".")

View(D)
