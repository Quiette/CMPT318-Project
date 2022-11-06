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
scaledTable <- table %>% mutate(across(where(is.numeric), scale))

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
biplot + theme_minimal() + xlim(-2, 0.5)


# Take only values which are important to PC1 and PC2. 
postPCATable <- scaledTable[,c(1, 2, 6, 8, 9)]

################################################################################
## PART 2: TRAINING AND TESTING MULTIVAR HMM

################################################################################
## PART 3: ANOMOLY DETECTION