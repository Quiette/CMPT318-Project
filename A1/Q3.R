#install.packages("lubridate")
library("rstudioapi")

#install.packages("lubridate")
library("lubridate")

#install.packages("psych")
library("psych") 

#install.packages("modeest")
library("modeest") 

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

# Set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

dayTimeStart = "09:00:00"
nightTimeStart = "17:00:00"

subDayStart = "10:00:00"
subDayEnd = "14:00:00"
subNightStart = "21:00:00"
subNightEnd = "02:00:00"

df <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")

df$Date <- as.POSIXct(df$Date, format= "%d/%m/%Y")
start <- as.POSIXct("08/01/2007",format= "%d/%m/%Y")
end <- as.POSIXct("14/01/2007",format= "%d/%m/%Y")
df <- df[df$Date >=  start & df$Date <= end,]
class(df$Date)

# Create weekday column, 1 if weekday, 0 if weekend
# Sun = 1, Mon = 1, T = 3, W = 4, Th = 5 F = 6, Sat = 7
weekdayCol <- wday(df$Date)
weekdayCol[weekdayCol == 1 | weekdayCol == 7 ] <- 0
weekdayCol[weekdayCol != 0] <- 1
df$WeekdayBool <- weekdayCol

PowerTimeDate <- df[,c(1,2,6,10)]

# Create sub DFs for weekday days and weekday nights
PowerTimeWkdayDay <- PowerTimeDate[(PowerTimeDate$Time >= dayTimeStart & 
                                PowerTimeDate$Time <= nightTimeStart) 
                                & PowerTimeDate$WeekdayBool == 1, ]

PowerTimeWkdayNight <- PowerTimeDate[(PowerTimeDate$Time < dayTimeStart | 
                                    PowerTimeDate$Time > nightTimeStart) & 
                                    PowerTimeDate$WeekdayBool == 1, ]

# Create sub DFs for weekend day and weekend night
PowerTimeWkendDay <- PowerTimeDate[(PowerTimeDate$Time >= dayTimeStart & 
                                     PowerTimeDate$Time <= nightTimeStart) 
                                   & PowerTimeDate$WeekdayBool == 0, ]

PowerTimeWkendNight <- PowerTimeDate[(PowerTimeDate$Time < dayTimeStart |
                                        PowerTimeDate$Time > nightTimeStart) & 
                                        PowerTimeDate$WeekdayBool == 0, ]

# Time series for day weekdays & night weekdays
wkdayDay_avg <- aggregate(PowerTimeWkdayDay$Global_intensity, 
                          list(PowerTimeWkdayDay$Time), mean)
wkdayNight_avg <- aggregate(PowerTimeWkdayNight$Global_intensity, 
                            list(PowerTimeWkdayNight$Time), mean)

# Time series for day weekend & night weekend
wkendDay_avg <- aggregate(PowerTimeWkendDay$Global_intensity, 
                          list(PowerTimeWkendDay$Time), mean)
wkendNight_avg <- aggregate(PowerTimeWkendNight$Global_intensity, 
          list(PowerTimeWkendNight$Time), mean)

wkendNight_avg$Time = as.POSIXct(wkendNight_avg$Group.1, format = "%H:%M:%S")
wkendDay_avg$Time = as.POSIXct(wkendDay_avg$Group.1, format = "%H:%M:%S")
wkdayNight_avg$Time = as.POSIXct(wkdayNight_avg$Group.1, format = "%H:%M:%S")
wkdayDay_avg$Time = as.POSIXct(wkdayDay_avg$Group.1, format = "%H:%M:%S")

# Extract sub-time intervals for graph (10am - 2pm day), (9pm - 2am for night)
subWkdayDay_avg <- wkdayDay_avg[wkdayDay_avg$Group.1 >= subDayStart 
                                  & wkdayDay_avg$Group.1 <= subDayEnd, ]
subWkendDay_avg <- wkendDay_avg[wkendDay_avg$Group.1 >= subDayStart
                                & wkendDay_avg$Group.1 <= subDayEnd, ]
subWkendNight_avg <- wkendNight_avg[wkendNight_avg$Group.1 >= subNightStart 
                                    | wkendNight_avg$Group.1 <= subNightEnd, ]
subWkdayNight_avg <- wkdayNight_avg[wkdayNight_avg$Group.1 >= subNightStart 
                                    | wkdayNight_avg$Group.1 <= subNightEnd, ]

# Make weekday and weekend day graph
WkEndDayPlot <- plot(x = subWkendDay_avg$Time, y = subWkendDay_avg$x, type = "l",
                     lty = 1, lwd=0.5, col = "red")
points(subWkdayDay_avg$Time, y = subWkdayDay_avg$x, type = "l", lty = 5, lwd=0.5, col = "blue")
legend("top", legend=c("Weekday", "Weekend"),col=c("blue", "red"), 
       cex=0.8,title="Data Legend", text.font=4, lty = 1:1)
abline(lm(subWkdayDay_avg$x ~ subWkdayDay_avg$Time, subWkdayDay_avg), col="blue", lwd = 2.0)
abline(lm(subWkendDay_avg$x ~ subWkendDay_avg$Time, subWkendDay_avg), col= "red", lwd = 2.0)
legend("topright", legend=c("Weekday Line", "Weekend Line"),
       col=c("blue", "red"), cex=0.8,title="Data Legend", text.font=4, lty = 1:1, lwd = 2.0:2.0)

# Create row number column to avoid gap in graph data
subWkdayNight_avg <- dplyr::mutate(subWkdayNight_avg, ID = row_number())
subWkendNight_avg <- dplyr::mutate(subWkendNight_avg, ID = row_number())

# Set DFs in descending order for graph
# wkdayNight_avg <- wkdayNight_avg[ order(wkdayNight_avg$Time , decreasing = TRUE), ]
# wkendNight_avg <- wkendNight_avg[ order(wkendNight_avg$Time , decreasing = TRUE), ]

# Make weekday and weekend night graph
WkEndNightPlot <- plot(x = subWkendNight_avg$ID, y = subWkendNight_avg$x, ylim = c(0, 25),
                       type = "l", lty = 1, lwd= 0.5, col = "red")
points(subWkdayNight_avg$ID, y = subWkdayNight_avg$x, type = "l", lty = 5, lwd=0.5, col = "blue")
legend("topleft", legend=c("Weekday", "weekend"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)
abline(lm(subWkdayNight_avg$x ~ subWkdayNight_avg$ID, subWkdayNight_avg), col="blue", lwd = 2.0)
abline(lm(subWkendNight_avg$x ~ subWkendNight_avg$ID, subWkendNight_avg), col= "red", lwd = 2.0)
legend("topright", legend=c("Weekday Line", "Weekend Line"),
       col=c("blue", "red"), cex=0.6,title="Data Legend", text.font=4, lty = 1:1, lwd = 2.0:2.0)



