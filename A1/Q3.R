#install.packages("lubridate")
library("rstudioapi")

#install.packages("lubridate")
library("lubridate")

#install.packages("psych")
library("psych") 

#install.packages("modeest")
library("modeest") 

library(ggplot2)

# Set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

dayTimeStart = "09:00:00"
nightTimeStart = "17:00:00"

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

WkEndNightPlot <- plot(x = wkendNight_avg$Time, y = wkendNight_avg$x)
WkdayNightPlot <-plot(x = wkdayNight_avg$Time, y = wkdayNight_avg$x)

WkEndDayPlot <- plot(x = wkendDay_avg$Time, y = wkendDay_avg$x, type = "l", lty = 1, lwd=0.5, col = "red")
points(wkdayDay_avg$Time, y = wkdayDay_avg$x, type = "l", lty = 5, lwd=0.5, col = "blue")
legend("top", legend=c("Weekday", "weekend"),col=c("blue", "red"), cex=0.8,title="Data Legend", text.font=4, lty = 1:1)


abline(lm(wkdayDay_avg$x ~ wkdayDay_avg$Time, wkdayDay_avg), col="blue", lwd = 2.0)
abline(lm(wkendDay_avg$x ~ wkendDay_avg$Time, wkendDay_avg), col= "red", lwd = 2.0)
legend("topright", legend=c("Weekday Line", "weekend Line"),col=c("blue", "red"), cex=0.8,title="Data Legend", text.font=4, lty = 1:1, lwd = 2.0:2.0)



View(wkdayDay_avg)
View(wkdayNight_avg)
View(wkendDay_avg)
View(wkendNight_avg)
