#install.packages("lubridate")
library("rstudioapi")

#install.packages("lubridate")
library("lubridate")

#install.packages("psych")
library("psych") 

#install.packages("modeest")
library("modeest") 

#install.packages("ggplot2")
library(ggplot2)

#install.packages("dplyr")
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

# Create row number column to avoid gap in graph data
subWkdayNight_avg <- dplyr::mutate(subWkdayNight_avg, ID = row_number())
subWkendNight_avg <- dplyr::mutate(subWkendNight_avg, ID = row_number())
subWkdayDay_avg <- dplyr::mutate(subWkdayDay_avg, ID = row_number())
subWkendDay_avg <- dplyr::mutate(subWkendDay_avg, ID = row_number())

# Split and organize night DFs in order
df1 <- subWkdayNight_avg[subWkdayNight_avg$ID >= 1 & subWkdayNight_avg$ID <= 121,]
df1 <- df1[order(df1$Group.1 , decreasing = FALSE), ]
df2 <- subWkdayNight_avg[subWkdayNight_avg$ID >= 122 & subWkdayNight_avg$ID <= 301,]
df2 <- df2[order(df2$Group.1 , decreasing = FALSE), ]
subWkdayNight_avg <- rbind(df2, df1)

df1 <- subWkendNight_avg[subWkendNight_avg$ID >= 1 & subWkendNight_avg$ID <= 121,]
df1 <- df1[order(df1$Group.1 , decreasing = FALSE), ]
df2 <- subWkendNight_avg[subWkendNight_avg$ID >= 122 & subWkendNight_avg$ID <= 301,]
df2 <- df2[order(df2$Group.1 , decreasing = FALSE), ]
subWkendNight_avg <- rbind(df2, df1)

# Create row number column to avoid gap in graph data
subWkdayNight_avg <- dplyr::mutate(subWkdayNight_avg, ID = row_number())
subWkendNight_avg <- dplyr::mutate(subWkendNight_avg, ID = row_number())
subWkdayDay_avg <- dplyr::mutate(subWkdayDay_avg, ID = row_number())
subWkendDay_avg <- dplyr::mutate(subWkendDay_avg, ID = row_number())

# Make weekday and weekend day graph
WkEndDayPlot <- plot(x = subWkendDay_avg$ID, y = subWkendDay_avg$x, 
                     main = "Global Intensity Trend (Day)", xlab = "Time", 
                     ylab = "Average Global Intensity (amps)", xaxt='n',
                     ylim = c(0, 25), type = "l", lty = 1, lwd= 0.5, col = "red")

# Add proper labels to x-axis
axis(1, at = seq(round(min(1)),round(max(241)), by = 60), 
     labels = c("10:00", "11:00", "12:00", "13:00", "14:00"))
seq(round(min(1)),round(max(301)), by = 60)

points(subWkdayDay_avg$ID, y = subWkdayDay_avg$x, type = "l", lty = 1, lwd=0.5, col = "blue")
legend("topleft", legend=c("Weekday", "Weekend"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)
# Add legend
legend("topright", legend=c("Linear Reg Weekday Line", "Linear Reg Weekend Line",
                            "Poly Reg Weekday Line", "Poly Reg Weekend Line"),
       col=c("blue", "red", col=4, col="maroon"), cex=0.6,title="Regression Legend", 
       text.font=4, lty = c(1,1,2,2), lwd = 2.0:2.0)

abline(lm(subWkdayDay_avg$x ~ subWkdayDay_avg$ID, subWkdayDay_avg), col="blue", lwd = 2.0)
abline(lm(subWkendDay_avg$x ~ subWkendDay_avg$ID, subWkendDay_avg), col= "red", lwd = 2.0)

# Add polynomial line for day
fit_polyWkdayDay <- lm(subWkdayDay_avg$x ~ poly(subWkdayDay_avg$ID, 3,
                                           raw=TRUE), subWkdayDay_avg)
lines(fit_polyWkdayDay$fitted.values, col=4, lty = 2, lwd=2)
fit_polyWkendDay <- lm(subWkendDay_avg$x ~ poly(subWkendDay_avg$ID, 7,
                                           raw=TRUE), subWkendDay_avg)
lines(fit_polyWkendDay$fitted.values, col="maroon", lty = 2, lwd=2)

# Make weekday and weekend night graph
WkEndNightPlot <- plot(x = subWkendNight_avg$ID, y = subWkendNight_avg$x,
                       main = "Global Intensity Trend (Night)", xlab = "Time", 
                       ylab = "Average Global Intensity (amps)", xaxt='n',
                       ylim = c(0, 25), type = "l", lty = 1, lwd= 0.5, col = "red")

# Add proper labels to x-axis
axis(1, at = seq(round(min(1)),round(max(301)), by = 60), 
     labels = c("21:00", "22:00", "23:00", "00:00", "1:00", "2:00"))
seq(round(min(1)),round(max(301)), by = 60)

points(subWkdayNight_avg$ID, y = subWkdayNight_avg$x, type = "l", lty = 1, lwd=0.5, col = "blue")
legend("topleft", legend=c("Weekday", "Weekend"),col=c("blue", "red"), 
       cex=0.6,title="Data Legend", text.font=4, lty = 1:1)
abline(lm(subWkdayNight_avg$x ~ subWkdayNight_avg$ID, subWkdayNight_avg), col="blue", lwd = 2.0)
abline(lm(subWkendNight_avg$x ~ subWkendNight_avg$ID, subWkendNight_avg), col= "red", lwd = 2.0)

# Add polynomial line for night
fit_poly1 <- lm(subWkdayNight_avg$x ~ poly(subWkdayNight_avg$ID, 3,
                                              raw=TRUE), subWkdayNight_avg)
lines(fit_poly1$fitted.values, col=4, lty = 2, lwd=2)
fit_poly2 <- lm(subWkendNight_avg$x ~ poly(subWkendNight_avg$ID, 3,
                                           raw=TRUE), subWkendNight_avg)
lines(fit_poly2$fitted.values, col="maroon", lty = 2, lwd=2)

legend("topright", legend=c("Linear Reg Weekday Line", "Linear Reg Weekend Line",
                            "Poly Reg Weekday Line", "Poly Reg Weekday Line"),
       col=c("blue", "red", col=4, col="maroon"), cex=0.6,title="Regression Legend", 
       text.font=4, lty = c(1,1,2,2), lwd = 2.0:2.0)

