#install.packages("lubridate")
library("rstudioapi")

#install.packages("lubridate")
library("lubridate")

#install.packages("psych")
library("psych") 

#install.packages("modeest")
library("modeest") 


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

View(wkdayDay_avg)
View(wkdayNight_avg)
View(wkendDay_avg)
View(wkendNight_avg)
