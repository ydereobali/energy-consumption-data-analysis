########################################################################
# Title: Analysis and Visualisation of Energy Metering Data
# Author: Yigit Dereobali
# Title: ubiqum module 3 task 1 github
# Description: Analyse energy metering data to develop business ideas
# Date: 13.06.2019
# Version: 1.3
########################################################################


# LIBRARIES ####
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(data.table)
library(plotly)
library(stats)
library(RMySQL)
library(lubridate)


# IMPORT and PROCCESS DATA ####
rawdata <- fread("household_power_consumption.txt")
mydata <- rawdata


# delete unnecessary columns
mydata <- subset(mydata, select = -c(Global_reactive_power, Voltage, Global_intensity))


# check incomplete data
colnames(mydata)[colSums(is.na(mydata)) > 0]
missing.rows <- data.table(meter3.missingdates = mydata$Date[rowSums(is.na(mydata)) > 0])


# omit incomplete data
mydata <- na.omit(mydata)


# chage data types
mydata$Global_active_power <- as.numeric(mydata$Global_active_power)
mydata$Sub_metering_1 <- as.numeric(mydata$Sub_metering_1)
mydata$Sub_metering_2 <- as.numeric(mydata$Sub_metering_2)
mydata$Date <- as.Date(mydata$Date, format = "%d/%m/%Y")


# change global active power to watthour
mydata$Global_active_power <- mydata$Global_active_power * 1000 / 60


# calculate consumption other than submeters 1, 2 and 3
mydata <- mydata %>% 
  mutate(Others = Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)


# Combine Date and Time in a new attribute column
mydata <- mydata %>% 
  mutate(DateTime = paste(Date, Time))
mydata <- subset(mydata, select = -c(Date, Time))


# change date time data type
mydata$DateTime <- as.POSIXct(mydata$DateTime, "%Y/%m/%d %H:%M:%S")
attr(mydata$DateTime, "tzone") <- "GMT"


# create, factorize and order year
mydata$year <- year(mydata$DateTime)
mydata$year <- factor(mydata$year, ordered = TRUE)
# create, factorize and order hour
mydata$hour <- hour(mydata$DateTime)
mydata$hour <- factor(mydata$hour, ordered = TRUE)
#create, factorize and order month
mydata$month <- month(mydata$DateTime)
mydata$month <- factor(mydata$month, ordered = TRUE)
# create, factorize and order weekdays
mydata$weekday <- weekdays(mydata$DateTime) 
mydata$weekday <- ordered(
  mydata$weekday,
  levels = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  )
)


# YEARLY CONSUMPTION DATA ####
years.consumption2 <- mydata %>% 
  group_by(year) %>% 
  summarise(meter1.yearly = sum(Sub_metering_1),
            meter2.yearly = sum(Sub_metering_2),
            meter3.yearly = sum(Sub_metering_3),
            others.yearly = sum(Others),
            total.yearly = sum(Global_active_power))

years.consumption2$years <- years.consumption2$year 


# MONTHLY CONSUMPTION DATA ####
monthly.consumption2 <- mydata %>% 
  group_by(month) %>% 
  summarise(meter1.monthly = sum(Sub_metering_1),
            meter2.monthly = sum(Sub_metering_2),
            meter3.monthly = sum(Sub_metering_3),
            others.monthly = sum(Others),
            total.monthly= sum(Global_active_power))
monthly.consumption2$month <- as.factor(monthly.consumption2$month)


# WEEKDAYS CONSUMPTION DATA ####
weekday.consumption2 <- mydata %>% 
  group_by(weekday) %>% 
  summarise(meter1.weekday = sum(Sub_metering_1),
            meter2.weekday = sum(Sub_metering_2),
            meter3.weekday = sum(Sub_metering_3),
            others.weekday = sum(Others),
            total.weekday= sum(Global_active_power))


# HOURLY CONSUMPTION DATA ####
hourly.consumption2 <- mydata %>% 
  group_by(hour) %>% 
  summarise(meter1.hourly = sum(Sub_metering_1),
            meter2.hourly = sum(Sub_metering_2),
            meter3.hourly = sum(Sub_metering_3),
            others.hourly = sum(Others),
            total.hourly= sum(Global_active_power))


# GGPLOT YEARLY ####
g32 <- ggplot(data = years.consumption2, aes(x = years, y = years.consumption)) +
  geom_line( aes(x = years, y = years.consumption2$total.yearly, group = 1, color = "Total")) +
  geom_line( aes(x = years, y = years.consumption2$others.yearly, group = 1, color = "Others")) +
  geom_line( aes(x = years, y = years.consumption2$meter3.yearly, group = 1, color = "SubMeter3")) +
  geom_line( aes(x = years, y = years.consumption2$meter1.yearly, group = 1, color = "SubMeter1")) +
  geom_line( aes(x = years, y = years.consumption2$meter2.yearly, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("years") +
  ggtitle("yearly power consumption") +
  scale_y_continuous(labels = scales::comma)

g32
ggplotly(g32)


# GGPLOT MONTHLY ####
g12 <- ggplot(data = monthly.consumption2) +
  geom_line( aes(x = month, y = monthly.consumption2$total.monthly, group = 1,  color = "Total")) +
  geom_line( aes(x = month, y = monthly.consumption2$others.monthly, group = 1, color = "Others")) +
  geom_line( aes(x = month, y = monthly.consumption2$meter3.monthly, group = 1, color = "SubMeter3")) +
  geom_line( aes(x = month, y = monthly.consumption2$meter1.monthly, group = 1, color = "SubMeter1")) +
  geom_line( aes(x = month, y = monthly.consumption2$meter2.monthly, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("month") +
  ggtitle("monthly power consumption") +
  scale_y_continuous(labels = scales::comma)

g12
ggplotly(g12)


# GGPLOT WEEKDAYS ####
g02 <- ggplot(data = weekday.consumption2) +
  geom_line( aes(x = weekday, y = weekday.consumption2$total.weekday, group = 1, color = "Total")) +
  geom_line( aes(x = weekday, y = weekday.consumption2$others.weekday, group = 1, color = "Others")) +
  geom_line( aes(x = weekday, y = weekday.consumption2$meter3.weekday, group = 1, color = "SubMeter3")) +
  geom_line( aes(x = weekday, y = weekday.consumption2$meter1.weekday, group = 1, color = "SubMeter1")) +
  geom_line( aes(x = weekday, y = weekday.consumption2$meter2.weekday, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("days of the week") +
  ggtitle("weekdays power consumption")  +
  scale_y_continuous(labels = scales::comma)
g02
ggplotly(g02)


# GGPLOT HOURLY ####
g22 <- ggplot(data = hourly.consumption2) +
  geom_line( aes(x = hour, y = hourly.consumption2$total.hourly, group = 1, color = "Total")) +
  geom_line( aes(x = hour, y = hourly.consumption2$others.hourly, group = 1, color = "Others")) +
  geom_line( aes(x = hour, y = hourly.consumption2$meter3.hourly, group = 1, color = "SubMeter3")) +
  geom_line( aes(x = hour, y = hourly.consumption2$meter1.hourly, group = 1, color = "SubMeter1")) +
  geom_line( aes(x = hour, y = hourly.consumption2$meter2.hourly, group = 1, color = "SubMeter2")) +
  ylab("power consumtion in watt hours") +
  xlab("hours") +
  ggtitle("hourly power consumption") +
  scale_y_continuous(labels = scales::comma)
g22
ggplotly(g22)



