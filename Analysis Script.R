library(data.table)
library(lubridate)
library(ggplot2)
library(scales)

# Make local clone of repo working directory
getwd()

# Unzip data and load data using quick "fread" function from data.table
unzip("activity.zip")
d <- fread('activity.csv')

# Apply transformations to data. ---------------------------------------------------
# Main objective is to be able to extract specific time frames from data (e.g. days)

# data.table uses specific functions to create IDate type objects which can then be manipulated
# using other data-friendly functionality.
d[,date := as.IDate(date)]

# Create "weekday" variable as factor based on date.
d[,weekday := wday(date,label = T)]

# Create "yearday" variable based on date. This number will correspond to day number in year
# (e.g. January 1st = 1, December 31st = 365).
d[,yearday := yday(date)]


# Create a "time" variable based on the intervals (format = "HH:MM").
intervalmins <- substr(d$interval,nchar(d$interval) - 1,nchar(d$interval))
intervalmins[nchar(intervalmins) == 1] <- paste('0',intervalmins[nchar(intervalmins) == 1], sep = '')
intervalhrs <- substr(d$interval,nchar(d$interval) - 3,nchar(d$interval) - 2)
intervalhrs[intervalhrs == ''] <- '00'
d[, time := as.ITime(paste(intervalhrs, intervalmins, sep = ':'), format = "%H:%M")]


# Analysis ---------------------------------------------------------------------------

# Analysis to answer question 1 in assignment ==================

# Calculate total number, mean, and median of steps per day.
dt1 <- d[!is.na(steps), list(sum(steps, na.rm = T),
                             round(mean(steps, na.rm = T),1),
                             quantile(steps, .50, na.rm = TRUE)),
         by = 'date']
setnames(dt1, old = c('V1','V2','V3'), new = c('Steps', 'Mean', 'Median'))

# Plot histogram of Total steps per day
hist1 <- ggplot(dt1, aes(x = Steps)) +
  geom_histogram()
hist1
# If we apply a log10 transformation the 2 outlying observations are easy to find,
# even with a histogram
hist1 + scale_x_log10()

dt1[, c(1,3,4), with = F]

# Analysis to answer question 2 in assignment ==================

# Calculate average number of steps taken across all days by interval
dt2 <- d[!is.na(steps), list(round(mean(steps, na.rm = T),1)),by = 'time']
setnames(dt2, old = c('time','V1'), new = c('Interval', 'Steps'))

# Identify 5-min interval with highest average number of steps taken across all days.
maxInterval <- dt2$Interval[match(max(dt2$Steps),dt2$Steps)]

# Plot time series of average steps taken across days (y-axis) by interval (x-axis)
dt2[, Interval := as.POSIXct(strftime(Interval, format="%H:%M"), format="%H:%M")]
lines2 <- ggplot(dt2, aes(x = Interval, y = Steps, group=1)) +
  geom_line() +
  scale_x_datetime(labels = date_format("%H:%M")) +
  geom_vline(xintercept = as.numeric(dt2$Interval[104]))
lines2



