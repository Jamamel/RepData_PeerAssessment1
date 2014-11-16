library(knitr)
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
png(filename = '.\\figure\\q1hist.png')
hist1
dev.off()


# If we apply a log10 transformation the 2 outlying observations are easy to find,
# even with a histogram
png(filename = '.\\figure\\q1histlog.png')
hist1 + scale_x_log10()
dev.off()
dt1[, c(1,3,4), with = F]

# Analysis to answer question 2 in assignment ==================

# Calculate average number of steps taken across all days by interval
dt2 <- d[!is.na(steps), list(round(mean(steps, na.rm = T),1)),by = 'time']
setnames(dt2, old = c('time','V1'), new = c('Interval', 'Steps'))

# Identify 5-min interval with highest average number of steps taken across all days.
maxInterval <- match(max(dt2$Steps),dt2$Steps)
maxIntLab <-  paste(hour(dt2$Interval[maxInterval]),':',minute(dt2$Interval[maxInterval]),' - ',dt2$Steps[maxInterval],' steps',sep = '')

# Plot time series of average steps taken across days (y-axis) by interval (x-axis)
dt2[, Interval := as.POSIXct(strftime(Interval, format="%H:%M"), format="%H:%M")]
lines2 <- ggplot(dt2, aes(x = Interval, y = Steps, group=1)) +
  geom_line() +
  geom_point(aes(x = dt2$Interval[maxInterval], y = dt2$Steps[maxInterval]),colour = 'red', size = 3) +
  scale_x_datetime(labels = date_format("%H:%M")) +
  annotate("text", label = maxIntLab, x = dt2$Interval[maxInterval], y = dt2$Steps[maxInterval] + 10, size = 5,fontface = "bold")

png(filename = '.\\figure\\q2lines.png')
lines2
dev.off()

# Analysis to answer question 3 in assignment ==================

# Calculate number of observations with missing values (NA) in "steps"
nmissing <- nrow(d[,d[is.na(steps)]])

# To impute said missing values, we calculate the average number of steps taken by weekday & interval
# These will be the values to impute.
auxdt3 <- d[, list(round(mean(steps, na.rm = T),1)),by = c('weekday','time')]
setnames(auxdt3,'V1','Steps')
dt3 <- copy(d)
dt3[,steps := as.numeric(steps)]
setkeyv(auxdt3,c('weekday','time'))
dt3[is.na(steps),steps := auxdt3[dt3[is.na(steps),c('weekday','time'),with = F],'Steps',with = F]]


# Calculate total number, mean, and median of steps per day.
dt3.1 <- dt3[!is.na(steps), list(sum(steps),
                             round(mean(steps),1),
                             quantile(steps, .50,)),
         by = 'date']
setnames(dt3.1, old = c('V1','V2','V3'), new = c('Steps', 'Mean', 'Median'))

# Plot histogram of Total steps per day
hist3 <- ggplot(dt3.1, aes(x = Steps)) +
  geom_histogram()

png(filename = '.\\figure\\q3hist.png')
hist3
dev.off()
# If we apply a log10 transformation the 2 outlying observations are easy to find,
# even with a histogram
png(filename = '.\\figure\\q3histlog.png')
hist3 + scale_x_log10()
dev.off()
dt3.1[, c(1,3,4), with = F]

dt1[,Impute := factor(1,levels = 1:2,labels = c('No','Yes'))]
dt3.1[,Impute := factor(2,levels = 1:2,labels = c('No','Yes'))]

compdt <- rbind(dt1,dt3.1)
comphist <- ggplot(compdt,aes(x = Steps,colour = Impute,fill = Impute)) +
  geom_density(color = NA,alpha = 0.5) +
  xlab("Total Steps") + scale_fill_manual(values=c(No = 'red',Yes = 'green'))
comphist

# Analysis to answer question 4 in assignment ==================

# Create "daytype" factor, identifying weekend vs. week days
dt3[,daytype := factor(ifelse(weekday %in% c('Sun','Sat'),'Weekend','Weekday'))]

# Calculate average number of steps taken across all days by interval and daytype
dt4 <- dt3[!is.na(steps), list(round(mean(steps, na.rm = T),1)),by = c('time','daytype')]
setnames(dt4, old = c('time','V1'), new = c('Interval', 'Steps'))

# Plot time series of average steps taken across days (y-axis) by interval (x-axis), using daytype
# as panel divider.
dt4[, Interval := as.POSIXct(strftime(Interval, format="%H:%M"), format="%H:%M")]
lines4 <- ggplot(dt4, aes(x = Interval, y = Steps, group=1)) +
  geom_line() +
  scale_x_datetime(labels = date_format("%H:%M")) +
  facet_grid(daytype ~ .)

png(filename = '.\\figure\\q4lines.png')
lines4
dev.off()

knit2html('PA1_template.Rmd')
browseURL('PA1_template.html')

