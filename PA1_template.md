---
title: 'Reproducible Research: Peer Assessment 1'
author: "Juan Manuel Hernandez"
date: "Sunday, November 16, 2014"
output: html_document
---

```{r setoptions, echo=FALSE, results='hide'}
library(knitr)
library(data.table)
library(lubridate)
library(scales)
library(ggplot2)
opts_chunk$set(echo = TRUE, results = 'hide')
```


## Loading and processing the data

This analysis will use the following libraries:

- [data.table](http://cran.r-project.org/web/packages/data.table/index.html)
- [lubridate](http://cran.r-project.org/web/packages/lubridate/index.html)
- [ggplot2](http://cran.r-project.org/web/packages/ggplot2/index.html)
- [scales](http://cran.r-project.org/web/packages/scales/index.html)

**Make sure the zip folder where you've stored the zip file referenced in the assignment containing the data is set as your working directory.**

```{r readdata}
setwd("C:/Users/Jamamel/Google Drive/John Hopkins Data Science/Reproducible Research/Project 1/RepData_PeerAssessment1")

# Unzip data and load data using quick "fread" function from data.table
unzip("activity.zip")
d <- fread('activity.csv')
```

In order to correctly perform the following analysis, we need to apply a few transformations to our data. Most of these are focused on creating "date/time" type variables which are easy to manipulate later on.

```{r transformdata}
# data.table uses specific functions to create IDate type objects which can then be manipulated
# using other data-friendly functionality.
d[,date := as.IDate(date)]

# Create "weekday" variable as factor based on date.
d[,weekday := wday(date,label = T)]

# Create a "time" variable based on the intervals (format = "HH:MM").
intervalmins <- substr(d$interval,nchar(d$interval) - 1,nchar(d$interval))
intervalmins[nchar(intervalmins) == 1] <- paste('0',intervalmins[nchar(intervalmins) == 1], sep = '')
intervalhrs <- substr(d$interval,nchar(d$interval) - 3,nchar(d$interval) - 2)
intervalhrs[intervalhrs == ''] <- '00'
d[, time := as.ITime(paste(intervalhrs, intervalmins, sep = ':'), format = "%H:%M")]
```

```{r, previewdata, results='markup', echo=FALSE}
print(d,type = 'html')
```

##What is mean total number of steps taken per day?

```{r, question1}
# Calculate total number, mean, and median of steps per day.
dt1 <- d[!is.na(steps), list(sum(steps, na.rm = T),
                             round(mean(steps, na.rm = T),1),
                             quantile(steps, .50, na.rm = TRUE)),
         by = 'date']
setnames(dt1, old = c('V1','V2','V3'), new = c('Steps', 'Mean', 'Median'))
```

Which yields the following distribution of total, mean, and median number of steps per day.

```{r previewq1,results='markup', echo=FALSE}
print(dt1,type = 'html')
```

When plotted normally, we see the distribution is fairly nicely distributed around 
`r median(dt1$Steps)` steps per day. 

```{r plotq1,results='markup'}
# Plot histogram of Total steps per day
hist1 <- ggplot(dt1, aes(x = Steps)) +
  geom_histogram()
hist1
```

If we apply a log10 transformation the 2 outlying observations are easy to find even with a histogram.
```{r plotq1log,results='markup'}
hist1 + scale_x_log10()
```


##What is the average daily activity pattern?

```{r q2, results='markup'}
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
lines2

```

Clearly, most of the walking takes place in the mornings, pressumably on this person's way to work or as part of their morning routine. The highest activity is concentrated around `r maxIntLab`.


##Imputing missing values

```{r missings}
# Calculate number of observations with missing values (NA) in "steps"
nmissing <- nrow(d[,d[is.na(steps)]])
```

There are `r nmissing` observations without a record of the number of steps taken across that interval.

We calculate the average number of steps taken by weekday & interval to impute said missing values.

```{r impute}
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

# Create histogram of Total steps per day
hist3 <- ggplot(dt3.1, aes(x = Steps)) +
  geom_histogram()
```

When plotted normally, we see the distribution is fairly nicely distributed around 
`r as.integer(median(dt3.1$Steps))` steps per day instead of `r median(dt1$Steps)` steps before imputation.It would appear then that imputation has helped concentrate our data and reduced its variability.

```{r plotq3,echo=FALSE,results='markup'}
dt3.1
hist3
```

##Are there differences in activity patterns between weekdays and weekends?

```{r q4}
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
```
```{r previewq4,results='markup',echo=FALSE}
dt4
lines4
```