# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())

```r
library(ggplot2)
library(lattice)
setwd("~/GitHub/RepData_PeerAssessment1")
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}

activity <- read.csv('activity.csv')
```
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
stepsperday <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```
2. Make a histogram of the total number of steps taken each day

```r
hist(stepsperday, main='Total Steps by Day', xlab='Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsperday, na.rm= TRUE)
```

```
## [1] 9354.23
```

```r
median(stepsperday, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanstepsbyint <- aggregate(steps ~ interval, data=activity, FUN=mean)

plot(meanstepsbyint, type='l', main='Avg Steps Taken by Interval', xlab='5 Min Intervals', ylab='Avg Steps Taken')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meanstepsbyint$interval[which.max(meanstepsbyint$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nullval<-which(is.na(activity))
length(nullval)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Strategy replaces NA's with mean for interval.

```r
actrepmerge <- merge(activity, meanstepsbyint, by = "interval", suffixes = c("",     ".y"))
nullval2 <- is.na(actrepmerge$steps)
actrepmerge$steps[nullval2] <- actrepmerge$steps.y[nullval2]
actrepmerge <- actrepmerge[, c(1:3)]
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
actwnonulls <- tapply(actrepmerge$steps, actrepmerge$date, sum, na.rm=TRUE) 
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

```r
hist(actwnonulls, main = "Total Steps per Day no Missing Values", xlab = "Total Steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Reduces size of first 2 BIN's from previous histogram.


```r
mean(actwnonulls, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(actwnonulls, na.rm = TRUE)
```

```
## [1] 10766.19
```

Mean and median are the same and slightly larger than the computation when ignoring NA's.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdayorend <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
actrepmerge$wday <- as.factor(sapply(actrepmerge$date, FUN=weekdayorend))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
actbywkday <- aggregate(actrepmerge$steps, by=list(actrepmerge$wday, actrepmerge$interval), mean)

names(actbywkday) <- c("wday", "interval", "steps")
xyplot(steps ~ interval | wday, actbywkday, type = "l", layout = c(1,2),
       xlab = "Interval", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
