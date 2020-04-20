Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

Data
----

The data for this assignment can be downloaded from the course web site:

Dataset:
\[<a href="http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" class="uri">http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip</a>\]

The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as NA)
-   **date**: The date on which the measurement was taken in YYYY-MM-DD
    format
-   **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

    #Read data from your working directory
    setwd("C:/Users/msacevich/Documents/Rstudio")
    ActivityData <- read.csv("activity.csv", header=TRUE)

    #Missing values
    ActivityDataNulls <- ActivityData[is.na(ActivityData$steps),]

    #Data without any missing values
    ActivityData1 <- ActivityData[!is.na(ActivityData$steps),]

What is mean total number of steps taken per day?
-------------------------------------------------

### 1. Calculate the total number of steps taken per day

    steps_per_day <- aggregate(steps ~ date, ActivityData1, sum)

### 2. Make a histogram of the total number of steps taken each day

    hist(steps_per_day$steps, main = "Total number of steps per day", xlab = "Steps per day", col = "light blue")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### 3. Calculate and report the mean and median of the total number of steps taken per day

    mean(steps_per_day$steps)

    ## [1] 10766.19

    median(steps_per_day$steps)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    steps_by_interval <- aggregate(steps ~ interval, ActivityData1, mean)
    plot(steps_by_interval$interval, steps_by_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    max <- steps_by_interval[which.max(steps_by_interval$steps),1]
    print(max)

    ## [1] 835

Imputing missing values
-----------------------

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    Nulls <- sum(!complete.cases(ActivityData))
    print(Nulls)

    ## [1] 2304

### 2,3. Devise a strategy for filling in all of the missing values in the dataset.Create a new dataset that is equal to the original dataset but with the missing data filled in.

    mean_steps <- with(ActivityData1, tapply(steps, ActivityData1$interval, mean))
    ActivityDataNulls$steps <- mean_steps

    NewActivityData <- rbind(ActivityData1, ActivityDataNulls)

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

    steps_per_day2 <- aggregate(steps ~ date, NewActivityData, sum)
    hist(steps_per_day2$steps, main = "Total number of steps per day", xlab = "Steps per day", col = "blueviolet")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    mean(steps_per_day2$steps)

    ## [1] 10766.19

    median(steps_per_day2$steps)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    NewActivityData$day <- "weekday"
    NewActivityData$day[weekdays(as.Date(NewActivityData$date), abb=T) %in% c("Sat","Sun")] <- "weekend"
    table(NewActivityData$day)

    ## 
    ## weekday weekend 
    ##   12960    4608

### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    library(lattice)
    meanstepsbyday <- aggregate(steps ~ interval + day, data=NewActivityData, FUN="mean")
    xyplot(steps ~ interval | day, data=meanstepsbyday, type="l", grid=T, layout=c(1,2), ylab="Number of steps", xlab="5-min. intervals from midnight", main="Average  5-min. activity intervals: Weekdays vs. Weekends")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)
