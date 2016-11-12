# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The dataset is from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:
steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

The first step is to load the data to data frame df. The data file name is activity.csv


```r
df <- read.csv('activity.csv')

df.clean <- na.omit(df)
```


## What is mean total number of steps taken per day?
First calculate sum, mean and median by date. If no data is available for a given day, NA is used for that day.

```r
library(doBy)
df.byDay <- summaryBy(steps ~ date, data=df, FUN = list(sum, mean, median), na.rm=TRUE)
```
1. Make a histogram of the total number of steps taken each day

```r
## get total steps per day
##df.total <- setNames(aggregate(df$steps, by=list(df$date), FUN="sum"),c("date","steps"))

##df.total.steps <- na.omit(as.numeric(df.total$steps))
hist(df.byDay$steps.sum, main="Total number of steps taken per day in two months", xlab="Total Steps")
```

<img src="Figs/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />
2. Calculate and report the mean and median total number of steps taken per day

```r
## The mean and median is calculated in df.byDate. We just need to display them
##subset(df.byDay,select = c(date, steps.mean, steps.median))
```


```r
hist(df.byDay$steps.mean, main="Mean total number of steps taken per day in two months", xlab="Mean Total Steps")
```

<img src="Figs/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

```r
hist(df.byDay$steps.median, main="Median total number of steps taken per day in two months", xlab="Median Total Steps")
```

<img src="Figs/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
df.byInterval <- summaryBy(steps ~ interval, data=df, FUN = list(mean), na.rm=TRUE)
plot(df.byInterval$interval,df.byInterval$steps.mean,type = "l", main = "Average number of steps taken by Interval", xlab = "Interval", ylab = "Average Steps")
```

<img src="Figs/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />


```r
df.max <- df.byInterval[df.byInterval$steps.mean == max(df.byInterval$steps.mean),]
```
The 5-minute interval "835" , on average across all the days in the dataset, contains the maximum number of steps 206

## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NaCount <- sum(is.na(df$steps))
```
There are 2304 missing values in the dataset

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The missing values will be filled with the mean for that day. For the days that have no data, the missing data will be filled with mean of that interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
Two new datasets are created with the missing data filled in with mean and median values for that data.

```r
library(zoo)
## filling missing data with mean for that day
dfNewMean <- transform(df, steps_new = na.aggregate(steps, by=date, FUN = mean))
dfNewMean <- transform(dfNewMean, steps_new = na.aggregate(steps, by=interval, FUN = mean))

## filling missing data with median for that day
dfNewMedian <- transform(df, steps_new = na.aggregate(steps, by=date, FUN = median))
dfNewMedian <- transform(dfNewMedian, steps_new = na.aggregate(steps, by=interval, FUN = median))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

As shown in the graph below, after filling in missing data with mean values for that day, only the mean total step between 30 to 40 increases. The median values are also changes.

```r
dfNewMean.byDay <- summaryBy(steps_new ~ date, data=dfNewMean, FUN = list(mean))
dfNewMedian.byDay <- summaryBy(steps_new ~ date, data=dfNewMean, FUN = list(median))

##plot two histograms to compare the original data vs filled data
par(mfrow=c(2,2))
hist(df.byDay$steps.mean, xlim=c(0, 80), ylim=c(0, 30), main="Original Data", xlab = "Mean Total Steps Per Day")
hist(dfNewMean.byDay$steps_new.mean, xlim=c(0, 80), ylim=c(0, 30), main="Filled Data With Mean Total", xlab = "Mean Total Steps Per Day")
hist(df.byDay$steps.median, xlim=c(0, 80), ylim=c(0, 60), main="Original Data", xlab = "Median Total Steps Per Day")
hist(dfNewMedian.byDay$steps_new.median, xlim=c(0, 80), ylim=c(0, 60), main="Filled Data With Mean Total", xlab = "Median Total Steps Per Day")
```

<img src="Figs/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

As shown in the graph below, after filling in missing data with median values for that day, the mean total step between 0 to 10 increases. The frequency of median values 0 is also increases.

```r
dfNewMean.byDay <- summaryBy(steps_new ~ date, data=dfNewMedian, FUN = list(mean))
dfNewMedian.byDay <- summaryBy(steps_new ~ date, data=dfNewMedian, FUN = list(median))

##plot two histograms to compare the original data vs filled data
par(mfrow=c(2,2))
hist(df.byDay$steps.mean, xlim=c(0, 80), ylim=c(0, 30), main="Original Data", xlab = "Mean Total Steps Per Day")
hist(dfNewMean.byDay$steps_new.mean, xlim=c(0, 80), ylim=c(0, 30), main="Filled Data With Median Total", xlab = "Mean Total Steps Per Day")
hist(df.byDay$steps.median, xlim=c(0, 80), ylim=c(0, 60), main="Original Data", xlab = "Median Total Steps Per Day")
hist(dfNewMedian.byDay$steps_new.median, xlim=c(0, 80), ylim=c(0, 60), main="Filled Data With Median Total", xlab = "Median Total Steps Per Day")
```

<img src="Figs/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(plyr)

dfNewMean$week <-ifelse(weekdays(as.Date(dfNewMean$date)) %in% c("Saturday","Sunday"), "Weekend","weekday")

dfNewMean.week <- subset(dfNewMean, select = c(interval, week, steps_new))

dfNewMean.byInterval <- ddply(dfNewMean.week, .(week, interval), numcolwise(mean))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
xyplot(steps_new ~ interval | week, dfNewMean.byInterval, type = "l",layout=c(1,2), ylab = "Number of Steps")
```

<img src="Figs/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />


