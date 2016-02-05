---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---


## Loading and preprocessing the dataun


```r
require(dplyr)
require(ggplot2)
require(lattice)
require(data.table)

activity <- read.csv(unz("activity.zip", "activity.csv"))
activity <- data.table(activity)
activity <- activity %>% mutate(date=as.Date(date, "%Y-%m-%d"))
```

## What is mean total number of steps taken per day?


```r
dailyMetrics <- activity %>% 
    group_by(date) %>% 
    summarise(totalSteps=sum(steps, na.rm = T))

ggplot(dailyMetrics, aes(x=totalSteps)) +
    xlab("Total Steps") + ylab("Count") +
    geom_histogram(colour="black", fill="#99CCFF") +
    theme_bw()
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
r <- dailyMetrics %>% summarise(meanSteps=mean(totalSteps, na.rm = TRUE), medianSteps=median(totalSteps, na.rm = TRUE))
```

The average number of steps taken per day was 9354.2295082, and the median was 10395.

## What is the average daily activity pattern?


```r
intervalMetrics <- activity %>% 
    group_by(interval) %>% 
    summarise(meanSteps=mean(steps, na.rm = T))

ggplot(intervalMetrics, aes(interval, meanSteps)) + geom_line() +
    xlab("Interval") + ylab("Mean Steps") +
    theme_bw()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
maxInterval <- intervalMetrics[meanSteps==max(meanSteps),]
```

The interval period with the highest mean number of steps was 835, with an mean of 206.1698113 steps.

## Imputing missing values


```r
missingValuesCount = nrow(activity[is.na(steps),])

replaceNAs <- function(){
    values <- sapply(1:nrow(activity), function(index){
        if (is.na(activity[index, steps])){
            return (intervalMetrics[interval==activity[index, interval], meanSteps])
            }else{
                return(activity[index, steps])
                }
        })    
    }

newActivity <- activity %>% mutate(steps=replaceNAs())

newDailyMetrics <- newActivity %>% 
    group_by(date) %>% 
    summarise(totalSteps=sum(steps, na.rm = T))

ggplot(newDailyMetrics, aes(x=totalSteps)) +
    xlab("Total Steps") + ylab("Count") +
    geom_histogram(colour="black", fill="#99CCFF") +
    theme_bw()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
newR <- newDailyMetrics %>% summarise(meanSteps=mean(totalSteps, na.rm = TRUE), medianSteps=median(totalSteps, na.rm = TRUE))
```

There are a total of 2304 NAs in the data set. To fill in the missing values, the average number of steps for each interval were used.

By imputing the values, the distribution of the data changes. The average number of steps taken per day changes to 10766.19, and the median to 10766.19.
Now, the most frequent average number of steps taken are around 10000 and 11000. The number of days without any activity reduced from 10 to just about 2.

## Are there differences in activity patterns between weekdays and weekends?



```r
computeWeekday <- function(){
    weekdays <- sapply(newActivity$date, function(date){
    
    if (weekdays(date) %in% c("Saturday", "Sunday")){
    return("weekend")
    }else{
        return("weekday")
        }
    })
    
    as.factor(weekdays)
}

newActivity <- newActivity %>% mutate(weekday=computeWeekday())

newIntervalMetrics <- newActivity %>% 
    group_by(interval, weekday) %>% 
    summarise(meanSteps=mean(steps, na.rm = T))

xyplot(meanSteps~interval|weekday, 
       data=newIntervalMetrics,
       main="Average Number of Steps over Weekdays", 
       ylab="Number of Steps", xlab="Interval", type="l",
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Analyzing the patterns of steps during weekdays and weekends it seems that, on average, more steps are taken at each interval during the weekend, although the highest number of steps across all intervals is on weekdays.
