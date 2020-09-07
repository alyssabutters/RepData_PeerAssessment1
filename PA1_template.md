---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Introduction

Activity monitoring devices such as [Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up) have sparked a "quantified self" movement, in which adherents aim to improve their health through self-measurements, finding patterns in their activities, or simply because they enjoy interacting with the tech.

Two months worth of data (October/November 2012) were collected from an anonymous participant wearing such a personal monitoring device.  In 5 minute intervals throughout each day, the number of steps taken by the individual was recorded.

This assignment details a small amount of data analysis from this data.

## Loading and preprocessing the data

The necessary packages were loaded and the provided datafile was unzipped and the data read into a new R project:


```r
setwd("C:/Users/alyssa.butters/Desktop/Coursera/R Programming/Reproducible Research/RepData_PeerAssessment1")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)

data <- read.csv("repdata_data_activity/activity.csv", header=TRUE)
```


A quick glance at the data indicates the dates provided need to be converted into a date format.


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

As a first step in analysis, the total number of steps taken per day was determined.  In order to do this, the data was filtered such that any rows in which the number of steps was missing were removed.  The remaining rows were then grouped by day and the sum taken of each day.


```r
daily_total_steps <- data %>% filter(!is.na(steps)) %>% group_by(date) %>% 
        summarise(steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(daily_total_steps)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Then, a histogram was made detailing the daily total steps (in a calming deep sky blue color):


```r
hist(daily_total_steps$steps, breaks=15, main="Total Number of Steps per Day",
     xlab="Steps", ylab="Frequency", col="deepskyblue4")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Summary statistics can be derived in order to determine the mean and median number of steps per day


```r
summary(daily_total_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

Therefore, the mean number of steps per day was 10766 and the median number of steps per day was 10765.


## What is the average daily activity pattern?

Next, the average daily activity pattern was evaluated. It was already determined that there are missing values for steps, but are there any missing interval values? 

```{r{}
any_missing_intervals <- is.na(data$interval)
sum(any_missing_intervals)
```

There are no missing interval values, so only the missing steps values need be accommodated.  Again, the rows containing missing values for steps were filtered out, the the remaining rows were filtered by interval and then the mean number of steps per interval were calculated.


```r
ave_steps_intervals <- data %>% filter(!is.na(steps)) %>% group_by(interval) %>% 
        summarise(steps=mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(ave_steps_intervals)
```

```
## # A tibble: 6 x 2
##   interval  steps
##      <int>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
## 6       25 2.09
```

To visualize this, the average steps taken per interval were plotted (this time in a more dynamic dark hot pink):


```r
plot(ave_steps_intervals$interval, ave_steps_intervals$steps, type="l", 
     main="Average Steps Taken Per Five Minute Interval", 
     xlab="5 minute interval", ylab="Average Number of Steps", col="hotpink4")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

But which 5-minute interval contains the maximum number of steps, if the average per day is considered?


```r
max_steps <- max(ave_steps_intervals$steps)
ave_steps_intervals[ave_steps_intervals$steps == max_steps,]
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```

So the interval in which, on average, the greatest number of steps was taken is 206 in the 835 interval.

## Imputing missing values

However, it must be considered that simply ignoring the missing values for steps may introduce bias into the estimate, especially if the number of missing values is large.  It seems prudent to calculate how many values are missing in order to get a feel for if the proportion of missing values is large.


```r
sum(is.na(data$interval))
```

```
## [1] 0
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

So, as determined above, there are no missing values for the interval variable, however there are 2304 missing steps values.  That's quite large.  

In order to address this, the missing values can be imputed using a variety of different methods.  For simplicity, it was elected to replace any missing value with the mean value for that interval.  

First, a function is created that allows determination of the mean for a specified interval when the function is called.


```r
get_mean <- function(interval){
        ave_steps_intervals[ave_steps_intervals$interval==interval,]$steps
}
```

Next, this function is utilized in a for loop to fill in each individual missing value, to create a new dataframe that is identical to the original dataframe, except that every missing value has been imputed and replaced by the mean of the interval from which is was derived.


```r
imputed_data <- data
for(i in 1:nrow(imputed_data)){
        if(is.na(imputed_data[i,]$steps)){
                imputed_data[i,]$steps <- get_mean(imputed_data[i,]$interval)
        }        
}
```

Did the for loop effectively replace all missing values?  Let's check:


```r
sum(is.na(imputed_data$steps))
```

```
## [1] 0
```

Yep!  It worked.  There are no more missing values in the new dataset.

Now, once again the total number of steps per day were calculated, this time using the imputed dataset:


```r
imputed_daily_total_steps <- imputed_data %>% group_by(date) %>% 
        summarise(steps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(imputed_daily_total_steps)
```

```
## # A tibble: 6 x 2
##   date        steps
##   <date>      <dbl>
## 1 2012-10-01 10766.
## 2 2012-10-02   126 
## 3 2012-10-03 11352 
## 4 2012-10-04 12116 
## 5 2012-10-05 13294 
## 6 2012-10-06 15420
```

and the imputed data can be plotted in the same manner as previously:


```r
hist(imputed_daily_total_steps$steps, breaks=15, 
     main="Total Number of Steps per Day (With Imputation",
     xlab="Steps", ylab="Frequency", col="darkmagenta")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


Have the imputed values changed the mean or the median?  Let's look at the summary statistics on the imputed dataset.


```r
summary(imputed_daily_total_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

Not surprisingly, since the missing values were imputed with the mean for the interval, the mean number of steps per day did not change, 10766.  The median, 10766, moves slightly closer to the mean, because imputing the missing values with the mean for the interval would pull the median closer to the mean.

## Are there differences in activity patterns between weekdays and weekends?

Finally, it is interesting to evaluate whether the participants activity (indicated by mean number of steps) differed between weekdays and weekends.  In order to do this, first the day of the week for each date was determined:


```r
wkd <- weekdays(data$date)
```

However, this returns a list, which is not particularly helpful in and of itself.  Therefore, the list of days of the week created was bound to the imputed dataset as a new column representing a new factor variable.


```r
wkd_data <- cbind(imputed_data, wkd)
head(wkd_data)
```

```
##       steps       date interval    wkd
## 1 1.7169811 2012-10-01        0 Monday
## 2 0.3396226 2012-10-01        5 Monday
## 3 0.1320755 2012-10-01       10 Monday
## 4 0.1509434 2012-10-01       15 Monday
## 5 0.0754717 2012-10-01       20 Monday
## 6 2.0943396 2012-10-01       25 Monday
```

But still not quite what is needed.  There is now a new factor variable with the day of the week, however we want to look at weekdays compared to weekend days.  A for loop was therefore created that replaces the wkd variable value with "weekend" if the value is "Saturday" or "Sunday", and otherwise replaces the wkd value with "weekday".


```r
for (i in 1:nrow(wkd_data)) {
        if (wkd_data[i,]$wkd %in% c("Saturday","Sunday")) {
                wkd_data[i,]$wkd<-"weekend"
        }
        else{
                wkd_data[i,]$wkd<-"weekday"
        }
}
```

Okay, so now the data is in a useful form.  As previously, the data can now be grouped by interval and the mean number of steps per interval can be calculated.


```r
ave_interval_wkd <- wkd_data %>% group_by(interval, wkd) %>%
        summarise(steps=mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
head(ave_interval_wkd)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval wkd      steps
##      <int> <chr>    <dbl>
## 1        0 weekday 2.25  
## 2        0 weekend 0.215 
## 3        5 weekday 0.445 
## 4        5 weekend 0.0425
## 5       10 weekday 0.173 
## 6       10 weekend 0.0165
```

Following this, a panel plot can be made with a time series plot for average number of steps on weekdays and weekends, allowing comparison of these averages.


```r
xyplot(steps ~ interval | wkd, ave_interval_wkd, type = "l", layout = c(1, 2), 
       main="Average Number of Steps Per Time Interval (Weekdays versus
       Weekends", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

From the plot, it appears that the subject was consistently most active at a given interval on weekdays (interval 835, see code below), but otherwise is more consistently active on weekend days.


```r
max_steps_wkd <- max(ave_interval_wkd$steps)
ave_interval_wkd[ave_interval_wkd$steps == max_steps_wkd,]
```

```
## # A tibble: 1 x 3
## # Groups:   interval [1]
##   interval wkd     steps
##      <int> <chr>   <dbl>
## 1      835 weekday  230.
```

