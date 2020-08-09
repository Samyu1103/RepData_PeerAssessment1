Reproducible Research: Peer Assessment 1
=======================================
# Loading and Processing the data


```r
#Load data
data_row <- read.csv("activity.csv")

#remove NA in data
data <- data_row[with(data_row,{!is.na(steps)}),]

#print out the first 20 rows
head(data,20)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
## 295     0 2012-10-02       30
## 296     0 2012-10-02       35
## 297     0 2012-10-02       40
## 298     0 2012-10-02       45
## 299     0 2012-10-02       50
## 300     0 2012-10-02       55
## 301     0 2012-10-02      100
## 302     0 2012-10-02      105
## 303     0 2012-10-02      110
## 304     0 2012-10-02      115
## 305     0 2012-10-02      120
## 306     0 2012-10-02      125
## 307     0 2012-10-02      130
## 308     0 2012-10-02      135
```

# What is the mean toal number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

-Calculate the total number of steps taken per day
-Make a histogram of the total number of steps taken each day
-Calculate and report the mean and median of the total number of steps taken per day


```r
by_day <- dplyr::group_by(data,date)
steps_by_day <- dplyr::summarise(by_day, total= sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
steps_by_day
```

```
## # A tibble: 53 x 2
##    date       total
##    <chr>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # … with 43 more rows
```


```r
hist(steps_by_day$total, main= "Histogram of total number of steps per day", xlab= "Total number of steps in a day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)


```r
steps_by_day <- unlist(steps_by_day)
summary(steps_by_day)
```

```
##    Length     Class      Mode 
##       106 character character
```

```r
Mean <- mean(steps_by_day)
```

```
## Warning in mean.default(steps_by_day): argument is not numeric or logical: returning NA
```

```r
Median <- median(steps_by_day)
```

```
## Warning in mean.default(sort(x, partial = half + 0L:1L)[half + 0L:1L]): argument is not numeric or logical: returning NA
```
Mean of the total number of steps per day is `Mean`, median is `Median`. 

# What is the average daily activity pattern?
-Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#processing data for plot
steps_by_interval <- aggregate(steps~interval, data, mean)
#create a time series plot
plot(steps_by_interval$interval, steps_by_interval$steps, type = "l", main = "Average number of steps over all days", xlab = "Interval", ylab = "Average number of steps", xlim = c(0,2000), ylim = c(0,200))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


```r
#find row with max steps
max_steps_row <- which.max(steps_by_interval$steps)
#find the interval with this max
max_interval <- steps_by_interval[max_steps_row,]
```
The interval `max_interval` has the maximum average value of steps(`steps`).

# Imputing Missing Values
-Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
-Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
-Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
-Create a new dataset that is equal to the original dataset but with the missing data filled in.
-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
na <- sum(is.na(data_row))
```
Total number of rows with NA's is `na`.
Now replace the NA's with the mean for the 5 min interval.


```r
data_imputed <- data_row
for (i in 1:nrow(data_imputed)) {
  if(is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- steps_by_interval[steps_by_interval$interval== interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
```

Now all NA’s are replaced with mean of 5-minute interval.

```r
df_imputed_steps_by_day <- aggregate(steps~date, data_imputed, sum)
head(df_imputed_steps_by_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
#mean and median of imputed data
mean_imputed <- aggregate(steps~date, data_imputed, mean)
median_imputed <- aggregate(steps~date, data_imputed, median)

#mean and median of data without NA's
mean_nona <- aggregate(steps~date, data_imputed, mean)
median_nona <- aggregate(steps~date, data_imputed, median)
```
Mean value stays the same but there is slight difference in median value.

# Are there differences in activity patterns between weekdays and weekends?

-Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
-Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <-"weekday"

# convert type_of_day from character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

# calculate average steps by interval across all days
df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

# create a plot
ggplot2:: qplot(interval, 
      steps, 
      data = df_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  ggplot2::facet_wrap(~ type_of_day, ncol = 1)
```

```
## Warning: Ignoring unknown parameters: type
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)















