---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


#### Loading and preprocessing the data

```r
getwd()
```

```
## [1] "C:/Users/Ester/Desktop/R/Cursos/COURSERA/DATASCIENCE_Foundations using R/CURSO5_Reproducible Research/RepData_PeerAssessment1"
```

```r
document <- read.csv("activity.csv", header = TRUE, sep = ",")
# Brief analysis of the data
class(document)
```

```
## [1] "data.frame"
```

```r
dim(document)
```

```
## [1] 17568     3
```

```r
str(document)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(document)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(document)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
# Review if there are some missing values and how many
sum(is.na(document))
```

```
## [1] 2304
```

```r
mean(is.na(document))
```

```
## [1] 0.04371585
```

```r
# The class of the variable 'date' is Factor. I change it to Date class
document$date <- as.Date(document$date, format = "%Y-%m-%d")
class(document$date)
```

```
## [1] "Date"
```

#### What is mean total number of steps taken per day?

```r
# Total numbers taken per day
steps_byDay <- aggregate(steps ~ date, data = document, sum)
summary(steps_byDay)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

```r
steps_byDay
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
# Histogram
hist(steps_byDay$steps, main = "Steps per day", xlab = "Steps", ylim = c(0, 15), col = "purple", breaks = seq(0, 25000, by = 1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dev.copy(png, "Plot_Question_1.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# mean and median of the total number of steps taken per day
mean_byDay <- mean(steps_byDay$steps, na.rm = TRUE)
mean_byDay
```

```
## [1] 10766.19
```

```r
median_byDay <- median(steps_byDay$steps, na.rm = TRUE)
median_byDay
```

```
## [1] 10765
```

#### What is the average daily activity pattern?

```r
# series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
interval_mean <- aggregate(steps ~ interval, data = document, mean, na.rm = TRUE)
plot(interval_mean$interval, interval_mean$steps, type = "l", col = "purple", xlim = c(0, 2500), main = "Average daily", xlab = "Interval", ylab = "Steps mean")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, "Plot_Question_2.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps <- interval_mean[which.max(interval_mean$steps),]$interval
max_steps
```

```
## [1] 835
```
#### Imputing missing values

```r
# Total number of missing values in the dataset (i.e. the total number of rows with NAs
null_rows <- sum(is.na(document$steps))
null_rows
```

```
## [1] 2304
```

```r
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# For this question, I have created a new column that include the mean of each interval by any value of 'steps' = NA
document$imputSteps <- ifelse(is.na(document$steps), interval_mean$steps[match(document$interval, interval_mean$interval)], document$steps)

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
completeDocument <- data.frame(steps = document$imputSteps, interval = document$interval, date = document$date)

# histogram of the total number of steps taken each day.
steps_byDayComplet <- aggregate(steps ~ date, data = completeDocument, sum)
hist(steps_byDayComplet$steps, main = "Steps per day complete", xlab = "Steps", ylim = c(0, 20), col = "purple", breaks = seq(0, 25000, by = 1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dev.copy(png, "Plot_Question_3.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

```r
# Calculate and report the mean and median total number of steps taken per day.
mean_byDayComplet <- mean(steps_byDayComplet$steps, na.rm = TRUE)
mean_byDayComplet
```

```
## [1] 10766.19
```

```r
median_byDayComplete <- median(steps_byDayComplet$steps, na.rm = TRUE)
median_byDayComplete
```

```
## [1] 10766.19
```

```r
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

# By changing the group we can see that there are very few changes in the mean value. The 1000 range is slightly higher, changes from about 10 frequencies to about 18
```


#### Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
str(completeDocument)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
```

```r
# I have already change the class of date variable in the beginning of this exercice. 

# Now we need identify what name of the day are each date. To do this I apply the function 'weekday()'
completeDocument$day_name <- weekdays(completeDocument$date)

# Once we have the names by each date, we have to create a new variable to classify and group by 'weekend' or 'weekday'
completeDocument$type_of_day <- ifelse(completeDocument$day_name == "sábado" | completeDocument$day_name == "domingo", "Weekend", "Weekday")

# Plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

# I have created two groups, one per type of day 'weekend' and the other with 'weekday'
weekend <- subset(completeDocument, type_of_day == "Weekend")
weekday <- subset(completeDocument, type_of_day == "Weekday")
# Now I calculate the mean by each group created
average_weekend <- aggregate(steps ~ interval, data = weekend, mean, na.rm = TRUE)
average_weekday <- aggregate(steps ~ interval, data = weekday, mean, na.rm = TRUE)

# Then I apply the 'par()' function to represent the layout for both plots together
par(mfrow = c(2, 1), mar = c(4, 4, 4, 1))
plot(average_weekend$interval, average_weekend$steps, type = "l", ylim = c(0, 250), ylab = "Frequency_weekend", xlab = "Interval", main = "Weekend", col = "purple")
plot(average_weekday$interval, average_weekday$steps, type = "l", ylim = c(0, 250), ylab = "Frequency_weekday", xlab = "Interval", main = "Weekday", col = "purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png, "Plot_Question_4.png", height = 480, width = 480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```


