Peer-graded Assignment Course Project 1
========================================

Mean Steps taken in a day


```r
#Load the data table package
library(data.table)
```

```
## data.table 1.10.4
```

```
##   The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way
```

```
##   Documentation: ?data.table, example(data.table) and browseVignettes("data.table")
```

```
##   Release notes, videos and slides: http://r-datatable.com
```

```r
activity <- read.csv("activity.csv")
#Converting the data frame into a datatable
activity <- data.table(activity)
activity$date <- as.Date(activity$date)

#Calculating and plotting the total number of steps in a day
totalActivity <- activity[, list(total= sum(steps, na.rm = T)), by=date]
hist(totalActivity$total, col = "magenta", main= "Total Number of steps in a day", xlab="Total Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
MeanActivity <- activity[, list(Mean= mean(steps, na.rm = T)), by=date]

#Calculating the Mean number of steps in a day
Mean <- mean(totalActivity$total, na.rm = T)

#Calculating the Median of the total steps in a day

Median <- median(totalActivity$total, na.rm = T)
```


1. Mean is 9354.2295082
2. Median is  10395

Average daily activity pattern(TimeSeries Analysis)



```r
MeanAc <- activity[, list(Mean= mean(steps, na.rm = T)), by=interval]
plot.ts(MeanAc$interval, MeanAc$Mean, type="l", pch=18, col="steel blue", xlab="5 Minute Interval", ylab="Mean steps across all the days", main="TimeSeries Analysis")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
MaxSteps <- max(MeanAc$Mean, na.rm=T)
rowNum <- which(MeanAc$Mean == MaxSteps)
MaxInterval <- MeanAc[rowNum,1]
```

The interval in which max number of steps is taken 835

Missing values in the DataSet

```r
CNa <-  sum(is.na(activity))
```

The number of missing values in the data set is 2304


```r
ac <- activity
# Replacing missing values with mean of that interval data
for( i in 1:17568  )
{ 
  
  if(is.na(ac$steps[i]))
  
    {
     j <- ac$interval[i]
     ac$steps[i] <-  MeanAc$Mean[MeanAc$interval==j ]
  
    }
}

totalAc <- ac[, list(total= sum(steps, na.rm = T)), by=date]

hist(totalAc$total, col=  "magenta", main= "Total Number of Steps  after imputing the data", xlab = "Total Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
MeanAc <- ac[, list(Mean= mean(steps, na.rm = T)), by=date]

#Calculating the Mean number of steps in a day
mean(totalAc$total)
```

```
## [1] 10766.19
```

```r
#Calculating the Median of the total steps in a day
median(totalAc$total)
```

```
## [1] 10766.19
```


Mean of the total number steps in a day doesn't effect much but the median does



```r
tempActivity <- ac
tempActivity$day <- weekdays(tempActivity$date) 
weekdy <- c( "Monday", "Tuesday", "Wednesday" ,"Thursday", "Friday")
weeken <- c("Saturday", "Sunday")
tempActivity$day = ifelse( tempActivity$day %in% weekdy, "WeekDay", "WeekEnd" )

MeanByWeekDay <- subset(tempActivity, day== "WeekDay")
MeanByWeekDay <- MeanByWeekDay[, list(Mean= mean(steps, na.rm = T)), by=interval]

MeanByWeekEnd <- subset(tempActivity, day== "WeekEnd")
MeanByWeekEnd <- MeanByWeekEnd[, list(Mean= mean(steps, na.rm = T)), by=interval]

rng <- range(MeanByWeekDay$Mean, MeanByWeekEnd$Mean)
par(mfrow= c(1, 2))
plot(MeanByWeekDay$interval, MeanByWeekDay$Mean, type="l", main="Week Day Data", ylim = rng, ylab="Number of Steps", xlab = "Interval" )
plot(MeanByWeekEnd$interval, MeanByWeekEnd$Mean, type="l", main="Week End Data", ylim= rng, ylab = "Number of Steps", xlab= "Interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


From the above two graphs we can notice that the activity on Week Day is compartively more than that on a Week End












