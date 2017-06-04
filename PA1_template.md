# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Reading the file in and converting the Date column into date format. 
Also adding Day names column and a Day type column. A sample of the processed data is shown below.


```r
actdata<-read.csv("activity.csv")
actdata$date<-as.Date(actdata$date , format="%Y-%m-%d")
actdata$day = strftime(actdata$date,'%A')
weekdayslist <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
actdata$DayType <- factor((weekdays(actdata$date) %in% weekdayslist), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
print(head(actdata))
```

```
##   steps       date interval    day DayType
## 1    NA 2012-10-01        0 Monday weekday
## 2    NA 2012-10-01        5 Monday weekday
## 3    NA 2012-10-01       10 Monday weekday
## 4    NA 2012-10-01       15 Monday weekday
## 5    NA 2012-10-01       20 Monday weekday
## 6    NA 2012-10-01       25 Monday weekday
```

## What is mean total number of steps taken per day?
1. Caluclating the total number of steps per day. A sample of the data is shown below.


```r
datatotalstepsperday<-aggregate(actdata$steps,by=list(actdata$date),FUN=sum,na.rm = TRUE)
names(datatotalstepsperday)<-c("date","Total_Steps_Day")
print(head(datatotalstepsperday))
```

```
##         date Total_Steps_Day
## 1 2012-10-01               0
## 2 2012-10-02             126
## 3 2012-10-03           11352
## 4 2012-10-04           12116
## 5 2012-10-05           13294
## 6 2012-10-06           15420
```
2. Histogram of the total number of steps taken each day


```r
histdata=hist(datatotalstepsperday$Total_Steps_Day,xlab = "Total Steps per day",main = "Histogram Total Steps Per day (missing data)", col="red")
```

![](PA1_template_files/figure-html/histogramtotnosteps-1.png)<!-- -->

3.1 The mean and median total number of steps each day


```r
dataavgstepsperday<-aggregate(actdata$steps,by=list(actdata$date),FUN=mean,na.rm = TRUE)
names(dataavgstepsperday)<-c("date","Avg_Steps_Day")
meantotstepsperday<-as.integer(mean(datatotalstepsperday$Total_Steps_Day,na.rm = TRUE))
mediantotstepsperday<-as.integer(median(datatotalstepsperday$Total_Steps_Day,na.rm = TRUE))
```


The mean total number of steps per day through the entire dataset is 9354.
The median total number of steps per day through the entire dataset is 10395.


## What is the average daily activity pattern?
1. Time series plot of 5 minute interval and the avegae number of steps taken, averaged across all days

```r
dataavgstepsperinterval<-aggregate(actdata$steps,by=list(actdata$interval),FUN=mean,na.rm=TRUE)
names(dataavgstepsperinterval)<-c("Interval","Avg_Steps_Interval")
maxinterval<-dataavgstepsperinterval[which.max(dataavgstepsperinterval$Avg_Steps_Interval),"Interval"]
maxintervalvalue<-dataavgstepsperinterval[which.max(dataavgstepsperinterval$Avg_Steps_Interval),"Avg_Steps_Interval"]

plot(dataavgstepsperinterval,type="l",xlab="5 minute intervals", ylab="Avergage no. of steps",main="Average daily 
     activity pattern",col="red")
```

![](PA1_template_files/figure-html/avgstepstakenperinterval-1.png)<!-- -->

2. Five minute interval that on an average contains maximum number of steps is 835 and the value is 206.1698113 steps


## Imputing missing values
1. Total number of NAs in the dataset


```r
rowNAs<-sum(is.na(actdata$steps))
print(rowNAs)
```

```
## [1] 2304
```

2/3. Filling NAs with the average number of steps corresponding to that interval. A sample of the filled dataset is shown below


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.3
```

```r
actdataFilled<-actdata
imputemissingvalues<-function(steps) replace(steps,is.na(steps),mean(steps, na.rm = TRUE))
actdataFilled <- ddply(actdataFilled, ~ interval, transform, steps = imputemissingvalues(steps))
print(head(actdataFilled))
```

```
##       steps       date interval       day DayType
## 1  1.716981 2012-10-01        0    Monday weekday
## 2  0.000000 2012-10-02        0   Tuesday weekday
## 3  0.000000 2012-10-03        0 Wednesday weekday
## 4 47.000000 2012-10-04        0  Thursday weekday
## 5  0.000000 2012-10-05        0    Friday weekday
## 6  0.000000 2012-10-06        0  Saturday weekend
```

4. Histogram of the total number of steps taken each day


```r
datatotalstepsperdayFilled<-aggregate(actdataFilled$steps,by=list(actdataFilled$date),FUN=sum)
names(datatotalstepsperdayFilled)<-c("date","Total_Steps_Day")

dataavgstepsperdayFilled<-aggregate(actdataFilled$steps,by=list(actdataFilled$date),FUN=mean)
names(dataavgstepsperdayFilled)<-c("date","Average_Steps_Day")

datamedianstepsperdayFilled<-aggregate(actdataFilled$steps,by=list(actdataFilled$date),FUN=median)
names(datamedianstepsperdayFilled)<-c("date","Median_Steps_Day")

histFilled=hist(datatotalstepsperdayFilled$Total_Steps_Day,xlab = "Total Steps per day",main = "Histogram Total Steps Per day (imputed data)",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

By filling in the missing values, the total steps per day have increased, so there are more days having the mean number of steps.


```r
library(lattice)
datatotalstepsperdayFilled$Type="Imputed Data"
datatotalstepsperday$Type="Missing Data"
combindeddata=rbind(datatotalstepsperdayFilled,datatotalstepsperday)
x_breaks <- c(0,5000 , 10000, 15000, 20000,25000)
histogram(~ Total_Steps_Day | Type, data=combindeddata,breaks = x_breaks,xlab ="Total Steps per day", ylab="Frequency" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

Line chart showing the average steps taken during each interval during a weekday and a weekend. The lot shows that the peak number of steps during weekdays is higher than during weekends. Also during weekdays the activity levels start earlier in the day than during weekends.


```r
library(lattice)
dataavgstepsperdaytype<-aggregate(actdataFilled$steps~actdataFilled$interval+actdataFilled$DayType,actdataFilled,FUN=mean,na.rm=TRUE)
names(dataavgstepsperdaytype)<-c("Interval","DayType","Steps")
xyplot(Steps~Interval|DayType,data=dataavgstepsperdaytype,type="l",
       layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
