
#Loading and Processing the data
actdata<-read.csv("activity.csv")
actdata$date<-as.Date(actdata$date , format="%Y-%m-%d")
actdata$day = strftime(actdata$date,'%A')

weekdayslist <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
actdata$DayType <- factor((weekdays(actdata$date) %in% weekdayslist), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

#What is the total mean steps taken each day
datatotalstepsperday<-aggregate(actdata$steps,by=list(actdata$date),FUN=sum)
names(datatotalstepsperday)<-c("date","Total_Steps_Day")

dataavgstepsperday<-aggregate(actdata$steps,by=list(actdata$date),FUN=mean)
names(dataavgstepsperday)<-c("date","Avg_Steps_Day")

datamedianstepsperday<-aggregate(actdata$steps,by=list(actdata$date),FUN=median)
names(datamedianstepsperday)<-c("date","Median_Steps_Day")

library(xtable)
xttotalstepsperday<-xtable(datatotalstepsperday)
xtavgstepsperday<-xtable(dataavgstepsperday)
xtmedianstepsperday<-xtable(datamedianstepsperday)

histdata=hist(datatotalstepsperday$Total_Steps_Day,xlab = "Total Steps per day",main = "Histogram Total Steps Per day (missing data)")

meantotstepsperday<-as.integer(mean(datatotalstepsperday$Total_Steps_Day,na.rm = TRUE))
mediantotstepsperday<-as.double(median(datatotalstepsperday$Total_Steps_Day,na.rm = TRUE))

#What is the average activity type
dataavgstepsperinterval<-aggregate(actdata$steps,by=list(actdata$interval),FUN=mean,na.rm=TRUE)
names(dataavgstepsperinterval)<-c("Interval","Avg_Steps_Interval")

plot(dataavgstepsperinterval,type="l",xlab="5 minute intervals", ylab="Avergage no. of steps",main="Average daily 
     activity pattern")

maxinterval<-dataavgstepsperinterval$Interval[apply(dataavgstepsperinterval,2,which.max)][1]
maxintervalvalue=dataavgstepsperinterval$Interval[apply(dataavgstepsperinterval,2,which.max)][2]



#imputing missing values - get average steps for across the dataset
library(plyr)

rowNAs<-sum(is.na(actdata$steps))
dataNAs<-actdata[is.na(actdata$steps),]

actdataFilled<-actdata
imputemissingvalues<-function(steps) replace(steps,is.na(steps),mean(steps, na.rm = TRUE))

actdataFilled <- ddply(actdataFilled, ~ interval, transform, steps = imputemissingvalues(steps))

datatotalstepsperdayFilled<-aggregate(actdataFilled$steps,by=list(actdataFilled$date),FUN=sum)
names(datatotalstepsperdayFilled)<-c("date","Total_Steps_Day")

dataavgstepsperdayFilled<-aggregate(actdataFilled$steps,by=list(actdataFilled$date),FUN=mean)
names(dataavgstepsperdayFilled)<-c("date","Average_Steps_Day")

datamedianstepsperdayFilled<-aggregate(actdataFilled$steps,by=list(actdataFilled$date),FUN=median)
names(datamedianstepsperdayFilled)<-c("date","Median_Steps_Day")

library(xtable)
xttotalstepsperdayFilled<-xtable(datatotalstepsperdayFilled)
xtavgstepsperdayFilled<-xtable(dataavgstepsperdayFilled)
xtmedianstepsperdayFilled<-xtable(datamedianstepsperdayFilled)

histFilled=hist(datatotalstepsperdayFilled$Total_Steps_Day,xlab = "Total Steps per day",main = "Histogram Total Steps Per day (imputed data)")

#Comparison
library(lattice)
datatotalstepsperdayFilled$Type="Imputed Data"
datatotalstepsperday$Type="Missing Data"
combindeddata=rbind(datatotalstepsperdayFilled,datatotalstepsperday)
x_breaks <- c(0,5000 , 10000, 15000, 20000,25000)
histogram(~ Total_Steps_Day | Type, data=combindeddata,breaks = x_breaks)

#Weekdays and weekends
library(lattice)
dataavgstepsperdaytype<-aggregate(actdataFilled$steps~actdataFilled$interval+actdataFilled$DayType,actdataFilled,FUN=mean,na.rm=TRUE)
names(dataavgstepsperdaytype)<-c("Interval","DayType","Steps")
xyplot(Steps~Interval|DayType,data=dataavgstepsperdaytype,type="l",
       layout=c(1,2))



library(dplyr)
actdata%>%
  group_by(date)%>% 
  summarise(Mean=mean(steps), Max=max(steps), Min=min(steps), Median=median(steps), Std=sd(steps))
