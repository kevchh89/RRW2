---
title : My proyect
output: 
  html_document: 
    keep_md: yes
author: KB
---


1 Read data set
```{r,echo=TRUE}
library(lubridate)
Dset<-read.csv('activity.csv',na.strings = 'NA')
Dset$date<-ymd(Dset$date)

```


2 Histogram of the total steps per day
```{r,echo=TRUE,results='asis'}
StepDay<-tapply(Dset$steps,Dset$date,sum )
hist(StepDay,xlab='Date',main = 'Histogram of steps per day')
```
3 Mean and median of the total steps per day
```{r,echo=TRUE}
tapply(Dset$steps,Dset$date,mean)
tapply(Dset$steps,Dset$date,median)
```

4 Time series plot of the average number of steps taken
```{r,echo=TRUE}
library(reshape2)

StepDayMean<-melt(tapply(Dset$steps,Dset$date,mean))
StepDayMedian<-melt(tapply(Dset$steps,Dset$date,median))
plot(x=as.numeric(ymd(StepDayMean[,1])),y=StepDayMean[,2],type='l',
     xlab='Date',ylab = 'Mean of steps per day',xaxt='n')
plot(x=as.numeric(ymd(StepDayMedian[,1])),y=StepDayMedian[,2],type='l',
     xlab='Date',ylab = 'Median of steps per day',xaxt='n')
```

5 The 5-minute interval that, on average, contains the maximum number of steps
```{r,echo=TRUE}
DsetNArm<-subset(Dset, !is.na(Dset$steps))
DsetNArm$interval[DsetNArm$steps==max(DsetNArm$steps)]
```

6 Code to describe and show a strategy for imputing missing data

```{r,echo=TRUE}
NAs<-which(is.na(Dset$steps))
Dset$steps[NAs]<-0
```

7 Histogram of the total number of steps taken each day after missing values are imputed

```{r,echo=TRUE}
hist(tapply(Dset$steps,Dset$date,sum),xlab = 'date',main = 'Histogram of steps per day')
```

8 Panel plot comparing the average number of steps taken per 5-minute 
interval across weekdays and weekends

```{r,echo=TRUE}
library(lattice)
Wds2<-wday(Dset$date)
Wds3<-NA
length(Wds3)<-length(Wds2)
Wds3[Wds2==2|Wds2==3|Wds2==4|Wds2==4|Wds2==6]<-'Weekdays'
Wds3[is.na(Wds3)]<-'Weekends'
Dset<-cbind(Dset,Wds3)
WdDate<-paste(Dset$Wds3,Dset$date)
Dset3<-cbind(Dset,WdDate)
Data8<-melt(tapply(Dset3$steps,Dset3$WdDate,mean))
Data8week<-unlist(strsplit(as.character(Data8$Var1), ' '))[seq(1,length(Data8$Var1)*2,by=2)]
Data8Date<-unlist(strsplit(as.character(Data8$Var1), ' '))[seq(2,length(Data8$Var1)*2,by=2)]
Data8Date<-ymd(Data8Date)
Data82<-cbind(Data8,Data8week,Data8Date)
xyplot(value~as.numeric(Data8Date)|Data8week,data=Data82, type='l',xlab='Date')
```
