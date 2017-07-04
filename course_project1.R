library(ggplot2)

#Reading in the data file
act.dat<- read.csv("activity.csv")
actdat.tidy<- na.omit(act.dat)

#Part 1: Finding the total number of steps taken in a day
q1.sum<- aggregate(steps~date,actdat.tidy,sum)

#Plotting the histogram of total number of steps taken in a day 
hist(q1.sum$steps, xlab = "Frequency", ylab = "Number of steps in a day", main = title("Plot 1"))

#Finding the mean and median 
q1.mean<- mean(q1.sum$steps)
q1.median<- median(q1.sum$steps)

#Part 2: Finding the average number of steps taken in a day during an interval
q2avg<- with(actdat.tidy,tapply(steps,interval,mean))
intervals<- actdat.tidy$interval
q2mean<-as.data.frame(cbind(intervals,q2avg))
plot(q2mean$intervals, q2mean$q2avg, type = "l", xlab = "Intervals", ylab = "average steps", main = title("Plot 2"))

maxsteps<- max(q2avg)
max.int<- q2mean[(q2mean$q2avg==maxsteps),]$intervals

#Part 3: Filling in the missing values
#1) finding the total number of NAs 
sum.na<- sum(is.na(act.dat))

#2) Fill in missing values with average values 
k1<- dim(act.dat)
q1.means<- aggregate(steps~date,act.dat, mean, na.action = NULL)
avg.steps<- mean(act.dat$steps, na.rm = TRUE)
q1.means[is.na(q1.means)]<- avg.steps
k2<- dim(q1.means)

for (x in 1:k1[1]){
    temp1<- act.dat[x,1]
    
    for (y in 1:k2[1]){
      if ((is.na(temp1))&&(act.dat[x,2]==q1.means[y,1])) act.dat[x,1]<- q1.means[y,2]
    }
}
# histogram of total number of steps taken per day
sum_updated<- aggregate(steps~date,act.dat,sum)
hist(sum_updated$steps, xlab = "Frequency", ylab = "Number of steps in a day", main = title("Plot 3"))

#3) recalculate the mean and median 
q1.mean.updated<- mean(sum_updated$steps)
q1.median.updated <- mean(sum_updated$steps)


#4) Comparing average steps taken (per interval) during Weekends and weekdays 

wd<- weekdays(as.Date(act.dat$date))
daycol<- vector()
# adding a new column named daycol with 2 levels: weekday/weekend
for (i in 1:k1[1]){
  daycol[i]<- if (wd[i]=="Sunday") "weekend" else if (wd[i]=="Saturday") "weekend" else "weekday"
}
act.dat<- cbind(act.dat,daycol)
library(ggplot2)
activity_day<- aggregate(steps~interval + daycol, act.dat, mean)
dayplot<- ggplot(activity_day, aes(x = interval, y = steps, color = daycol))+geom_line()+labs (x = "Interval", y = "avg steps", title = "Average steps per interval: weekdays vs weekdays")
print(dayplot)

