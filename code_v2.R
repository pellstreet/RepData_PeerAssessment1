#Reproducible data assignment: Course Project 1

setwd("C:/Users/epellichero/Desktop/Personal_docs/JH_certificate/5_Reproducible_research/Week_1/RepData_PeerAssessment1")

df<-read.csv("activity.csv",header=TRUE,colClasses=c("integer","Date","integer"))

# 1-What is mean total number of steps taken per day? ignore the NA data

#filter the NA data from the data frame

df.1<-na.omit(df)

#sum the steps taken per day

df.1_st<-aggregate(steps ~ date,df.1,sum)

#plot a histogram of the total steps per day
par(mar = c(5, 4, 4, 2), mgp = c(3, 1, 0))

hist(df.1_st$steps,col="blue", 
        main="Total steps per day", 
        ylab="Frequency (Number of days)", xlab="steps", xlim=c(0,25000),breaks=25)

#add grid and box

grid(col="black")
box()

#print the chart

dev.copy(png, file="n_steps_pr_day.png",width = 1000, height = 700)

dev.off()

#Calculate and report the mean and median of the total number of steps taken per day

mean(df.1_st$steps,na.rm=TRUE)
median(df.1_st$steps,na.rm=TRUE)

#2-What is the average daily activity pattern?

df.2_avg<-aggregate(steps ~ interval,df.1,mean)

par(mar = c(5, 6, 4, 2), mgp = c(3, 1, 0))

plot(df.2_avg$interval,df.2_avg$steps,type="l",col="blue", 
     main="Daily average steps per interval", 
     ylab="Average Number of steps", xlab="Interval",
     cex.main=1.2, cex.names=0.9,cex.axis=1,cex.lab=1)

grid()

dev.copy(png, file="average daily steps.png",width = 1000, height = 700)

dev.off()

#2 -Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

df.2_avg[df.2_avg$steps==max((df.2_avg$steps)),]

#3- Imputing missing values

#3.1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA s)

sum(is.na(df))
sum(is.na(df$steps))

#3.2 Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for 
#that day, or the mean for that 5-minute interval, etc.

df.3<-merge(df,df.2_avg,by="interval")

df.3$steps.x[is.na(df.3$steps.x)]<-df.3$steps.y[is.na(df.3$steps.x)]

#check there are no NAs value in data frame

sum(is.na(df.3))

#remove extra collumn with averages

df.3$steps.y<-NULL

#reanme steps column

names(df.3)[2]<-"steps_fill"

#3.3 Make a histogram of the total number of steps taken each day and Calculate and
#report the mean and median total number of steps taken per day. Do these values differ 
#from the estimates from the first part of the assignment? What is the impact of imputing 
#missing data on the estimates of the total daily number of steps?

#sum the steps taken per day

df.3_tot<-aggregate(steps_fill ~ date,df.3,sum)

#plot a histogram of the total steps per day

hist(df.3_tot$steps_fill,col="blue", 
     main="Total steps per day (NA values filled in)", 
     ylab="Frequency (Number of days)", xlab="steps", xlim=c(0,25000),breaks=25)

#add grid and box

grid(col="black")
box()

#print the chart

dev.copy(png, file="n_steps_pr_day_FILLED.png",width = 1000, height = 700)

dev.off()

#Calculate and report the mean and median of the total number of steps taken per day
3
mean(df.3$steps_fill)
median(df.3$steps_fill)

#4-Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
df.3$wkd<-weekdays(df.3$date)

a<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
b<-c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend")

for (i in 1:length(a)){

df.3$wkd<-mapply(gsub,a[i],b[i],df.3$wkd)}

#check new column

str(df.3$wkd)

#Make a panel plot containing a time series plot (i.e. type = "l" ) of the 5-minute interval 
#(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
#(y-axis). See the README file in the GitHub repository to see an example of what this plot should 
#look like using simulated data.

df.4<-aggregate(steps_fill ~ wkd+interval,df.3,mean)

library(ggplot2)

df.4$wkd<-as.factor(df.4$wkd)

g <- ggplot(df.4, aes(x=interval, y=steps_fill, color = wkd)) +
  geom_line() +
  facet_wrap(~ wkd,nrow=2,ncol=1)+
  labs(x="interval")+
  labs(y="steps")+
  ggtitle("Average Steps Over 5 Minute Intervals - Weekdays And Weekends")


print(g)

