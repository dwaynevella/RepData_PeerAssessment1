library(dplyr)
library(ggplot2)

df<-tbl_df(read.csv("activity.csv"))

df$date<-as.Date(df$date, format="%Y-%m-%d")
df_filtered<-df[complete.cases(df), ]
df_grouped<-group_by(df, by=date)
df_summary <- summarize(df_grouped, Steps=sum(steps))

##### Histogram of the total number of steps taken each day ##### PART2
print(ggplot(df_summary, aes(x=by, y=Steps))+geom_bar(stat="identity")+xlab("Date")+ggtitle("Plot of Number of Steps per Date"))

##### Mean and median number of steps taken each day #### PART3
df_mean_median <- summarize(df_grouped,Mean=mean(steps, na.rm = TRUE), Mdn=median(steps,na.rm = TRUE))
print(ggplot(df_mean_median, aes(x=by, y=Mean))+geom_line(aes(y=Mean,color="Mean"))+geom_line(aes(y=df_mean_median$Mdn,color="Median"))+xlab("Date")+ylab("Mean & Median"))

#### Time series plot of the average number of steps taken #### PART4
df_time <- df_filtered %>% group_by(by=interval) %>% summarize(Steps=mean(steps))
print(ggplot(df_time, aes(x=by, y=Steps))+geom_line()+xlab("Interval")+ylab("Mean Number of Steps"))

#### The 5-minute interval that, on average, contains the maximum number of steps #### PART 5
Max_steps<- df_time[which.max(df_time$Steps),1]

#### Code to describe and show a strategy for imputing missing data #### PART 6
sum(is.na(df$steps))/nrow(df)*100 ## computing % of observations with mssing steps
incompletecases <- !(complete.cases(df$steps))
imputed_data <- mice(df[,c(1,3)], m=5)
completeData<-complete(imputed_data,5) #using the 5th set of imputed data
completeData<-cbind(completeData,df$date)


#### Histogram of the total number of steps taken each day after missing values are imputed #### PART7
df_grouped <- group_by()
df_summary <- summarize(completeData, Steps=sum(steps))
print(ggplot(df_summary, aes(x=by, y=Steps))+geom_bar(stat="identity")+xlab("Date")+ggtitle("Plot of Number of Steps per Date"))

#### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends #### PART 8
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df_filtered$wDay <- weekdays(df_filtered$date)
df_filtered$wDay <- factor((weekdays(df_filtered$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

df_wday_grouped<-df_filtered %>% group_by(interval, wDay) %>% summarise(Mn=mean(steps))
yweekend<-df_wday_grouped$Mn[df_wday_grouped$wDay=="weekend"]
yweekday<-df_wday_grouped$Mn[df_wday_grouped$wDay=="weekday"]

intervals<-unique(df_wday_grouped$interval)
p1<- ggplot(df_wday_grouped, aes(x=intervals, y=yweekend))+geom_line()

yweekend<-df_wday_grouped$Mn[df_wday_grouped$wDay=="weekend"]
yweekday<-df_wday_grouped$Mn[df_wday_grouped$wDay=="weekday"]

par(mfrow=c(1,2))
plot(intervals, yweekend, xlab="Intervals", type = "l", ylab="Average Steps", main="Average Steps per Interval during Weekend")
plot(intervals, yweekday, xlab="Intervals",type = "l", ylab="Average Steps", main="Average Steps per Interval during Weekday")







