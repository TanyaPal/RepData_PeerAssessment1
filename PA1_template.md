# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

activity<-read.csv("activity.csv")
tot_steps <-sum(act$steps,na.rm=TRUE)
steps_per_day<- aggregate(steps~date,data=act,FUN=sum,na.rm=TRUE)
hist(steps_per_day$steps)

## What is mean total number of steps taken per day?

mean(steps_per_day$steps)
median(steps_per_day$steps)

## What is the average daily activity pattern?
five_min_average<- aggregate(steps~interval,data=activity,FUN = mean,na.rm=TRUE)
plot(x=five_min_average$interval, y=five_min_average$steps, type = "l")
 
 max_steps <- max(five_min_average$steps)
 for (i in 1:288) 
  {
     if (five_min_average$steps[i] == max_steps)
         five_min_interval_at_max_steps <- five_min_average$interval[i]
  }

## Imputing missing values
total_na <- 0
for (i in 17568)
{
if (is.na(activity$steps[i]))
total_na <-total_na+1
}

activity_filled_in <- activity
for (i in 1:17568) # loop to find the na
{
    if(is.na(activity_filled_in$steps[i])) # if steps is na store the pointer 
    { 
        five_min_pointer <- activity_filled_in$interval[i] #store the value of pointer to find the mean on five minute interval
        for (j in 1:288)  # loop to find the value of pointer on the data frame of five minute interval
        {
            if (five_min_average$interval[j] == five_min_pointer) # finding the value of mean of five minute interval data frame
                activity_filled_in$steps[i] <- five_min_average$steps[j] # replacing the na by the mean in that fime minute interval 

        }
    }
}

## Calculating the total number of steps taken each day and stored in a variable
total_steps_each_day_filled_in <- aggregate(steps~date, data=activity_filled_in, FUN=sum, na.rm=TRUE)

## Generating the Histogram by each day with new dataset (activity_filled_in)
hist(total_steps_each_day_filled_in$steps)

## funtion mean and median
total_steps_each_day_mean_filled_in <- mean(total_steps_each_day_filled_in$steps)
total_steps_each_day_median_filled_in <- median(total_steps_each_day_filled_in$steps)


## Are there differences in activity patterns between weekdays and weekends?

activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")
activity_wd<- data.frame(date= activity$date, weekday= tolower(weekdays(activity$date)), steps=activity$steps, interval=activity$interval)
 activity_wd<- cbind(activity,daytype=ifelse(activity_wd$weekday == "saturday" | activity_wd$weekday == "sunday", "weekend", "weekday"))

weekday<- grep("weekday", activity_wd$daytype)
weekday_frame <- activity_wd[weekday,]
weekend_frame <- activity_wd[-weekday,]

five_minutes_average_weekday <- aggregate(steps~interval, data=weekday_frame, FUN=mean, na.rm=TRUE)
five_minutes_average_weekend <- aggregate(steps~interval, data=weekend_frame, FUN=mean, na.rm=TRUE)

plot(x = five_minutes_average_weekday$interval, y = five_minutes_average_weekday$steps, type = "l") 
plot(x=five_minutes_average_weekend$interval, y =five_minutes_average_weekend$steps, type="l")

