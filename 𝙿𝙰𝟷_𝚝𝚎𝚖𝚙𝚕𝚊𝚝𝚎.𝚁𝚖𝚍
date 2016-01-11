##      PART I: Loading and Preprocessing the data

## Objectives:
## a: Convert the data in column "data" into POSIXct class data, so they are readable as a date
## b: Calculate the number of weekdays
## c: Identify whether a day is a weekday or day in the weekend
## d: Create a final dataframe holding the date, the weekday number, the weekday type, the interval and the number of steps

# Clear the workspace
rm(list=ls())

# Load the raw activity data
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)

# Converting the dates into POSIXct class data, so they are readable as dates
activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

# Calculating the weekday number
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

# Compute the day type (weekend or weekday)
activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                       activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))

# Creating a new data frame with all the attributes
activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)

# Clear the workspace
rm(activity_raw)


##      PART II: Mean total number of steps per day

# Summing up the total number of steps per day (NA value removed)
sum_activity <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)

# Giving new header names
names(sum_activity) <- c("date", "total steps")

# Construcuting a histogram
hist(sum_activity$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Total number of steps taken each day\n(NA removed)")

#Calculating mean and median
mean(sum_activity$total)
median(sum_activity$total)


##      PART III: Differences weekdays vs weekends

# Clear the workspace
rm(sum_activity)

# Calculating the means of steps accross all days for each interval
mean_activity <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

# Giving new header names
names(mean_activity) <- c("interval", "mean")

# Creating a time series plot
plot(mean_activity$interval, 
     mean_activity$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Average number of steps per intervals\n(NA removed)")

# Find the position holding the maximum number of steps
max_pos <- which(mean_activity$mean == max(mean_activity$mean))

# Looking up the value of the interval for this workspace
max_interval <- mean_activity[max_pos, 1]

# Clear the workspace
rm(max_pos, mean_activity)

##      PART IV: Inputing the missing values

# Clear the workspace
rm(max_interval)

# Counting the number of NAs using a boolean function is.na().
NA_count <- sum(is.na(activity$steps))

# Clear the workspace
rm(NA_count)

# Find the NA positions
na_pos <- which(is.na(activity$steps))

# We will mitigate the bias caused by the missing values by 
# replacing the NA values by the mean steps of the non-NA-data
# We will do this by creating a vector meanNonNA_vec holding the n times
# the mean of the non-NA values, with n the number of NA values 
meanNonNA_vector <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))

# Replace the NAs by the means
activity[na_pos, "steps"] <- meanNonNA_vector 

# Clear the workspace
rm(meanNonNA_vector, na_pos)

# Compute the total number of steps each day (NA values removed)
sum_activity <- aggregate(activity$steps, by=list(activity$date), FUN=sum)

# Giving new header names
names(sum_activity) <- c("date", "total")

# Creating the histogram of the total number of steps each day
hist(sum_activity$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Total number of steps taken each day\n(NA replaced by mean value)")


##      PART V: DIFFERENCES WEEKDAYS AND WEEKEND DAYS
##      Note: We already included an attribute "day type" in PART I of the exercise

# Clear the workspace
rm(sum_activity)

# Loading the lattice graphical library
library(lattice)

# Calculate the average number of steps taken, averaged across all daytype variable
mean_activity <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

# Giving new header neames
names(mean_activity) <- c("daytype", "weekday", "interval", "mean")

# Creating a time series plot for each day tpe
xyplot(mean ~ interval | daytype, mean_activity, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))

# Clear the workspace
rm(mean_activity)
