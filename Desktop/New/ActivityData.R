library (ggplot2)
library(ggplot2)
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')

activityData <- read.csv("file:///Users/janhavivaswani/Desktop/Coursera/activity1.csv",
                         header = TRUE, sep = ",",
                         colClasses=c("numeric", "character", "numeric"))
activityData$date <- as.Date(activityData$date, format = "%Y/%m/%d")
activityData$interval <- as.factor(activityData$interval)

str(activityData)

summary(activityData)
names(activityData)
head(activityData)
NROW(activityData)

##1
activityData <- read.csv("file:///Users/janhavivaswani/Desktop/Coursera/activity1.csv",
                         header = TRUE, sep = ",",
                         colClasses=c("numeric", "character", "numeric"))

StepsPerDay <- aggregate(steps ~ date, activityData, sum) 
colnames(StepsPerDay ) <- c("date","steps")
head(StepsPerDay)

##2
hist(StepsPerDay$steps)

##3
steps_mean   <- mean(StepsPerDay$steps, na.rm=TRUE)
print(steps_mean)
steps_median <- median(StepsPerDay$steps, na.rm=TRUE)
print(steps_median)

##4
activityData <- read.csv("file:///Users/janhavivaswani/Desktop/Coursera/activity1.csv",
                         header = TRUE, sep = ",",
                         colClasses=c("numeric", "character", "numeric"))

StepsPerInterval <- aggregate(activityData$steps, 
                                by = list(interval = activityData$interval),
                                FUN=mean, na.rm=TRUE)
StepsPerInterval$interval <- as.integer(levels(StepsPerInterval$interval)[StepsPerInterval$interval]) ##
colnames(StepsPerInterval) <- c("interval", "steps")

ggplot(StepsPerInterval, aes(x=interval, y=steps)) +   
  geom_line(color="green", size=1) +  
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
  theme_bw()

##5
MaxInterval <- StepsPerInterval[which.max(StepsPerInterval$steps),]
print(MaxInterval)

##6
missing_values <- sum(is.na(activityData$steps))
print(missing_values)

na_fill <- function(data, pervalue) {
  na_index <- which(is.na(data$steps))
  na_replace <- unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  fill_steps <- data$steps
  fill_steps[na_index] <- na_replace
  fill_steps
}
activityData_fill <- data.frame(  
  steps = na_fill(activityData, steps_per_interval),  
  date = activityData$date,  
  interval = activityData$interval)
##all missing values
print(activityData_fill)

##7
StepsPerDay_fill <- aggregate(steps ~ date, activityData_fill, sum)
colnames(StepsPerDay_fill) <- c("date","steps")

ggplot(StepsPerDay_fill, aes(x = steps)) + 
  geom_histogram(fill = "green", binwidth = 1000) + 
  labs(title="Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Frequency") + theme_bw() 

newmean <- mean(StepsPerDay_fill$steps, na.rm = TRUE)
print(newmean)
newmedian <- median(StepsPerDay_fill$steps, na.rm = TRUE)
print(newmedian)

##8
activityData$date <- as.Date(strptime(activityData$date, format="%Y-%m-%d"))
activityData$day <- weekdays(activityData$date)
for (i in 1:nrow(activityData)) {
  if (activityData[i,]$day %in% c("Saturday","Sunday")) {
    activityData[i,]$day<-"weekend"
  }
  else{
    activityData[i,]$day<-"weekday"
  }
}
stepsByDay <- aggregate(activityData$steps ~ activityData$interval + activityData$day, activityData, mean)

names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")