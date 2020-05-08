Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks.

In this project use data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day. The
data consists of two months of data from an anonymous individual
collected during the months of October and November, 2012 and include
the number of steps taken in 5 minute intervals each day.

### Data Set:

-   Data Set: [Activity monitoring
    Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
    \[52KB\].

The variables included in this dataset are:

-   **steps:** Number of steps taking in a 5-minute interval (missing
    values are coded as NA)

-   **date:** The date on which the measurement was taken in YYYY-MM-DD
    format

-   **interval:** Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

what we are going to do:
------------------------

1- Find the mean total number of steps taken per day.

2- Find What is the average daily activity pattern.

3- Find if there are differences in activity patterns between weekdays
and weekends.

### Loading and preprocessing the data

    activity_data <- read.csv("activity.csv", as.is = TRUE)

    # Remove missing values
    activity <- activity_data[complete.cases(activity_data),]

### Part1: Calculate the mean of total number of steps taken per day

    # First calculate the Total number of steps taken per day
    total_steps <- aggregate(steps ~ date, activity, sum)

    # histogram
    hist(total_steps$steps, main = "Total number of Steps taken per day", xlab = "Steps", col="#684D71")

![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/06_ExploreActivityMonitoringDevices/graphs/Plot1.png)

    # Calculate and report the mean and median of the total number of steps taken per day
    round(mean(total_steps$steps))

    ## [1] 10766

    median(total_steps$steps)

    ## [1] 10765

### Part2: The average daily activity pattern

    # Compute average steps per intervals
    avg_steps <- aggregate(steps ~ interval, activity, mean)

    # Make a time series plot
    plot(avg_steps$interval, avg_steps$steps, type = "l", 
    main = "Average Daily Activity Pattern",xlab = "Time interval", 
    ylab = "Average Steps", col="#5B022C")

![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/06_ExploreActivityMonitoringDevices/graphs/Plot2.png)

    # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    # Get interval index which contain maximum number of steps
    interval_index <- which.max(avg_steps$steps)

    # Get the interval index and the maximum number of steps
    print(paste("The interval with maximum number of steps is",avg_steps[interval_index,]$interval,"And the maximum number of steps is", round(avg_steps[interval_index,]$steps, digits = 1)))

    ## [1] "The interval with maximum number of steps is 835 And the maximum number of steps is 206.2"

### Imputing missing values

    # The total number of missing values in the dataset
    missing_activity_data <- activity_data[!(complete.cases(activity_data)),]
    print(paste("The Total number of missing values ",nrow(missing_activity_data)))

    ## [1] "The Total number of missing values  2304"

    # Devise a strategy for filling in all of the missing values in the dataset.
    for (i in 1:nrow(activity_data)) {
        if(is.na(activity_data$steps[i])) {
            val <- avg_steps$steps[which(avg_steps$interval == activity_data$interval[i])]
            activity_data$steps[i] <- val 
        }
    }

    # Steps per day
    steps_per_day <- aggregate(steps ~ date, activity_data, sum)

    # Histogram
    hist(steps_per_day$steps, main = "Histogram of total number of steps per day", 
    xlab = "Steps per day", col="#684D71")

![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/06_ExploreActivityMonitoringDevices/graphs/Plot3.png)

    # Mean and the Median of total number of steps per daya
    round(mean(steps_per_day$steps))

    ## [1] 10766

    median(steps_per_day$steps)

    ## [1] 10766.19

**We Note the Values of mean and median doesn’t change**

### Part3: Find if there are differences in activity patterns between weekdays and weekends

    # First we create a function that define if the date is a weekday
    week_day <- function(date_val) {
        wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
        if  (!(wd == 'Saturday' || wd == 'Sunday')) {
            x <- 'Weekday'
        } else {
            x <- 'Weekend'
        }
        x
    }

    # Add a new column to activity dataset
    activity_data$day_type <- as.factor(sapply(activity_data$date, week_day))

    #load the ggplot library
    library(ggplot2)

    # Create the aggregated data frame by intervals and day_type
    steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity_data, mean)

    # Create the plot
    ggplot(steps_per_day_impute, aes(interval, steps)) +
        geom_line(stat = "identity", aes(colour = day_type)) +
        theme_gray() +
        facet_grid(day_type ~ ., scales="fixed", space="fixed") +
        labs(x="Interval", y=expression("No of Steps")) +
        ggtitle("Number of steps Per Interval by day type")

![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/06_ExploreActivityMonitoringDevices/graphs/Plot4.png)
