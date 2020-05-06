Introduction
------------

In this project I’m doing exploratory graphs to understanding the data
and its properities and explore if there any patterns in data which is
from the UC Irvine Machine Learning Repository, a popular repository for
machine learning datasets.

**Dataset**: [Electric power
consumption](https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption)
The dataset has 2,075,259 rows and 9 columns\[20Mb\].

**Description**: Measurements of electric power consumption in one
household with a one-minute sampling rate over a period of almost 4
years. Different electrical quantities and some sub-metering values are
available.

The following descriptions of the 9 variables in the dataset are taken
from the [UCI web
site](https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption):

1- **Date**: Date in format dd/mm/yyyy

2-**Time**: time in format hh:mm:ss

3-**Global\_active\_power**: household global minute-averaged active
power (in kilowatt)

4-**Global\_reactive\_power**: household global minute-averaged reactive
power (in kilowatt)

5-**Voltage**: minute-averaged voltage (in volt)

6-**Global\_intensity**: household global minute-averaged current
intensity (in ampere)

7-**Sub\_metering\_1**: energy sub-metering No. 1 (in watt-hour of
active energy). It corresponds to the kitchen, containing mainly a
dishwasher, an oven and a microwave (hot plates are not electric but gas
powered).

8-**Sub\_metering\_2**: energy sub-metering No. 2 (in watt-hour of
active energy). It corresponds to the laundry room, containing a
washing-machine, a tumble-drier, a refrigerator and a light.

9-**Sub\_metering\_3**: energy sub-metering No. 3 (in watt-hour of
active energy). It corresponds to an electric water-heater and an
air-conditioner.

------------------------------------------------------------------------

### Notes must be considering about the data which I used it in this project:

1- Using data from the dates 2007-02-01 and 2007-02-02.

2- In this dataset missing values are coded as ?.

Making Plots
------------

**The goal here is to explore how household energy usage varies over a
2-day period in February, 2007.**

Plot1
-----

    # First read data
    power_data <- data.table::fread(input = "data/household_power_consumption.txt", na.strings = "?")

    # change Date column to Date format
    power_data[, Date := lapply(.SD, as.Date, "%d/%m/%Y"), .SDcols = c("Date")]

    # Filter Dates
    power_data <- power_data[(Date >= "2007-02-01") & (Date <= "2007-02-02")]


    # Plot 1
    hist(power_data[, Global_active_power], main="Global Active Power", 
         xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red")

![](ElectricPowerConsumptionProject_files/figure-markdown_strict/unnamed-chunk-1-1.png)

Plot2
-----

    # First read data
    power_data <- data.table::fread(input = "data/household_power_consumption.txt", na.strings = "?")

    # Prevents Scientific Notation
    power_data[, Global_active_power := lapply(.SD, as.numeric), .SDcols = c("Global_active_power")]

    # Making a POSIXct date capable of being filtered and graphed by time of day
    power_data[, dateTime := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]

    # Filter Dates
    power_data <- power_data[(dateTime >= "2007-02-01") & (dateTime < "2007-02-03")]
    plot(x = power_data[,dateTime],
         y = power_data[,Global_active_power], 
         type = "l", xlab = "", ylab = "Global Active Power KilloWatts")

![](ElectricPowerConsumptionProject_files/figure-markdown_strict/unnamed-chunk-2-1.png)

Plot3
-----

    # First read data
    power_data <- data.table::fread(input = "data/household_power_consumption.txt", na.strings = "?")

    # Prevents Scientific Notation
    power_data[, Global_active_power := lapply(.SD, as.numeric), .SDcols = c("Global_active_power")]

    # Making a POSIXct date capable of being filtered and graphed by time of day
    power_data[, dateTime := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]

    # Filter Dates
    power_data <- power_data[(dateTime >= "2007-02-01") & (dateTime < "2007-02-03")]
    plot(power_data[, dateTime], power_data[, Sub_metering_1], type="l", xlab="", ylab="Energy sub metering")
    lines(power_data[, dateTime], power_data[, Sub_metering_2],col="red")
    lines(power_data[, dateTime], power_data[, Sub_metering_3],col="blue")
    legend("topright"
           , col=c("black","red","blue")
           , c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  ")
           ,lty=c(1,1), lwd=c(1,1))

![](ElectricPowerConsumptionProject_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Plot4
-----

    # First read data
    power_data <- data.table::fread(input = "data/household_power_consumption.txt", na.strings = "?")

    # Prevents Scientific Notation
    power_data[, Global_active_power := lapply(.SD, as.numeric), .SDcols = c("Global_active_power")]

    # Making a POSIXct date capable of being filtered and graphed by time of day
    power_data[, dateTime := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")]

    # Filter Dates
    power_data <- power_data[(dateTime >= "2007-02-01") & (dateTime < "2007-02-03")]
    par(mfrow=c(2,2))

    # Plot1
    plot(x = power_data[,dateTime], y = power_data[,Global_active_power], 
         type = "l", xlab = "", ylab = "Global Active Power")

    # Plot2
    plot(x = power_data[,dateTime], y = power_data[,Voltage], 
         type = "l", xlab = "dateTime", ylab = "Voltage")

    # Plot3

    plot(power_data[, dateTime], power_data[, Sub_metering_1], type="l", xlab="", ylab="Energy sub metering")
    lines(power_data[, dateTime], power_data[, Sub_metering_2],col="red")
    lines(power_data[, dateTime], power_data[, Sub_metering_3],col="blue")
    legend("topright"
           , col=c("black","red","blue")
           , c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  ")
           ,lty=c(1,1), lwd=c(1,1))

    # Plot4
    plot(x = power_data[,dateTime], y = power_data[,Global_reactive_power], 
         type = "l", xlab = "dateTime", ylab = "Voltage")

![](ElectricPowerConsumptionProject_files/figure-markdown_strict/unnamed-chunk-4-1.png)
