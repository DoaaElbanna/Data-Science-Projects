Introduction
------------

Fine particulate matter (PM2.5) is an air pollutant that is a concern
for people’s health when levels in air are high. The Environmental
Protection Agency (EPA) is tasked with setting national ambient air
quality standards for fine PM and for tracking the emissions of this
pollutant into the atmosphere. Approximatly every 3 years, the EPA
releases its database on emissions of PM2.5. This database is known as
the National Emissions Inventory (NEI).

You can find more information at the [EPA National Emissions Inventory
web
site.](https://www.epa.gov/air-emissions-inventories/national-emissions-inventory-nei)

For each year and for each type of PM source, the NEI records how many
tons of PM2.5 were emitted from that source over the course of the
entire year.The Data that used for this project are for 1999, 2002,
2005, 2008.

—————————————————————————————————
---------------------------------

### Data:

The data for this project are available(as a single zip file).

The Zip file contains two files:

**1-PM2.5 Emissions Data(summarySCC\_PM25.rds):** This file contains a
data frame with all of the PM2.5 emissions data for 1999, 2002, 2005,
and 2008.

#### There are sex variales:

-   **fips:** A five-digit number (represented as a string) indicating
    the U.S. county
-   **SCC:** The name of the source as indicated by a digit string (see
    source code classification table)
-   **Pollutant:** A string indicating the pollutant.
-   **Emissions:** Amount of PM2.5 emitted, in tons.
-   **type:** The type of source (point, non-point, on-road, or non-road)
-   **year:** The year of emissions recorded.

**2-Source Classification Code
Table(Source\_Classification\_Code.rds):**

This table provides a mapping from the SCC digit strings in the
Emissions table to the actual name of the PM2.5 source.The sources are
categorized in a few different ways from more general to more specific.
[Learn more about
SCC](https://ofmpub.epa.gov/sccsearch/docs/SCC-IntroToSCCs.pdf).

—————————————————————————————————
---------------------------------

#### The Goal of this project is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008.

**First Read the Data Files**
```r
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
```
### Part1:

Make a plot showing the total PM2.5 emissions decreased in the United
States for each of the years 1999, 2002, 2005, and 2008.

**Implementation:**
```r
    total_pm <- with(NEI,tapply(NEI$Emissions, NEI$year, sum, na.rm = TRUE))
    years <- unique(NEI$year)
    plot(years, total_pm, pch =19, type = "o", main = expression("Total PM"[2.5]*" Emissions over a years"), 
         xlab = "Year", ylab = expression("Total PM"[2.5]*" Emissions"))
```

------------------------------------------------------------------------

### Part2:

Make a plot showing total emissions from PM2.5 decreased in the
**Baltimore City**, Maryland (**fips**==“24510”) from 1999 to 2008.

**Implementation:**
```r
    baltimore_data <- subset(NEI, fips == "24510")  # Subset baltimore data
    tpm_baltimore <- with(baltimore_data,tapply(baltimore_data$Emissions, baltimore_data$year, sum, na.rm = TRUE))
    barplot(as.table(tpm_baltimore), main = expression("Total PM"[2.5]*" Emissions for Baltimore City"), 
            xlab = "Year", ylab = expression("Total Pm"[2.5]*" Emissions"), col = "#004c4c")
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/04_ExploreNEIDatabaseProject/graphs/Plot2.png)

------------------------------------------------------------------------

### Part3:

In this part of the project, Make a plot to showing from the four types
of sources indicated by the **type** (point, nonpoint, onroad, nonroad)
variable, which of these four sources have seen decreases in emissions
from 1999–2008 for Baltimore City? Which have seen increases in
emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
answer this question.

**Implementation:**
```r
    library(ggplot2)
    ggplot(baltimore_data, aes(factor(baltimore_data$year), baltimore_data$Emissions, fill = baltimore_data$type)) +  
    geom_bar(stat="identity") + facet_grid(.~baltimore_data$type, scales = "free", space = "free")+
      theme_bw() + labs(x="Year", y=expression("Total PM"[2.5]*" Emissions")) + 
      labs(title = expression("Total PM"[2.5]*" Emissions in Baltimore by Source Types")) + 
      guides(fill = guide_legend(title = "Source Types"))+
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_fill_manual(values=c("#3D0F2B","#69527E","#AA6F73", "#444888"))
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/04_ExploreNEIDatabaseProject/graphs/Plot3.png)

From the plot, The nonpoint, onroad, nonroad sources are decresed from
1999-2008 and The point source increased from 1999-2008.

------------------------------------------------------------------------

### Part4:

This plot describe Across the United States, how have emissions from
coal combustion-related sources changed from 1999–2008.

**Implementation:**

```r
    # Subset coal combustion related NEI data
    comb_related <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
    coal_related <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
    coal_combustion <- (comb_related & coal_related)
    combSCC <- SCC[coal_combustion,]$SCC
    combNEI <- NEI[NEI$SCC %in% combSCC,]

    ggplot(combNEI,aes(factor(year),Emissions/10^5)) +
      geom_bar(stat="identity",fill="#326369",width=0.75) +
      theme_bw() +  guides(fill=FALSE) +
      labs(x="Year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
      labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions In US from 1999-2008"))
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/04_ExploreNEIDatabaseProject/graphs/Plot4.png)

From the plot we notice that emissions from coal combustion-related
sources have a slight decrease from 1999–2008.

------------------------------------------------------------------------

### Part5:

In this plot describe how have emissions from motor vehicle sources
changed from 1999–2008 in Baltimore City.

**Implementation:**

```r
    # First subset the motor vehicles
    vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
    vehiclesSCC <- SCC[vehicles,]$SCC
    vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

    # subset for motor vehicles in Baltimore
    baltimoreVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips==24510,]

    ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) +
      geom_bar(stat="identity",fill="#764F70",width=0.75) +
      theme_bw() +  guides(fill=FALSE) +
      labs(x="Year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
      labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore"))
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/04_ExploreNEIDatabaseProject/graphs/Plot5.png)

------------------------------------------------------------------------

### Part6:

In this plot we Compare emissions from motor vehicle sources in
Baltimore City with emissions from motor vehicle sources in Los Angeles
County, California (**fips**==“06037”). And showing which city has seen
greater changes over time in motor vehicle emissions.

**Implementation:**
```r
    vehicles_baltimoreNEI <- vehiclesNEI[vehiclesNEI$fips == 24510,]
    vehicles_baltimoreNEI$city <- "Baltimore City"
    vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
    vehiclesLANEI$city <- "Los Angeles County"
    bothNEI <- rbind(vehicles_baltimoreNEI,vehiclesLANEI)

    ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) +
      geom_bar(stat="identity") +
      facet_grid(scales="free", space="free", .~city) +
      theme_bw() +
      labs(x="Year", y=expression("Total PM"[2.5]*" Emission ")) +
      labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("#a74c65", "#866f8d"))
```
![](https://github.com/DoaaElbanna/Data-Science-Projects/blob/master/04_ExploreNEIDatabaseProject/graphs/Plot6.png)
