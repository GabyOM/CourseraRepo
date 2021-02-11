## Load Libraries
library(ggplot2)
library(RColorBrewer)

## Load data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

str(NEI)
str(SCC)


## 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == “24510”) from 1999 to 2008? Use the base plotting 
## system to make a plot answering this question.

baltimorecity <- subset(NEI, NEI$fips == "24510")

baltimore_totale <- aggregate(Emissions ~ year, baltimorecity, sum)

plot(baltimore_totale$year, baltimore_totale$Emissions, type = "o", 
     main = expression("Total Baltimore" ~ PM[2.5] ~ "Emissions by Year"),
     xlab = "Year", ylab = expression("Total Baltimore "~ PM[2.5] ~ "Emissions"), 
     col = "lightcoral")

dev.copy(png, filename="plot2.png",
         width  = 480,
         height = 480)

dev.off()