## Load Libraries
library(ggplot2)
library(RColorBrewer)

## Load data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

str(NEI)
str(SCC)

## 1. Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? Using the base plotting system, make a plot 
## showing the total PM2.5 emission from all sources for each of the 
## years 1999, 2002, 2005, and 2008.

# Aggregate by sum the total emissions by year
NEI_totale <- aggregate(Emissions ~ year, NEI, sum)

plot(NEI_totale$year, NEI_totale$Emissions, type = "o", col = "lightcoral", 
     main = expression("Total US "~ PM[2.5]~ "Emissions by Year"), 
     ylab = expression("Total US "~   PM[2.5] ~ "Emissions"), 
     xlab = "Year")
dev.copy(png, filename="plot1.png",
         width  = 480,
         height = 480)

dev.off()