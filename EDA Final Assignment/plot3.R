## Load Libraries
library(ggplot2)
library(RColorBrewer)

## Load data
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

str(NEI)
str(SCC)

## 3. Of the four types of sources indicated by the type 
## (point, nonpoint, onroad, nonroad) variable, which of these four 
## sources have seen decreases in emissions from 1999-2008 for 
## Baltimore City? Which have seen increases in emissions from 
## 1999-2008? Use the ggplot2 plotting system to make a plot answer 
## this question.

baltimorecity <- subset(NEI, NEI$fips == "24510")
baltimore_type <- aggregate(Emissions ~ year + type, baltimorecity, sum)

ggplot(baltimore_type, aes(year, Emissions, col = type)) +
  geom_line() +
  geom_point() +
  ggtitle(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions by Type and Year")) +
  ylab(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions")) +
  xlab("Year") +
  scale_colour_discrete(name = "Type of sources") +
  theme(legend.title = element_text(face = "bold"))

dev.copy(png, filename="plot3.png",
         width  = 480,
         height = 480)

dev.off()