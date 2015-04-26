#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

library(ggplot2)

yearV = c(1999, 2002, 2005, 2008)
typeV = c("POINT", "NONPOINT", "ONROAD", "NONROAD")

yearRow = vector()
typeRow = vector()
totalEmissionsRow = vector()
for (y in yearV)  {
    for (t in typeV)  {
        yearRow <- c(yearRow, y)
        typeRow <- c(typeRow, t)
        totalEmissionsRow <- c(totalEmissionsRow, sum(NEI$Emissions[NEI$year==y 
                                                                & NEI$type==t]))
    }
}

outputDF <- data.frame(year=yearRow, type=typeRow, totalPM25=totalEmissionsRow)

qplot(year, totalPM25, data=outputDF, geom=c("point", "smooth"), method="lm", facets=.~type)

dev.copy(png, "plot3.png", width=480, height=480, units="px")
dev.off()