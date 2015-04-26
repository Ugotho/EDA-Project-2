#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

yearV = c(1999, 2002, 2005, 2008)

BaltimoreFips = "24510"

totalEmissionsV = vector()
for (y in yearV)  {
    totalEmissionsV <- c(totalEmissionsV, sum(NEI$Emissions[NEI$year==y 
                                              & NEI$fips==BaltimoreFips]))
}

outputDF <- data.frame(year=yearV, totalPM25=totalEmissionsV)

with(outputDF, plot(year, totalPM25))
model <- lm(totalPM25 ~ year, outputDF)
abline(model)

dev.copy(png, "plot2.png", width=480, height=480, units="px")
dev.off()