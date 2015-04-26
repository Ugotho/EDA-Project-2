#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

# Here we find all the SCC categories that have to do with coal
coalSCCV = vector()
for (r in 1:nrow(SCC))  {
    coalBased <- regexpr("[c][o][a][l]", SCC[r,3], ignore.case=TRUE)
    if (coalBased > 0)  {
         coalSCCV <- c(coalSCCV, as.character(SCC$SCC[r]))
    }
}

# Below is the rate limiting step, 
print("*** There are A LOT of emissions recorded - Give it a few minutes ***")

# Finding all the (SCC) coal-based observations within the NEI table
coalNEI <- data.frame()
for (scc in coalSCCV)  {
    currentDF <- NEI[NEI$SCC==scc, c(4,6)]
    coalNEI <- rbind(coalNEI, currentDF)
}

# Here all NEI observations of coal emissions are sorted by year
coalEmissionsV <- c(0, 0, 0, 0)

coalEmissionsV[1] <- sum(coalNEI[coalNEI$year==1999, 1])
coalEmissionsV[2] <- sum(coalNEI[coalNEI$year==2002, 1])
coalEmissionsV[3] <- sum(coalNEI[coalNEI$year==2005, 1])
coalEmissionsV[4] <- sum(coalNEI[coalNEI$year==2008, 1]) 

yearV = c(1999, 2002, 2005, 2008)

outputDF <- data.frame(year=yearV, coalPM25=coalEmissionsV)

with(outputDF, plot(year, coalPM25))
model <- lm(coalPM25 ~ year, outputDF)
abline(model)

dev.copy(png, "plot4.png", width=480, height=480, units="px")
dev.off()