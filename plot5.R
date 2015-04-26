#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

# Here we find all the SCC categories that have to do with vehicles
vehicleSCCV = vector()
for (r in 1:nrow(SCC))  {
    vehicleBased <- regexpr("[v][e][h][i][c][l][e]", SCC[r,3], ignore.case=TRUE)
    if (vehicleBased > 0)  {
        vehicleSCCV <- c(vehicleSCCV, as.character(SCC$SCC[r]))
    }
}

BaltimoreFips = "24510"
BaltimoreNEI <- NEI[NEI$fips==BaltimoreFips,]

# Below is the rate limiting step, 
print("*** There are A LOT of emissions recorded - Give it a few minutes ***")

# Finding all the (SCC) vehicle-based observations within the BaltimoreNEI table
BaltimoreVehicleNEI <- data.frame()
for (scc in vehicleSCCV)  {
    currentDF <- BaltimoreNEI[BaltimoreNEI$SCC==scc, c(4,6)]
    BaltimoreVehicleNEI <- rbind(BaltimoreVehicleNEI, currentDF)
}

# Here all NEI observations of vehicle emissions are sorted by year
BaltimoreVehicleEmissionsV <- c(0, 0, 0, 0)

BaltimoreVehicleEmissionsV[1] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==1999, 1])
BaltimoreVehicleEmissionsV[2] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==2002, 1])
BaltimoreVehicleEmissionsV[3] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==2005, 1])
BaltimoreVehicleEmissionsV[4] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==2008, 1]) 

yearV = c(1999, 2002, 2005, 2008)

outputDF <- data.frame(year=yearV, BaltimoreVehiclePM25=BaltimoreVehicleEmissionsV)

with(outputDF, plot(year, BaltimoreVehiclePM25))
model <- lm(BaltimoreVehiclePM25 ~ year, outputDF)
abline(model)

dev.copy(png, "plot5.png", width=480, height=480, units="px")
dev.off()