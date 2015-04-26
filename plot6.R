#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

library(ggplot2)

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

LosAngelesFips = "06037"
LosAngelesNEI <- NEI[NEI$fips==LosAngelesFips,]

# Below is the rate limiting step, 
print("*** There are A LOT of emissions recorded - Give it a few minutes ***")

# Finding all the (SCC) vehicle-based observations within the BaltimoreNEI table
BaltimoreVehicleNEI <- data.frame()
LosAngelesVehicleNEI <- data.frame()
for (scc in vehicleSCCV)  {
    currentDF <- BaltimoreNEI[BaltimoreNEI$SCC==scc, c(4,6)]
    BaltimoreVehicleNEI <- rbind(BaltimoreVehicleNEI, currentDF)
    
    currentDF <- LosAngelesNEI[LosAngelesNEI$SCC==scc, c(4,6)]
    LosAngelesVehicleNEI <- rbind(LosAngelesVehicleNEI, currentDF)    
}

# Here all NEI observations of vehicle emissions are sorted by year
BaltimoreVehicleEmissionsV <- c(0, 0, 0, 0)
BaltimoreVehicleEmissionsV[1] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==1999, 1])
BaltimoreVehicleEmissionsV[2] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==2002, 1])
BaltimoreVehicleEmissionsV[3] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==2005, 1])
BaltimoreVehicleEmissionsV[4] <- sum(BaltimoreVehicleNEI[BaltimoreVehicleNEI$year==2008, 1]) 

BaltimoreNormEmissionsV <- BaltimoreVehicleEmissionsV / mean(BaltimoreVehicleEmissionsV)

LosAngelesVehicleEmissionsV <- c(0, 0, 0, 0)
LosAngelesVehicleEmissionsV[1] <- sum(LosAngelesVehicleNEI[LosAngelesVehicleNEI$year==1999, 1])
LosAngelesVehicleEmissionsV[2] <- sum(LosAngelesVehicleNEI[LosAngelesVehicleNEI$year==2002, 1])
LosAngelesVehicleEmissionsV[3] <- sum(LosAngelesVehicleNEI[LosAngelesVehicleNEI$year==2005, 1])
LosAngelesVehicleEmissionsV[4] <- sum(LosAngelesVehicleNEI[LosAngelesVehicleNEI$year==2008, 1]) 

LosAngelesNormEmissionsV <- LosAngelesVehicleEmissionsV / mean(LosAngelesVehicleEmissionsV)

yearV = c(1999, 2002, 2005, 2008, 1999, 2002, 2005, 2008)

normVehicleEmissionsV = c(BaltimoreNormEmissionsV, LosAngelesNormEmissionsV)

cityV <- c(rep("Baltimore", 4), rep("Los Angeles", 4))

outputDF <- data.frame(year=yearV, normVehiclePM25=normVehicleEmissionsV, city=cityV)

qplot(year, normVehiclePM25, data=outputDF, color=city, geom=c("point", "smooth"), method="lm")

dev.copy(png, "plot6.png", width=480, height=480, units="px")
dev.off()
