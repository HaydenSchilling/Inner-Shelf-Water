#bring in 20sec OPC data
OPC20	<- read.csv("data/All_Sites_CleanedData_250119.csv",
                  header=TRUE, sep=",", dec=".", fill=TRUE)


EH <- subset(OPC20, site == "EvansHead")

plot(EH$depth~ EH$long, col = EH$temp)

install.packages("ContourFunctions")

library(ContourFunctions)

cf_data(EH$long,EH$depth*-1,EH$temp, gg=F, with_lines = T)

#look at the OPC points
#pdf('3plots/OPC_bins.pdf', width=20, height=5)
plot(OPC20[OPC20$site == "EvansHead",]$long, -OPC20[OPC20$site == "EvansHead",]$depth, cex=0.5, pch=20, col="grey",type="o")

ET <- as.section(longitude = EH$long, latitude = EH$lat, salinity = EH$sal, 
              temperature = EH$temp, pressure = EH$depth, station = "X")

plot(ET)
as.section(ET)

EQ <- as.section(ET)
plot(ET, which='temp', xtype='long',
     ztype='image')


points(OPC20[OPC20$site == "EvansHead",]$long, -OPC20[OPC20$site == "EvansHead",]$GA_depth, cex=0.5, pch=20, col="brown",type="o")
points(OPC[OPC$site == "EvansHead",]$Lon, -OPC[OPC$site == "EvansHead",]$Depth, cex=0.5, pch=20, col="black")
arrows(OPC[OPC$site == "EvansHead",]$Lon, -OPC[OPC$site == "EvansHead",]$Depth_min,
       OPC[OPC$site == "EvansHead",]$Lon, -OPC[OPC$site == "EvansHead",]$Depth_max,
       length=0.01, angle=90, code=3)
#dev.off()

