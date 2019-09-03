lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "lattice", "ggplot2",
         "fields","rgdal", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma",
         "gridExtra", "maps", "mapdata", "sp", "rgeos"), 
       require, character.only = TRUE)

#read main data
OPC_CTD_HYD_ADP	<- read.csv("1data/OPC_CTD_HYD_ADP 120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#read discrepancy data (this is created at the end of 'CTD overlap')
DIS	<- read.csv("1data/OPC_CTD discrepency 120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE, na.strings = c("", " "))
#rename site in OPC
DIS$site2 <- plyr::revalue(DIS$site, c("EvansHead"="EH",
                                           "NorthSolitary"="NS",
                                           "DiamondHead"="DH",
                                           "CapeByron"="CB"))

#merge
OPC_CTD_HYD_ADP_D <- merge(OPC_CTD_HYD_ADP, DIS, by=c("Lon","Depth", "site2"), all.x=TRUE, all.y=TRUE)

###################################################################################
# GA DEPTH
###################################################################################
#Bathy data
load(file = paste("1data/GA_bathy.rda"))

# Convert coords object into a spatial object
coords <- OPC_CTD_HYD_ADP_D[,c("Lon","Lat")]
coordinates(coords) <- ~Lon+Lat

# Add a column with the Depth at those coordinates
OPC_CTD_HYD_ADP_D$GA_depth2 <- raster::extract(GA_bathy, coords)
OPC_CTD_HYD_ADP_D$GA_depth2 <- (OPC_CTD_HYD_ADP_D$GA_depth2)*-1

###################################################################################
# SLOPE
###################################################################################

#Bathy data
GhimikerCBB <- raster("1data/Slope_Class.grd")

# Add a column with the Depth at those coordinates
OPC_CTD_HYD_ADP_D$slope <- raster::extract(GhimikerCBB, coords)
plot(table(OPC_CTD_HYD_ADP_D$slope, useNA='always'))

par(mfrow=c(1,1))
ggplot(OPC_CTD_HYD_ADP_D, aes(x=slope)) +
  geom_histogram(position="identity", alpha=0.4)+
  facet_grid(rows = vars(site))

###################################################################################
# DISTANCE TO COAST
###################################################################################

#get NSW map
nsw <- shapefile("1data/coast/NSWcoast.shp")
plot(nsw)
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
nsw <- spTransform(nsw, CRS(newproj))
proj4string(nsw)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
projection(nsw)
extent <- c(152,155, -32, -28)
nsw <- crop(nsw, extent)

OPC_CTD_HYD_ADP_D_sp <- OPC_CTD_HYD_ADP_D
coordinates(OPC_CTD_HYD_ADP_D_sp) <- ~Lon+Lat
proj4string(OPC_CTD_HYD_ADP_D_sp)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_CTD_HYD_ADP_D_sp <- spTransform(OPC_CTD_HYD_ADP_D_sp, CRS(newproj))

#plot and check projections
plot(nsw)
plot(OPC_CTD_HYD_ADP_D_sp,add=T, col="blue", pch='.')
projection(OPC_CTD_HYD_ADP_D_sp)
projection(nsw)

#calculate distances
distance_matrix_temp <- as.data.frame(dist2Line(OPC_CTD_HYD_ADP_D_sp,
                                                nsw, distfun=distHaversine))
OPC_CTD_HYD_ADP_D_sp@data$D_coast <- distance_matrix_temp$distance

#HERE IS WHERE YOU MAKE THE FINAL DATASET :)
Data <- OPC_CTD_HYD_ADP_D_sp@data
Data$Lon <- OPC_CTD_HYD_ADP_D_sp@coords[,1]
Data$Lat <- OPC_CTD_HYD_ADP_D_sp@coords[,2]

#plot the distances
#png(filename = "3plots/OPC_distShore.png", units = "in", res=300,width=5, height=3)
ggplot(Data, aes(x=site, y=D_coast/1000, fill = factor(site))) + 
  geom_violin()+
  theme(legend.position = "none")+
  ylab("Distance to shore (km)")+
  xlab("")
#dev.off()

#write.csv(Data, file="1data/IntegratedData_120419.csv", row.names=FALSE)















