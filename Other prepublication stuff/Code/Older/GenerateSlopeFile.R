#####################################################################################################
# R script : Calculate Slope from Bathymetry data
# Author : Peter Yates and Clara Peron
#####################################################################################v


rm(list=ls())

lapply(c("fields", "raster"), require, character.only=T)  

#Bathy data
load(file = paste("GA_bathy.rda"))

# Calculate slope using the terrain () function in {raster}
GBat <- terrain(GA_bathy, opt='slope', unit='degrees', neighbors=8)

# Save the Slope raster in a local directory
#writeRaster(GBat, filename="slope.tif", format="GTiff", overwrite=TRUE)



# --------------------- Open the Slope raster  ----------------------------

G <- raster("slope.tif")

range(values(G), na.rm=T) #0.00000 64.59449, but not much under 10? (look at plot(G))
plot(G)

#Classify slope
m1 <- c(0, 0.5, 1,0.5, 2, 2,  2, 5, 3, 5, 9, 4, 9, 16, 5, 16,70, 6)
rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
rclmat1
GhimikerC <- reclassify(G, rclmat1, include.lowest=TRUE)
proj4string(GhimikerC)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
projection(GhimikerC)
#table(values(GhimikerC))
#plot(table(values(GhimikerC)))
#plot(GhimikerC)

#save(GhimikerC, file = "Slope_Class.rda")
#GhimikerC <- writeRaster(GhimikerC, "Slope_Class.grd",overwrite=TRUE)
GhimikerCBB <- raster("Slope_Class.grd")

#plot(GhimikerCBB, smallplot = c(0.86, 0.9, 0.2, 0.8), col=tim.colors(6), breaks=seq(0,6, 1), lab.breaks=seq(1,7, 1), alpha=0.7)

#png(filename = "Slope Map.png", units = "in", res=300,width=8, height=7)
#plot(GhimikerCBB, col=tim.colors(6), alpha=0.8,smallplot= c())
#dev.off()

#plot with tows
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC	<- read.csv("All_Sites_CleanedData_250119.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
OPC_points <- OPC
coordinates(OPC_points) <- ~long+lat
proj4string(OPC_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
OPC_points <- spTransform(OPC_points, CRS(newproj))

#crop slope
extent <- c(152,155, -32, -28)
GhimikerCBBcrop <- crop(GhimikerCBB, extent)

#png(filename = "Slope Map wth OPC.png", units = "in", res=300,width=5, height=7)
plot(GhimikerCBBcrop, col=tim.colors(6), alpha=0.8,smallplot= c())
plot(OPC_points, col="black", pch='.',add=T)
projection(OPC_points)
projection(GhimikerCBB)
#dev.off()




