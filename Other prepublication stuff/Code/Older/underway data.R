lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp"), 
       require, character.only = TRUE)

#import OPC data
OPC	<- read.csv("1data/MNF_2004_Summary_050419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#make spatial
OPC_points <- OPC
coordinates(OPC_points) <- ~Lon+Lat
proj4string(OPC_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_points <- spTransform(OPC_points, CRS(newproj))


underway	<- read.csv("1data/MNF_SS200408_underway_trawler/MNF_SS200408_underway_trawler.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
underway_D <- underway[!is.na(underway$DEPTH),]
underway_Dsp <- underway_D
coordinates(underway_Dsp) <- ~Longitude+Latitude
proj4string(underway_Dsp)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
underway_Dsp <- spTransform(underway_Dsp, CRS(newproj))

extent<- c(151,155,-32 , -28)
plot(extent(extent), ylab=expression(Latitude*phantom(0)*degree*S),
     xlab=expression(Longitude*phantom(0)*degree*E), col="white",main=" ")
points(underway_Dsp)
points(OPC_points, col="blue", pch='.')
