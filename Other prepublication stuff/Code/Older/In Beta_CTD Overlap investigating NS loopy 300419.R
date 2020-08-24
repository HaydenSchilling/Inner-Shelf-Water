lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp"), 
       require, character.only = TRUE)

#Import CTD profile data and make spatial
CTD	<- read.csv("1data/MNF_SS200408_ctd_trawler/MNF_SS200408_ctd_trawler.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
CTD_points <- CTD
coordinates(CTD_points) <- ~END_LON+END_LAT
proj4string(CTD_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
CTD_points <- spTransform(CTD_points, CRS(newproj))

#import OPC data
OPC	<- read.csv("1data/MNF_2004_Summary_050419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#make spatial
OPC_points <- OPC
coordinates(OPC_points) <- ~Lon+Lat
proj4string(OPC_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_points <- spTransform(OPC_points, CRS(newproj))

#define extend same as Jase's plot
extent<- c(153.35,153.75,-30.004 , -29.996)


#add column for site
OPC <- OPC %>%
  dplyr::mutate(site = ifelse(between(Lat, -28.7, -28.4), "CapeByron",
                                  ifelse(between(Lat, -29.1, -28.8), "EvansHead",
                                         ifelse(between(Lat, -30.5,-29.5),"NorthSolitary", 
                                                ifelse(between(Lat, -32, -31.5), "DiamondHead","error")))))
OPC <- OPC %>%
  dplyr::mutate(site2 = ifelse(between(Lat, -28.7, -28.4), "1",
                              ifelse(between(Lat, -29.1, -28.8), "2",
                                     ifelse(between(Lat, -30.5,-29.5),"3", 
                                            ifelse(between(Lat, -32, -31.5), "4","error")))))

#plot
OPC$day <- substring(OPC$datestr, 1,2)
plot(extent(extent), ylab=expression(Latitude*phantom(0)*degree*S),
     xlab=expression(Longitude*phantom(0)*degree*E), col="white",main=" ")
points(OPC$Lon, OPC$Lat, col=OPC$day)

plot(OPC$Lon, OPC$Lat, col=OPC$day)
points(OPC[OPC$day=="10",]$Lon, OPC[OPC$day=="10",]$Lat, col="black")
plot(extent(extent), ylab=expression(Latitude*phantom(0)*degree*S),
     xlab=expression(Longitude*phantom(0)*degree*E), col="white",main=" ")
points(OPC[OPC$day=="10",]$Lon, OPC[OPC$day=="10",]$Lat, col="black")

plot(extent(extent), ylab=expression(Latitude*phantom(0)*degree*S),
     xlab=expression(Longitude*phantom(0)*degree*E), col="white",main=" ")
points(OPC[OPC$day=="07",]$Lon, OPC[OPC$day=="07",]$Lat, col="blue")
points(OPC[OPC$day=="10",]$Lon, OPC[OPC$day=="10",]$Lat, col="red")

#make spatial
OPC_points <- OPC
coordinates(OPC_points) <- ~Lon+Lat
proj4string(OPC_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_points <- spTransform(OPC_points, CRS(newproj))







#png(filename = "3plots/CTD_on_OPC.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue")
plot(CTD_points, pch='.', add=T)
plot(CTD_points, pch=1, add=T)
text(CTD_points, labels = CTD_points@data$STATION, cex = 0.6,col="dark green", pos=1, offset=0.5)
#dev.off()



#using this plot, the following CTD stations are irrelevent:
CTD_filtered <- subset(CTD, !(STATION %in% c(39:44, 46:51)))
CTD_filtered <- drop.levels(CTD_filtered)
CTD_filtered_points <- CTD_filtered
coordinates(CTD_filtered_points) <- ~END_LON+END_LAT
proj4string(CTD_filtered_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
CTD_filtered_points <- spTransform(CTD_filtered_points, CRS(newproj))

#replot
#png(filename = "3plots/CTD_filtered_on_OPC.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue", cex= 0.5)
plot(CTD_filtered_points, pch='.', add=T)
plot(CTD_filtered_points, pch=1, add=T)
text(CTD_filtered_points, labels = CTD_filtered_points@data$STATION, cex = 0.6,col="dark green", pos=1, offset=0.5)
#dev.off()

#add column to CTD which identifies the corresponding OPC tow location
CTD_filtered <- CTD_filtered %>%
  dplyr::mutate(OPC_site = ifelse(STATION %in% c(52:61), "EH",
                                  ifelse(STATION %in% c(31:38), "NS",
                                         ifelse(STATION %in% c(24:30),"DH", "error"))))

#add column to CTD which identifies the start and end points
CTD_filtered <- CTD_filtered %>%
  dplyr::mutate(end_points = ifelse(STATION == 52, "EH_start",
                                    ifelse(STATION == 61, "EH_end",
                                           ifelse(STATION == 31,"NS_start", 
                                                  ifelse(STATION == 38, "NS_end",
                                                         ifelse(STATION == 24, "DH_start",
                                                                ifelse(STATION == 30, "DH_end","not_end")))))))
#make 1 row per station
CTD_dates <- CTD_filtered %>% dplyr::group_by(OPC_site, end_points) %>%
  summarise(start= first(START_TIME), end = last(END_TIME))
#this is what I use to evaluate spatial and temporal overlap

################ Calculating distances
#aggregate CTD to one row per station
CTD_aggregated <- CTD_filtered %>% dplyr::group_by(STATION, OPC_site) %>%
  summarise(lat= first(BOTTOM_LAT), long = first(BOTTOM_LON))
CTD_aggregated_points <- CTD_aggregated
coordinates(CTD_aggregated_points) <- ~long+lat
proj4string(CTD_aggregated_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
CTD_aggregated_points <- spTransform(CTD_aggregated_points, CRS(newproj))

#function to convert points to lines
points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

#Make OPC lines
names(OPC)
OPC_lines <- points_to_line(data = OPC, 
                            long = "Lon", 
                            lat = "Lat", 
                            id_field = "site", 
                            sort_field = "datenum")
proj4string(OPC_lines)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_lines <- spTransform(OPC_lines, CRS(newproj))

#add the distances to CTD aggregated points
distance_matrix_temp <- as.data.frame(dist2Line(CTD_aggregated_points, OPC_lines, distfun=distHaversine))
CTD_aggregated_points@data$distance <- distance_matrix_temp$distance
CTD_aggregated_points@data$lineID <- distance_matrix_temp$ID

#isolate the distances
distances <- CTD_aggregated_points@data

#plot the distances
#png(filename = "3plots/CTD_OPC_proximity.png", units = "in", res=300,width=3, height=3)
ggplot(distances, aes(x=OPC_site, y=distance/1000, fill = factor(OPC_site))) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme(legend.position = "none")+
  ylab("Proximity (km)")+
  xlab("Nearest OPC tow")
#dev.off()

summary(distances[distances$OPC_site == "DH",]$distance)
summary(distances[distances$OPC_site == "EH",]$distance)
summary(distances[distances$OPC_site == "NS",]$distance)

##### Look at discrepancy between Seasoar and CTD interps across space and time
#bring in interpolations and OPC samples
OPC_CTD	<- read.csv("1data/OPC_CTD 120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
OPC_CTD_points <- OPC_CTD
coordinates(OPC_CTD_points) <- ~Lon+Lat
proj4string(OPC_CTD_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_CTD_points <- spTransform(OPC_CTD_points, CRS(newproj))

#add the distances to CTD aggregated points
distance_matrix_temp2 <- as.data.frame(dist2Line(OPC_CTD_points, CTD_aggregated_points, distfun=distHaversine))
OPC_CTD_points@data$distance <- distance_matrix_temp2$distance

OPC_CTD_full <- OPC_CTD_points@data
OPC_CTD_full$Lon <- OPC_CTD_points@coords[,1]
OPC_CTD_full$Lat <- OPC_CTD_points@coords[,2]
#OPC_CTD_full <- OPC_CTD_full %>% select(temperature, Temp, salinity, Salt, distance, site, Lon, Depth)
OPC_CTD_full$tempdiff <- OPC_CTD_full$Temp - OPC_CTD_full$temperature
OPC_CTD_full$saldiff <- OPC_CTD_full$Salt - OPC_CTD_full$salinity

OPC_CTD_full <- OPC_CTD_full %>% select(Lon, site, Depth, tempdiff, saldiff, distance)
#rename site
OPC_CTD_full$site2 <- plyr::revalue(OPC_CTD_full$site, c("EvansHead"="EH",
                                           "NorthSolitary"="NS",
                                           "DiamondHead"="DH",
                                           "CapeByron"="CB"))
#write.csv(OPC_CTD_full, file="1data/OPC_CTD discrepency 120419.csv", row.names=FALSE)

#pdf('3plots/distanceVdifference.pdf', width = 5, height =5)
plot(OPC_CTD_full$distance/1000,OPC_CTD_full$tempdiff, ylab= "Temperature diff (Seasoar-CTD)", xlab= "Distance (km)", main="Temperature", xlim = c(0,3))
abline(h=0, col="blue")
#abline(fit1 <- lm(distance~tempdiff, data=OPC_CTD_full), col='red')
#legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit1)$adj.r.squared, digits=4)))

plot(OPC_CTD_full$distance/1000,OPC_CTD_full$saldiff, ylab= "Salinity diff (Seasoar-CTD)", xlab= "Distance (km)", main="Salinity", xlim = c(0,3))
abline(h=0, col="blue")
#abline(fit1 <- lm(saldiff~distance, data=OPC_CTD_full), col='red')
#legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit1)$adj.r.squared, digits=4)))
#dev.off()
names(OPC_CTD)
















