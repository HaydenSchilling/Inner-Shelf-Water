lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp","measurements","lubridate"), 
       require, character.only = TRUE)

#import OPC data and make spatial
OPC	<- read.csv("data/MNF_2004_Summary_050419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#add column for site
OPC <- OPC %>%
  dplyr::mutate(site = ifelse(between(Lat, -28.7, -28.4), "CapeByron",
                              ifelse(between(Lat, -29.1, -28.8), "EvansHead",
                                     ifelse(between(Lat, -30.5,-29.5),"NorthSolitary", 
                                            ifelse(between(Lat, -32, -31.5), "DiamondHead","error")))))

OPC_points <- OPC
coordinates(OPC_points) <- ~Lon+Lat
proj4string(OPC_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_points <- spTransform(OPC_points, CRS(newproj))

#Import hydrology  data and make spatial
HYD	<- read.csv("data/S0408.csv", header=TRUE, sep=",", dec=".", fill=TRUE, na.strings = c("", " "))
#convert from degrees minutes to decimal degrees
# change the : symbol to a space
HYD$lat = gsub(':', ' ', HYD$Latitude)
HYD$long = gsub(':', ' ', HYD$Longitude)

#drop nas
sum(is.na(HYD$Latitude))#24
sum(is.na(HYD$Longitude))#24
HYD <- HYD %>% drop_na(lat)
HYD <- HYD %>% drop_na(long)

# remove ' E' and ' S'
HYD$lat = gsub('S', '', HYD$lat)
HYD$long = gsub('E', '', HYD$long)

#change to numeric
HYD$lat <- as.character(HYD$lat)
HYD$long <- as.character(HYD$long)

# convert from decimal minutes to decimal degrees
HYD$lat2 = measurements::conv_unit(HYD$lat, from = 'deg_dec_min', to = 'dec_deg')
HYD$long2 = measurements::conv_unit(HYD$long, from = 'deg_dec_min', to = 'dec_deg')

#change lat negative
HYD$lat3 <- -(as.numeric(HYD$lat2))
HYD$long3 <- as.numeric(HYD$long2)

HYD_points <- HYD
coordinates(HYD_points) <- ~long3+lat3
proj4string(HYD_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
HYD_points <- spTransform(HYD_points, CRS(newproj))

#png(filename = "3plots/HYD_on_OPC.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue")
plot(HYD_points, pch='.', add=T)
plot(HYD_points, pch=1, add=T)
text(HYD_points, labels = HYD_points@data$Station, cex = 0.6,col="dark green", pos=1, offset=0.5)
#dev.off()
# Hydrology stations correspond to CTD stations...although there are some small
#differences spatial location with respect to OPC tow.

#using this plot, the following HYD stations are irrelevent:
HYD_filtered <- subset(HYD, !(Station %in% c(39:44, 46:51)))
HYD_filtered <- drop.levels(HYD_filtered)
HYD_filtered_points <- HYD_filtered
coordinates(HYD_filtered_points) <- ~long3+lat3
proj4string(HYD_filtered_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
HYD_filtered_points <- spTransform(HYD_filtered_points, CRS(newproj))

#replot
#png(filename = "3plots/HYD_filtered_on_OPC.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue")
plot(HYD_filtered_points, pch='.', add=T)
plot(HYD_filtered_points, pch=1, add=T)
text(HYD_filtered_points, labels = HYD_filtered_points@data$Station, cex = 0.6,col="dark green", pos=1, offset=0.5)
#dev.off()

#add column to HYD which identifies the corresponding OPC tow location
HYD_filtered$Station <- as.numeric(HYD_filtered$Station)

HYD_filtered <- HYD_filtered %>%
  dplyr::mutate(OPC_site = ifelse(Station %in% c(52:61), "EH",
                                  ifelse(Station %in% c(31:38), "NS",
                                         ifelse(Station %in% c(24:30),"DH", "error"))))
#remove rows with OPC_site = 'error'
HYD_filtered <- subset(HYD_filtered, !(OPC_site == "error"))
HYD_filtered <- drop.levels(HYD_filtered) #note there is some redundacy with this...

#add column to HYD which identifies the start and end points
HYD_filtered <- HYD_filtered %>%
  dplyr::mutate(end_points = ifelse(Station == 52, "EH_start",
                                    ifelse(Station == 61, "EH_end",
                                           ifelse(Station == 31,"NS_start", 
                                                  ifelse(Station == 38, "NS_end",
                                                         ifelse(Station == 24, "DH_start",
                                                                ifelse(Station == 30, "DH_end","not_end")))))))

#make datetime and drop nas
HYD_filtered <- HYD_filtered %>% drop_na(Date) #200 really???? yes. looks like no data there anyway
HYD_filtered <- HYD_filtered %>% drop_na(Time)
HYD_filtered$datetime <- paste(HYD_filtered$Date, HYD_filtered$Time)

#drop NAs from columsn of interest
HYD_filtered <- HYD_filtered %>% drop_na(Oxygen)
HYD_filtered <- HYD_filtered %>% drop_na(Silicate)
HYD_filtered <- HYD_filtered %>% drop_na(Nitrate)
HYD_filtered <- HYD_filtered %>% drop_na(Phosphate)
HYD_filtered <- HYD_filtered %>% drop_na(Depth)

#format variables
HYD_filtered$Oxygen <- as.numeric(as.character(HYD_filtered$Oxygen))
HYD_filtered$Silicate <- as.numeric(as.character(HYD_filtered$Silicate))
HYD_filtered$Nitrate <- as.numeric(as.character(HYD_filtered$Nitrate))
HYD_filtered$Phosphate <- as.numeric(as.character(HYD_filtered$Phosphate))
HYD_filtered$Depth2 <- as.numeric(as.character(HYD_filtered$Depth))
HYD_filtered$Salinity <- as.numeric(as.character(HYD_filtered$Salinity))
HYD_filtered$Station <- as.factor(HYD_filtered$Station)
#HYD_filtered$datetime <- as.POSIXlt(HYD_filtered$datetime, tz = "UTC",format =  "%m/%d/%Y %H:%M", optional=F)
HYD_filtered$lat3 <- round(HYD_filtered$lat3,4)
HYD_filtered$long3 <- round(HYD_filtered$long3,4)
#HYD$test <- year(HYD$datetime2)

#select variables
HYD_filtered_final <- HYD_filtered[,c("Station", "Depth", "Sal_Bot", "Oxy_Bot", 
                                      "Salinity","Oxygen","Silicate","Nitrate","Phosphate",
                                      "CTD.depth","CTD.Sal","CTD.Temp","lat3","long3",
                                      "OPC_site","end_points","datetime")]

#save the processed station by station data
#write.csv(HYD_filtered_final, file="data/HYD cleaned 080419.csv", row.names=FALSE)

#############
#spatial and temporal overlap; this has not been updated, as at 080419
#############
#make 1 row per site
HYD_dates <- HYD_filtered %>% dplyr::group_by(OPC_site, end_points) %>%
summarise(start= first(Date_Time), end = last(Date_Time))
#note this is reversed. all good.

#reverse order in time
HYD_filtered$datetime <- as.POSIXlt(HYD_filtered$datetime, tz = "UTC",format =  "%m/%d/%Y %H:%M", optional=F)
HYD_filteredtwo <- HYD_filtered[order(HYD_filtered$datetime),]
HYD_filteredtwo$datetime <- NULL
HYD_filtered$datetime <- NULL
HYD_dates <- HYD_filteredtwo %>% dplyr::group_by(OPC_site, end_points) %>%
  summarise(start= first(Date_Time), end = last(Date_Time))
###

################ Calculating distances
#aggregate HYD to one row per Station
HYD_aggregated <- HYD_filtered %>% dplyr::group_by(Station, OPC_site) %>%
  summarise(lat= first(lat3), long = first(long3))
HYD_aggregated_points <- HYD_aggregated
coordinates(HYD_aggregated_points) <- ~long+lat
proj4string(HYD_aggregated_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
HYD_aggregated_points <- spTransform(HYD_aggregated_points, CRS(newproj))

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
OPC_lines <- points_to_line(data = OPC, 
                            long = "Lon", 
                            lat = "Lat", 
                            id_field = "site", 
                            sort_field = "datenum")
proj4string(OPC_lines)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_lines <- spTransform(OPC_lines, CRS(newproj))


#add the distances to HYD aggregated points
distance_matrix_temp <- as.data.frame(dist2Line(HYD_aggregated_points, OPC_lines, distfun=distHaversine))
HYD_aggregated_points@data$distance <- distance_matrix_temp$distance
HYD_aggregated_points@data$lineID <- distance_matrix_temp$ID

#isolate the distances
distances <- HYD_aggregated_points@data

#plot the distances
#png(filename = "3plots/HYD_OPC_proximity.png", units = "in", res=300,width=3, height=3)
ggplot(distances, aes(x=OPC_site, y=distance/1000, fill = factor(OPC_site))) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  theme(legend.position = "none")+
  ylab("Proximity (km)")+
  xlab("Nearest OPC tow")
#dev.off()

summary(distances[distances$OPC_site == "DH",]$distance)
summary(distances[distances$OPC_site == "EH",]$distance)
summary(distances[distances$OPC_site == "NS",]$distance)




