lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp","measurements","lubridate"), 
       require, character.only = TRUE)

#import OPC (and CTD and HYD) data and make spatial
OPC	<- read.csv("data/OPC_HYD 120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
OPC_points <- OPC
coordinates(OPC_points) <- ~Lon+Lat
proj4string(OPC_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
OPC_points <- spTransform(OPC_points, CRS(newproj))

#Import ADCP  data and make spatial
ADP	<- read.csv("data/MNF_SS200408_adcp_ship_trawler.csv", header=TRUE, sep=",", dec=".", fill=TRUE, na.strings = c("", " "))
ADP_points <- ADP
coordinates(ADP_points) <- ~Lon+Lat
proj4string(ADP_points)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
ADP_points <- spTransform(ADP_points, CRS(newproj))

#png(filename = "3plots/ADP_on_OPC.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue")
plot(ADP_points, pch='.', add=T)
plot(ADP_points, pch=1, add=T)
#dev.off()

#Filter by date-time. I want the ADP collected during OPC tows.
#make StartUTC a date:
ADP$time <- as.POSIXlt(ADP$StartUTC, tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
# ADP$year <- year(ADP$time) #just to check it worked
# ADP$month <- month(ADP$time)
# ADP$day <- day(ADP$time)
# ADP$hour <- hour(ADP$time)
# ADP$minutes <- minute(ADP$time)

# Site	          OPC start time	    OPC end time
# Cape Byron      11/09/2004 22:03	  12/09/2004 00:06
# Evans Head	    11/09/2004 00:47	  11/09/2004 02:43
# North Solitary	07/09/2004 11:34	  07/09/2004 14:12
# Diamond Head	  06/09/2004 09:53	  06/09/2004 12:01

#pull out ADP data within these (and including) (modified to be +/- 30 minutes to fill those gaps on the ends)

extract_OPC_ADP <- function(a,b,c,d,e,f,g,h){ADP[ADP$time >= a & ADP$time <= b |
                                                   ADP$time >= c & ADP$time <= d |
                                                   ADP$time >= e & ADP$time <= f |
                                                   ADP$time >= g & ADP$time <= h,]}

EH_start <- as.POSIXlt("11/09/2004 00:47", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
EH_start2 <- EH_start - minutes(30)

EH_end <- as.POSIXlt("11/09/2004 02:43", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
EH_end2 <- EH_end + minutes(30)

NS_start <- as.POSIXlt("07/09/2004 11:34", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
NS_start2 <- NS_start - minutes(30)

NS_end <- as.POSIXlt("07/09/2004 14:12", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
NS_end2 <- NS_end + minutes(15)

DH_start <- as.POSIXlt("06/09/2004 09:53", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
DH_start2 <- DH_start - minutes(30)

DH_end <- as.POSIXlt("06/09/2004 12:01", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
DH_end2 <- DH_end + minutes(30)

CB_start <- as.POSIXlt("11/09/2004 22:03", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
CB_start2 <- CB_start - minutes(30)

CB_end <- as.POSIXlt("12/09/2004 00:06", tz = "UTC", format =  "%d/%m/%Y %H:%M", optional=F)
CB_end2 <- CB_end + minutes(30)

ADP_tows <- extract_OPC_ADP(EH_start2,EH_end2,
                            NS_start2, NS_end2,
                            DH_start2, DH_end2,
                            CB_start2, CB_end2) 

#make spatial
ADP_tows_sp <- ADP_tows
coordinates(ADP_tows_sp) <- ~Lon+Lat
proj4string(ADP_tows_sp)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
ADP_tows_sp <- spTransform(ADP_tows_sp, CRS(newproj))

#replot
#png(filename = "3plots/ADPtows_on_OPC_300419.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue")
plot(ADP_tows_sp, pch='.', add=T)
plot(ADP_tows_sp, pch=1, add=T)
#dev.off()

# Site	          OPC start time	    OPC end time
# Cape Byron      11/09/2004 22:03	  12/09/2004 00:06
# Evans Head	    11/09/2004 00:47	  11/09/2004 02:43
# North Solitary	07/09/2004 11:34	  07/09/2004 14:12
# Diamond Head	  06/09/2004 09:53	  06/09/2004 12:01

#add column to ADP which identifies the corresponding OPC tow location
CB_tow <- ADP_tows[ADP_tows$time >= CB_start2 & ADP_tows$time <= CB_end2,]
CB_tow$OPC_site <- "CB"
EH_tow <- ADP_tows[ADP_tows$time >= EH_start2 & ADP_tows$time <= EH_end2,]
EH_tow$OPC_site <- "EH"
NS_tow <- ADP_tows[ADP_tows$time >= NS_start2 & ADP_tows$time <= NS_end2,]
NS_tow$OPC_site <- "NS"
DH_tow <- ADP_tows[ADP_tows$time >= DH_start2 & ADP_tows$time <= DH_end2,]
DH_tow$OPC_site <- "DH"
ADP_tows2 <- rbind(CB_tow,EH_tow,NS_tow,DH_tow)

#make spatial
ADP_tows2_sp <- ADP_tows2
coordinates(ADP_tows2_sp) <- ~Lon+Lat
proj4string(ADP_tows2_sp)  <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
newproj<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
ADP_tows2_sp <- spTransform(ADP_tows2_sp, CRS(newproj))

#replot by OPC_site
#png(filename = "3plots/ADP_tows_on_OPC_labels_300419.png", units = "in", res=300,width=20, height=20)
plot(OPC_points, pch='.', col="blue")
plot(ADP_tows2_sp, pch='.', add=T)
plot(ADP_tows2_sp, pch=1, add=T)
text(ADP_tows2_sp, labels = ADP_tows2_sp@data$OPC_site, cex = 0.6,col="dark green", pos=1, offset=0.5)
#dev.off()

#drop NAs from columsn of interest
ADP_tows2 <- ADP_tows2 %>% drop_na(Lat)
ADP_tows2 <- ADP_tows2 %>% drop_na(Lon)
ADP_tows2 <- ADP_tows2 %>% drop_na(U)
ADP_tows2 <- ADP_tows2 %>% drop_na(V)
ADP_tows2 <- ADP_tows2 %>% drop_na(Depth)
ADP_tows2 <- ADP_tows2 %>% drop_na(OPC_site)

#select variables
ADP_tows_final <- ADP_tows2[,c("U", "V", "Depth", "OPC_site", "Lat","Lon", "StartUTC")]

#save the processed station by station data
#write.csv(ADP_tows_final, file="data/ADP_tows_final_300419.csv", row.names=FALSE)
