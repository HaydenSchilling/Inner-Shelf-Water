lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp","oce"), 
       require, character.only = TRUE)

CTD	<- read.csv("../MNF_SS200408_ctd_trawler/MNF_SS200408_ctd_trawler.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#remove non-OPC CTD samples
CTD_filtered <- subset(CTD, !(STATION %in% c(39:44, 46:51)))
CTD_filtered <- drop.levels(CTD_filtered)

#add column to CTD which identifies the corresponding OPC tow location
CTD_filtered <- CTD_filtered %>%
  dplyr::mutate(OPC_site = ifelse(STATION %in% c(52:61), "EH",
                                  ifelse(STATION %in% c(31:38), "NS",
                                         ifelse(STATION %in% c(24:30),"DH", "error"))))

#plot the station samples down the water column
plot(-CTD_filtered[CTD_filtered$OPC_site == "EH",]$PRESSURE~CTD_filtered[CTD_filtered$OPC_site == "EH",]$BOTTOM_LON)
plot(-CTD_filtered[CTD_filtered$OPC_site == "NS",]$PRESSURE~CTD_filtered[CTD_filtered$OPC_site == "NS",]$BOTTOM_LON)
plot(-CTD_filtered[CTD_filtered$OPC_site == "DH",]$PRESSURE~CTD_filtered[CTD_filtered$OPC_site == "DH",]$BOTTOM_LON)

library(oce)
data(section)
GS <- subset(section, 102 <= stationId & stationId <= 124)
GSg <- sectionGrid(GS, p=seq(0, 1600, 25))
plot(GSg, which=c(1,99), map.xlim=c(-85,-(64+13/60)))

s <- handleFlags(section, flags=list(c(1, 3:9)))
ctd <- as.ctd(s[["salinity"]], s[["temperature"]], s[["pressure"]],
              longitude=s[["longitude"]], latitude=s[["latitude"]])
col <- ifelse(s[["longitude"]] > -30, "black", "gray")
plotTS(ctd, col=col, eos="gsw")









look <- section@data$station[[1]]
look
plot(look)
