lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma",
         "gridExtra"), 
       require, character.only = TRUE)

#read processed data
ADP	<- read.csv("data/ADP_tows_final_300419.csv", header=TRUE, sep=",", dec=".", fill=TRUE, na.strings = c("", " "))

#add columns to an OCE "CTD" section
#this is the order of what goes into as.section:
#salinity, temperature, pressure, longitude, latitude, station, sectionId
#'variables' are in place of sal and temp (U and V)
#Depth in place of pressue
sites <- c("EH", "DH", "NS", "CB")
variables <- c("U","V")
for(i in sites){
  for(j in variables){
    assign(paste(i, j, sep = "."), as.section(ADP[ADP$OPC_site == print(i),][[j]],
                                              ADP[ADP$OPC_site == print(i),][[j]],
                                              ADP[ADP$OPC_site == print(i),]$Depth,
                                              ADP[ADP$OPC_site == print(i),]$Lon,
                                              ADP[ADP$OPC_site == print(i),]$Lat,
                                              ADP[ADP$OPC_site == print(i),]$StartUTC,
                                              sectionId = print(i)))
  }}

#have a look at the raw data
A <- ggplot(ADP[ADP$OPC_site == "CB",],aes(x=Lon,y=-Depth, size= V)) + 
  geom_point(aes(col=V),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("CB V")
B <- ggplot(ADP[ADP$OPC_site == "EH",],aes(x=Lon,y=-Depth, size= V)) + 
  geom_point(aes(col=V),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("EH V")
C <- ggplot(ADP[ADP$OPC_site == "NS",],aes(x=Lon,y=-Depth, size= V)) + 
  geom_point(aes(col=V),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("NS V")
D <- ggplot(ADP[ADP$OPC_site == "DH",],aes(x=Lon,y=-Depth, size= V)) + 
  geom_point(aes(col=V),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("DH V")
#pdf('3plots/V at all sites.pdf', width=15, height = 10)
grid.arrange(A, B, C, D, nrow = 2)
#dev.off()

E <- ggplot(ADP[ADP$OPC_site == "CB",],aes(x=Lon,y=-Depth, size= U)) + 
  geom_point(aes(col=U),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("CB U")
ef <- ggplot(ADP[ADP$OPC_site == "EH",],aes(x=Lon,y=-Depth, size= U)) + 
  geom_point(aes(col=U),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("EH U")
G <- ggplot(ADP[ADP$OPC_site == "NS",],aes(x=Lon,y=-Depth, size= U)) + 
  geom_point(aes(col=U),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("NS U")
H <- ggplot(ADP[ADP$OPC_site == "DH",],aes(x=Lon,y=-Depth, size= U)) + 
  geom_point(aes(col=U),alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue")+
  ggtitle("DH U")
#pdf('3plots/UV all sites.pdf', width=15, height = 10)
grid.arrange(A, B, C, D, nrow = 2)
grid.arrange(E, ef, G, H, nrow = 2)
#dev.off()

#do some plots of sections
#clear plots
dev.off(dev.list()["RStudioGD"])
# specify sections
sections <- c("CB.U", "CB.V", "EH.U", "EH.V", "NS.U", "NS.V", "DH.U", "DH.V")

#pdf('3plots/adcp plotting test.pdf', width = 10, height = 5)
for(i in sections) {
  temp.sec <- get(i)
  plot(temp.sec, which="salinity", ylim=c(-10, 250), xtype='longitude', 
       ztype='image', zcol=oceColorsChlorophyll(n=20), legend=FALSE)
  points(temp.sec[["longitude"]], temp.sec[["pressure"]], cex=1.5, pch=20, col="blue")
  mtext(print(i), side = 3, cex=1.5, padj=-0.5)
  text(ADP[ADP$OPC_site == print(substring(i, 1,2)),]$Lon,
       ADP[ADP$OPC_site == print(substring(i, 1,2)),]$Depth,
       ADP[ADP$OPC_site == print(substring(i, 1,2)),][[print(substring(i, 4))]], cex=0.7, col="black", pos=3)
  temp.sec <- NULL
}
#dev.off()


#vertical interpolation
for(i in sections) {
  temp.sec <- get(i)
  temp.vertgrid <- sectionGrid(temp.sec, p=1, method="approx")
  assign(paste(i,"g",sep="."), temp.vertgrid)
  temp.sec <- NULL
}

#extract the data from sections and make a fancy xt
gridded_sections <- c("CB.U.g", "CB.V.g", "EH.U.g", "EH.V.g", "NS.U.g", "NS.V.g", "DH.U.g", "DH.V.g")
for(i in gridded_sections) {
  temp.sec <- get(i)
  temp_lon <- round(temp.sec[["longitude"]] ,3)
  temp_depth <- round(temp.sec[["depth"]], 1)
  temp_variable <- round(temp.sec[["salinity"]], 2) #strsplit(i, "[.]")[[1]][2]
  temp_matrixdf <- as.data.frame(cbind(temp_lon,temp_depth, temp_variable))
  temp_matrixdf$temp_variable[temp_matrixdf$temp_variable == 0] <- 100000
  temp_matrix2 <- as.matrix(temp_matrixdf)
  temp_matrix_xt <- xtabs(temp_variable~temp_depth+temp_lon, temp_matrix2, na.action = na.pass)
  temp_matrix_xt <- as.matrix.data.frame(temp_matrix_xt)
  temp_matrix_xt[temp_matrix_xt == 0] <- NA
  temp_matrix_xt[temp_matrix_xt == 100000] <- 0
  assign(paste("M",i,sep="."), temp_matrix_xt)
  #get the unique depths and longs interp function (ie 'x' and 'y' inputs)
  assign(paste("Xdepths",i,sep="."), unique(temp_matrixdf$temp_depth))
  assign(paste("Xlons",i,sep="."), rev(unique(temp_lon)))
}


#Read OPC transect
#import OPC data and make spatial NOTE THIS HAS HAD CTD AND HYD INTERP ADDED :)
#i.e. from HYD interpolation 120419
OPC	<- read.csv("data/OPC_HYD 120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#add column for site
OPC <- OPC %>%
  dplyr::mutate(site = ifelse(between(Lat, -28.7, -28.4), "CapeByron",
                              ifelse(between(Lat, -29.1, -28.8), "EvansHead",
                                     ifelse(between(Lat, -30.5,-29.5),"NorthSolitary", 
                                            ifelse(between(Lat, -32, -31.5), "DiamondHead","error")))))

#bring in 20sec OPC data
OPC20	<- read.csv("data/All_Sites_CleanedData_250119.csv",
                  header=TRUE, sep=",", dec=".", fill=TRUE)

#loop through to get interpolated values
#rename site in OPC
OPC$site2 <- OPC$site2.x
OPC$site2 <- plyr::revalue(OPC$site2, c("EH"="EH",
                                         "NS"="NS",
                                         "DH"="DH",
                                         "CapeByron"="CB"))
#rename site in OPC
OPC20$site2 <- plyr::revalue(OPC20$site, c("EvansHead"="EH",
                                           "NorthSolitary"="NS",
                                           "DiamondHead"="DH",
                                           "CapeByron"="CB"))



#run interpolation
gridded_sections <- c("CB.U.g", "CB.V.g", "EH.U.g", "EH.V.g", "NS.U.g", "NS.V.g", "DH.U.g", "DH.V.g")
for(i in gridded_sections) {
  temp.interp <- interp2(sort(get(paste("Xlons", i, sep="."))),
                         sort(get(paste("Xdepths", i, sep="."))),
                         get(paste("M", i, sep=".")),
                         OPC[OPC$site2 == print(substring(i, 1,2)),]$Lon,
                         OPC[OPC$site2 == print(substring(i, 1,2)),]$Depth, method = c("linear"))
  assign(paste("interp",i,sep="."), temp.interp)
  #get output
  temp.output <- assign(paste("F", i, sep="."), as.data.frame(cbind(OPC[OPC$site2 == print(substring(i, 1,2)),]$Lon,
                                                                    OPC[OPC$site2 == print(substring(i, 1,2)),]$Depth, temp.interp)))
  colnames(temp.output)<- c("longitude","depth",print(strsplit(i, "[.]")[[1]][2]))
  assign(paste("F", i, sep="."), temp.output)
  temp.interp <- NULL
  temp.output <- NULL
}

#stick them all together
CB.output <- cbind(F.CB.U.g,
                   F.CB.V.g$V)
CB.output$site <- "CB"
colnames(CB.output) <- c("Lon", "Depth", "U", "V", "site")

EH.output <- cbind(F.EH.U.g,
                   F.EH.V.g$V)
EH.output$site <- "EH"
colnames(EH.output) <- c("Lon", "Depth", "U", "V", "site")

NS.output <- cbind(F.NS.U.g,
                   F.NS.V.g$V)
NS.output$site <- "NS"
colnames(NS.output) <- c("Lon", "Depth", "U", "V", "site")

DH.output <- cbind(F.DH.U.g,
                   F.DH.V.g$V)
DH.output$site <- "DH"
colnames(DH.output) <- c("Lon", "Depth", "U", "V", "site")

final.output <- rbind(CB.output, EH.output, NS.output, DH.output)

#loop through plotting
# specify sections
gridded_sections <- c("CB.U.g", "CB.V.g", "EH.U.g", "EH.V.g", "NS.U.g", "NS.V.g", "DH.U.g", "DH.V.g")

#pdf('3plots/all_ADCP_interp300419Linear.pdf', width = 25)
for(i in gridded_sections) {
  temp.sec <- get(i)
  temp.sec.ungridded <- get(paste(strsplit(i, "[.]")[[1]][1], strsplit(i, "[.]")[[1]][2],sep = "."))
  col.data <- subset(final.output, site == strsplit(i, "[.]")[[1]][1], select = strsplit(i, "[.]")[[1]][2])
  col.data$mycol <- !is.na(col.data[[strsplit(i, "[.]")[[1]][2]]])
  plot(temp.sec, which=print("salinity"), legend = FALSE,
       xlim=c(min(final.output[final.output$site == strsplit(i, "[.]")[[1]][1],]$Lon),
              max(final.output[final.output$site == strsplit(i, "[.]")[[1]][1],]$Lon)),
       ylim=c(0, 2*max(final.output[final.output$site == strsplit(i, "[.]")[[1]][1],]$Depth)),
       xtype='longitude', ztype='image', zcol=oceColorsJet(n=20),
       showBottom = FALSE, grid=FALSE)
  #zlim=c(min(temp.sec[["salinity"]],na.rm = TRUE),max(temp.sec[["salinity"]],na.rm = TRUE)))#,
  #zlim = c(min(final.output[,strsplit(i, "[.]")[[1]][2]],na.rm = TRUE),5*max(final.output[,strsplit(i, "[.]")[[1]][2]],na.rm = TRUE)))
  points(temp.sec.ungridded[["longitude"]], temp.sec.ungridded[["pressure"]], cex=1, pch=20, col="dark gray")
  mtext(i, side = 3, cex=1.5, padj=-0.5)
  points(final.output[final.output$site==strsplit(i, "[.]")[[1]][1],]$Lon, final.output[final.output$site==strsplit(i, "[.]")[[1]][1],]$Depth, cex=1, pch=21, bg=col.data$mycol)
  points(OPC20[OPC20$site == print(strsplit(i, "[.]")[[1]][1]),]$long, OPC20[OPC20$site == print(strsplit(i, "[.]")[[1]][1]),]$GA_depth, cex=0.5, pch=20, col="brown",type="o")
  text(final.output[final.output$site==strsplit(i, "[.]")[[1]][1],]$Lon,
       final.output[final.output$site==strsplit(i, "[.]")[[1]][1],]$Depth,
       round(final.output[final.output$site==strsplit(i, "[.]")[[1]][1],strsplit(i, "[.]")[[1]][2]],2), cex=0.6, col="black", pos=3)
}
#dev.off()

#add new variables to OPC data
final.output$site2 <- final.output$site
OPC_ADP <- merge(OPC, final.output, by=c("Lon","Depth", "site2"), all.x=TRUE, all.y=TRUE)
#write.csv(OPC_ADP, file="data/OPC_CTD_HYD_ADP 300419.csv", row.names=FALSE)

#save workspace
#save.image(file="6workspaces/ADP workspace 120419am.RData")
#load(file="6workspaces/ADP workspace 120419am.RData")

