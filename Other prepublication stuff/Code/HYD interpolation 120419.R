lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma"), 
       require, character.only = TRUE)

#load workspace
#load(file="workspace 080419am.RData")

#read processed data
HYD	<- read.csv("data/HYD cleaned 080419.csv", header=TRUE, sep=",", dec=".", fill=TRUE, na.strings = c("", " "))

#add columns to an OCE "CTD" section
#this is the order of what goes into as.section:
    #salinity, temperature, pressure, longitude, latitude, station, sectionId
#'variables' are in place of sal
#Depth in place of pressue
sites <- c("EH", "DH", "NS")
variables <- c("Oxygen","Nitrate", "Phosphate", "Silicate")
for(i in sites){
  for(j in variables){
assign(paste(i, j, sep = "."), as.section(HYD[HYD$OPC_site == print(i),][[j]],
                                            HYD[HYD$OPC_site == print(i),]$CTD.Temp,
                                            HYD[HYD$OPC_site == print(i),]$Depth,
                                            HYD[HYD$OPC_site == print(i),]$long3,
                                            HYD[HYD$OPC_site == print(i),]$lat3,
                                            HYD[HYD$OPC_site == print(i),]$Station,
                                            sectionId = print(i)))
}}


#do some plots of sections
#clear plots
dev.off(dev.list()["RStudioGD"])
# specify sections
sections <- c("EH.Nitrate", "EH.Phosphate", "EH.Silicate", "EH.Oxygen",
              "NS.Nitrate", "NS.Phosphate", "NS.Silicate", "NS.Oxygen",
              "DH.Nitrate", "DH.Phosphate", "DH.Silicate", "DH.Oxygen")

#pdf('3plots/HYD plotting test.pdf', width = 10, height = 5)
for(i in sections) {
    temp.sec <- get(i)
    plot(temp.sec, which="salinity", ylim=c(-10, 250), xtype='longitude', 
         ztype='image', zcol=oceColorsChlorophyll(n=20), legend=FALSE)
    points(temp.sec[["longitude"]], temp.sec[["pressure"]], cex=1.5, pch=20, col="blue")
    mtext(print(i), side = 3, cex=1.5, padj=-0.5)
    text(HYD[HYD$OPC_site == print(substring(i, 1,2)),]$long3,
         HYD[HYD$OPC_site == print(substring(i, 1,2)),]$CTD.depth,
         HYD[HYD$OPC_site == print(substring(i, 1,2)),][[print(substring(i, 4))]], cex=0.7, col="black", pos=3)
    temp.sec <- NULL
  }
#dev.off()


#vertical interpolation
for(i in sections) {
  temp.sec <- get(i)
  temp.vertgrid <- sectionGrid(temp.sec, p=1, method="approx") #was "lm"
  assign(paste(i,"g",sep="."), temp.vertgrid)
  temp.sec <- NULL
}

#extract the data from sections and make a fancy xt
gridded_sections <- c("EH.Nitrate.g", "EH.Phosphate.g", "EH.Silicate.g", "EH.Oxygen.g",
                      "NS.Nitrate.g", "NS.Phosphate.g", "NS.Silicate.g", "NS.Oxygen.g",
                      "DH.Nitrate.g", "DH.Phosphate.g", "DH.Silicate.g", "DH.Oxygen.g")
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
#import OPC data and make spatial NOTE THIS HAS HAD CTD INTERP ADDED :)
#i.e. from CTD interpolation 120419
OPC	<- read.csv("data/OPC_CTD 120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

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
OPC$site2 <- plyr::revalue(OPC$site, c("EvansHead"="EH",
                                       "NorthSolitary"="NS",
                                       "DiamondHead"="DH"))
#rename site in OPC
OPC20$site2 <- plyr::revalue(OPC20$site, c("EvansHead"="EH",
                                           "NorthSolitary"="NS",
                                           "DiamondHead"="DH"))


 
#run interpolation
gridded_sections <- c("EH.Nitrate.g", "EH.Phosphate.g", "EH.Silicate.g", "EH.Oxygen.g",
                      "NS.Nitrate.g", "NS.Phosphate.g", "NS.Silicate.g", "NS.Oxygen.g",
                      "DH.Nitrate.g", "DH.Phosphate.g", "DH.Silicate.g", "DH.Oxygen.g")
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
EH.output <- cbind(F.EH.Nitrate.g,
                   F.EH.Oxygen.g$Oxygen,
                   F.EH.Silicate.g$Silicate,
                   F.EH.Phosphate.g$Phosphate)
EH.output$site <- "EH"
colnames(EH.output) <- c("Lon", "Depth", "Nitrate", "Oxygen", "Silicate", "Phosphate", "site")

NS.output <- cbind(F.NS.Nitrate.g,
                   F.NS.Oxygen.g$Oxygen,
                   F.NS.Silicate.g$Silicate,
                   F.NS.Phosphate.g$Phosphate)
NS.output$site <- "NS"
colnames(NS.output) <- c("Lon", "Depth", "Nitrate", "Oxygen", "Silicate", "Phosphate", "site")

DH.output <- cbind(F.DH.Nitrate.g,
                   F.DH.Oxygen.g$Oxygen,
                   F.DH.Silicate.g$Silicate,
                   F.DH.Phosphate.g$Phosphate)
DH.output$site <- "DH"
colnames(DH.output) <- c("Lon", "Depth", "Nitrate", "Oxygen", "Silicate", "Phosphate", "site")

final.output <- rbind(EH.output, NS.output, DH.output)

#loop through plotting
# specify sections
gridded_sections <- c("EH.Nitrate.g", "EH.Phosphate.g", "EH.Silicate.g", "EH.Oxygen.g",
                      "NS.Nitrate.g", "NS.Phosphate.g", "NS.Silicate.g", "NS.Oxygen.g",
                      "DH.Nitrate.g", "DH.Phosphate.g", "DH.Silicate.g", "DH.Oxygen.g")
#specify variables
variables <- c("temperature", "salinity", "oxygen", "fluorescence")

#make site abbreviated
final.output$site <- plyr::revalue(final.output$site, c("EH"="EvansHead",
                                                         "NS"="NorthSolitary",
                                                         "DH"="DiamondHead"))
final.output$site2 <- plyr::revalue(final.output$site, c("EvansHead"="EH",
                                                        "NorthSolitary"="NS",
                                                        "DiamondHead"="DH"))

final.output[final.output$site == "EvansHead",]$Lon
final.output[final.output$site2 == "EH.sec.g",]$Lon
final.output[final.output$site2 == temp.site,]$Lon
final.output[final.output$site2 == "EH",]$Lon


#pdf('3plots/all_HYD_interp120419.pdf', width = 25)
for(i in gridded_sections) {
    temp.sec <- get(i)
    temp.sec.ungridded <- get(paste(strsplit(i, "[.]")[[1]][1], strsplit(i, "[.]")[[1]][2],sep = "."))
    col.data <- subset(final.output, site2 == strsplit(i, "[.]")[[1]][1], select = strsplit(i, "[.]")[[1]][2])
    col.data$mycol <- !is.na(col.data[[strsplit(i, "[.]")[[1]][2]]])
    plot(temp.sec, which=print("salinity"), legend = FALSE,
         xlim=c(min(final.output[final.output$site2 == strsplit(i, "[.]")[[1]][1],]$Lon),
                max(final.output[final.output$site2 == strsplit(i, "[.]")[[1]][1],]$Lon)),
         ylim=c(0, 2*max(final.output[final.output$site2 == strsplit(i, "[.]")[[1]][1],]$Depth)),
         xtype='longitude', ztype='image', zcol=oceColorsJet(n=20),
         showBottom = FALSE, grid=FALSE)
         #zlim=c(min(temp.sec[["salinity"]],na.rm = TRUE),max(temp.sec[["salinity"]],na.rm = TRUE)))#,
         #zlim = c(min(final.output[,strsplit(i, "[.]")[[1]][2]],na.rm = TRUE),5*max(final.output[,strsplit(i, "[.]")[[1]][2]],na.rm = TRUE)))
    points(temp.sec.ungridded[["longitude"]], temp.sec.ungridded[["pressure"]], cex=1, pch=20, col="dark gray")
    mtext(i, side = 3, cex=1.5, padj=-0.5)
    points(final.output[final.output$site2==strsplit(i, "[.]")[[1]][1],]$Lon, final.output[final.output$site2==strsplit(i, "[.]")[[1]][1],]$Depth, cex=1, pch=21, bg=col.data$mycol)
    points(OPC20[OPC20$site2 == print(strsplit(i, "[.]")[[1]][1]),]$long, OPC20[OPC20$site2 == print(strsplit(i, "[.]")[[1]][1]),]$GA_depth, cex=0.5, pch=20, col="brown",type="o")
    text(final.output[final.output$site2==strsplit(i, "[.]")[[1]][1],]$Lon,
         final.output[final.output$site2==strsplit(i, "[.]")[[1]][1],]$Depth,
         round(final.output[final.output$site2==strsplit(i, "[.]")[[1]][1],strsplit(i, "[.]")[[1]][2]],1), cex=0.6, col="black", pos=3)
  }
#dev.off()

#add new variables to OPC data
#rename site in final.output
#final.output$site <- plyr::revalue(final.output$site, c("EH" = "EvansHead",
#                                           "NS" = "NorthSolitary",
#                                           "DH"= "DiamondHead"))
OPC$site2.y <-NULL
OPC$site2.x <-NULL
OPC_HYD <- merge(OPC, final.output, by=c("Lon","Depth", "site"), all.x=TRUE, all.y=TRUE)
#write.csv(OPC_HYD, file="data/OPC_HYD 120419.csv", row.names=FALSE)


#pdf('3plots/HYD_CTD_oxy_compare120419.pdf', width = 5, height =5)
plot(OPC_HYD$oxygen, OPC_HYD$Oxygen, xlab= "Interpolated CTD", ylab= "Interpolated Hydrology", main="Oxygen")
abline(fit1 <- lm(oxygen ~ Oxygen, data=OPC_HYD), col='red')
legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit1)$adj.r.squared, digits=4)))
#dev.off()


#save workspace
#save.image(file="6workspaces/HYD workspace 120419am.RData")
#load(file="6workspaces/HYD workspace 120419am.RData")

