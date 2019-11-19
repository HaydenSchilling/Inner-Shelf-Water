# you decided to do this the netcdf way because the other way requires putting DO in the place of temperature, for example. I want to get at C, T, D, DO and F

## Hayden had issues with this code 30/9/2018

lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "rgdal", "lattice", "ggplot2",
         "fields","rgdal", "ncdf4", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma"), 
       require, character.only = TRUE)

#load workspace
#load(file="workspace 080419am.RData")

#Locate my files
EH.files  <- dir('data/ncdf/EH', pattern='*.nc', full.names=TRUE)
NS.files  <- dir('data/ncdf/NS', pattern='*.nc', full.names=TRUE)
DH.files  <- dir('data/ncdf/DH', pattern='*.nc', full.names=TRUE)

#make a funtion to read netcdf (by Clark Richards)
netcdf.ctd <- function(file) {
  x <- read.netcdf(file)
  res <- as.ctd(x)
  res <- oceDeleteData(res, 'latitude')
  res <- oceDeleteData(res, 'longitude')
  res <- oceSetMetadata(res, 'waterDepth', res[['waterDepth']])
  res <- oceDeleteData(res, 'waterDepth')
  res <- oceSetMetadata(res, 'startTime', as.POSIXct(res[['time']][1], origin='2004-10-20 00:00:00', tz='UTC'))
  res <- oceDeleteData(res, 'time')
}

#create my CTD sections
EH.ctd <- lapply(EH.files, netcdf.ctd)
EH.sec <- as.section(EH.ctd)

str(EH.ctd[[1]]@data)
plot(EH.ctd[[1]])

NS.ctd <- lapply(NS.files, netcdf.ctd)
NS.sec <- as.section(NS.ctd)

DH.ctd <- lapply(DH.files, netcdf.ctd)
DH.sec <- as.section(DH.ctd)

#do some plots of sections
#clear plots
dev.off(dev.list()["RStudioGD"])
# specify sections
sections <- c("EH.sec", "NS.sec", "DH.sec")
#specify variables
variables <- c("temperature", "salinity", "oxygen", "fluorescence")

pdf('plots/plotting test.pdf', width = 10, height = 5)
for(i in sections) {
  for(j in variables) {
  temp.sec <- get(i)
  plot(temp.sec, which=print(j), ylim=c(-10, 250), xtype='longitude', 
       ztype='image', zcol=oceColorsChlorophyll(n=20))
  points(temp.sec[["longitude"]], temp.sec[["pressure"]], cex=1.5, pch=20, col="blue")
  temp.sec <- NULL
}}
dev.off()

#vertical interpolation
for(i in sections) {
  temp.sec <- get(i)
  temp.vertgrid <- sectionGrid(temp.sec, p=1, method="approx")
  assign(paste(i,"g",sep="."), temp.vertgrid)
  temp.sec <- NULL
}

#extract the data from sections and make a fancy xt
gridded_sections <- c("EH.sec.g", "NS.sec.g", "DH.sec.g")
for(i in gridded_sections) {
  for(j in variables) {
  temp.sec <- get(i)
  temp_lon <- round(temp.sec[["longitude"]] ,3)
  temp_depth <- round(temp.sec[["depth"]], 1)
  temp_variable <- round(temp.sec[[print(j)]], 2)
  temp_matrixdf <- as.data.frame(cbind(temp_lon,temp_depth, temp_variable))
  temp_matrixdf$temp_variable[temp_matrixdf$temp_variable == 0] <- 100000
  temp_matrix2 <- as.matrix(temp_matrixdf)
  temp_matrix_xt <- xtabs(temp_variable~temp_depth+temp_lon, temp_matrix2, na.action = na.pass)
  temp_matrix_xt <- as.matrix.data.frame(temp_matrix_xt)
  temp_matrix_xt[temp_matrix_xt == 0] <- NA
  temp_matrix_xt[temp_matrix_xt == 100000] <- 0
  assign(paste("M",i,j,sep="."), temp_matrix_xt)
  #get the unique depths and longs interp function (ie 'x' and 'y' inputs)
  assign(paste("Xdepths",i,j,sep="."), unique(temp_matrixdf$temp_depth))
  assign(paste("Xlons",i,j,sep="."), rev(unique(temp_lon)))
}}


#Read OPC transect
#import OPC data and make spatial
OPC	<- read.csv("data/MNF_2004_Summary_050419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#add column for site
OPC <- OPC %>%
  dplyr::mutate(site = ifelse(between(Lat, -28.7, -28.4), "CapeByron",
                              ifelse(between(Lat, -29.1, -28.8), "EvansHead",
                                     ifelse(between(Lat, -30.5,-29.5),"NorthSolitary", 
                                            ifelse(between(Lat, -32, -31.5), "DiamondHead","error")))))

#bring in 20sec OPC data
OPC20	<- read.csv("data/All_Sites_CleanedData_250119.csv",
                  header=TRUE, sep=",", dec=".", fill=TRUE)

#look at the OPC points
#pdf('3plots/OPC_bins.pdf', width=20, height=5)
plot(OPC20[OPC20$site == "EvansHead",]$long, -OPC20[OPC20$site == "EvansHead",]$depth, cex=0.5, pch=20, col="grey",type="o")
points(OPC20[OPC20$site == "EvansHead",]$long, -OPC20[OPC20$site == "EvansHead",]$GA_depth, cex=0.5, pch=20, col="brown",type="o")
points(OPC[OPC$site == "EvansHead",]$Lon, -OPC[OPC$site == "EvansHead",]$Depth, cex=0.5, pch=20, col="black")
arrows(OPC[OPC$site == "EvansHead",]$Lon, -OPC[OPC$site == "EvansHead",]$Depth_min,
       OPC[OPC$site == "EvansHead",]$Lon, -OPC[OPC$site == "EvansHead",]$Depth_max,
       length=0.01, angle=90, code=3)
#dev.off()
    
#loop through to get interpolated values
    #rename site in OPC
    OPC$site2 <- plyr::revalue(OPC$site, c("EvansHead"="EH.sec.g",
                                           "NorthSolitary"="NS.sec.g",
                                           "DiamondHead"="DH.sec.g"))
    #rename site in OPC20
    OPC20$site2 <- plyr::revalue(OPC20$site, c("EvansHead"="EH.sec.g",
                                           "NorthSolitary"="NS.sec.g",
                                           "DiamondHead"="DH.sec.g"))
    
    # specify sections
    gridded_sections <- c("EH.sec.g", "NS.sec.g", "DH.sec.g")
    #specify variables
    variables <- c("temperature", "salinity", "oxygen", "fluorescence")    
  
    
    
    for(i in gridded_sections) {
      for(j in variables) {
    temp.interp <- interp2(sort(get(paste("Xlons", i, j, sep="."))),
                       sort(get(paste("Xdepths", i, j, sep="."))),
                       get(paste("M", i, j, sep=".")),
                       OPC[OPC$site2 == print(i),]$Lon,
                       OPC[OPC$site2 == print(i),]$Depth, method = c("linear"))
    assign(paste("interp",i,j,sep="."), temp.interp)
    #get output
    temp.output <- assign(paste("F", i,j, sep="."), as.data.frame(cbind(OPC[OPC$site2 == print(i),]$Lon,
                                 OPC[OPC$site2 == print(i),]$Depth, temp.interp)))
    colnames(temp.output)<- c("longitude","depth",print(j))
    assign(paste("F", i,j, sep="."), temp.output)
    temp.interp <- NULL
    temp.output <- NULL
    }}
    
#stick them all together
EH.output <- cbind(F.EH.sec.g.fluorescence,
                   F.EH.sec.g.oxygen$oxygen,
                   F.EH.sec.g.salinity$salinity,
                   F.EH.sec.g.temperature$temperature)
EH.output$site <- "EvansHead"
colnames(EH.output) <- c("Lon", "Depth", "fluorescence", "oxygen", "salinity", "temperature", "site")
  
NS.output <- cbind(F.NS.sec.g.fluorescence,
                   F.NS.sec.g.oxygen$oxygen,
                   F.NS.sec.g.salinity$salinity,
                   F.NS.sec.g.temperature$temperature)
NS.output$site <- "NorthSolitary"
colnames(NS.output) <- c("Lon", "Depth", "fluorescence", "oxygen", "salinity", "temperature", "site")

DH.output <- cbind(F.DH.sec.g.fluorescence,
                   F.DH.sec.g.oxygen$oxygen,
                   F.DH.sec.g.salinity$salinity,
                   F.DH.sec.g.temperature$temperature)
DH.output$site <- "DiamondHead"
colnames(DH.output) <- c("Lon", "Depth", "fluorescence", "oxygen", "salinity", "temperature", "site")

final.output <- rbind(EH.output, NS.output, DH.output)
final.output$site2 <- plyr::revalue(final.output$site, c("EvansHead"="EH.sec.g",
                                                "NorthSolitary"="NS.sec.g",
                                                "DiamondHead"="DH.sec.g"))
    
#loop through plotting
  # specify sections
  gridded_sections <- c("EH.sec.g", "NS.sec.g", "DH.sec.g")
  #specify variables
  variables <- c("temperature", "salinity", "oxygen", "fluorescence")
  
 # pdf('3plots/all_CTD_interp_approx method.pdf', width = 25)
  for(i in gridded_sections) {
    for(j in variables) {
      temp.sec <- get(i)
      temp.sec.ungridded <- get(substring(i, 1,6))
      col.data <- subset(final.output, site2 == i, select = j)
      col.data$mycol <- !is.na(col.data[[j]])
      plot(temp.sec, which=print(j), legend = FALSE,
           xlim=c(min(final.output[final.output$site2==i,]$Lon),max(final.output[final.output$site2==i,]$Lon)), ylim=c(0, 2*max(final.output[final.output$site2==i,]$Depth)), xtype='longitude', ztype='image', zcol=oceColorsJet(n=20), showBottom = FALSE, grid=FALSE)
      points(temp.sec.ungridded[["longitude"]], temp.sec.ungridded[["pressure"]], cex=1, pch=20, col="dark gray")
      mtext(paste(print(j),print(substring(i, 1,2)), sep = " "), side = 3, cex=1.5, padj=-0.5)
      points(final.output[final.output$site2==i,]$Lon, final.output[final.output$site2==i,]$Depth, cex=1, pch=21, bg=col.data$mycol)
      points(OPC20[OPC20$site2 == print(i),]$long, OPC20[OPC20$site2 == print(i),]$GA_depth, cex=0.5, pch=20, col="brown",type="o")
      text(final.output[final.output$site2==i,]$Lon,
           final.output[final.output$site2==i,]$Depth,
           round(final.output[final.output$site2==i,j],1), cex=0.6, col="black", pos=3)
    }}
#dev.off()

#add new variables to OPC data
OPC_CTD <- merge(OPC, final.output, by=c("Lon","Depth", "site"), all.x=TRUE, all.y=TRUE)
#write.csv(OPC_CTD, file="1data/OPC_CTD 120419.csv", row.names=FALSE)


#pdf('3plots/sal_temp_compare_approxmethod.pdf', width = 5, height =5)
plot(OPC_CTD$temperature, OPC_CTD$Temp, xlab= "Interpolated CTD", ylab= "SeaSoar", main="Temperature")
abline(fit1 <- lm(temperature ~ Temp, data=OPC_CTD), col='red')
legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit1)$adj.r.squared, digits=4)))

plot(OPC_CTD$salinity, OPC_CTD$Salt, xlab= "Interpolated CTD", ylab= "SeaSoar", main="Salinity")
abline(fit2 <- lm(salinity ~ Salt, data=OPC_CTD), col='red')
legend("topleft", bty="n", legend=paste("R2 =", format(summary(fit2)$adj.r.squared, digits=4)))
#dev.off()


#save workspace
#save.image(file="6workspaces/workspace 120419am.RData")
#load(file="6workspaces/workspace 120419am.RData")

