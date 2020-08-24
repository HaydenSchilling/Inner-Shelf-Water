lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "lattice", "ggplot2",
         "fields","rgdal", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma",
         "gridExtra", "maps", "mapdata", "sp", "rgeos"), require, character.only = TRUE)

#read data
df	<- read.csv("1data/IntegratedData_120419.csv", header=TRUE, sep=",", dec=".", fill=TRUE)

#select variables
df <- df %>% select(Lat, Lon, site, site2, Depth, Temp, Salt, NBSSSlope, bSlope,
                    NBSSIntercept, NBSSRsq, Biomass, Abundance, MinESD, MaxESD, D_coast, GA_depth2, fluorescence, 'V', oxygen)

plot(df$NBSSSlope, df$NBSSIntercept, xlim=c(-2, -0.75))
plot(df$bSlope, df$NBSSIntercept, xlim=c(-2, -0.75))
plot(df$bSlope, df$NBSSSlope) #hmm

#go with NBSS slope for now...
summary(df$NBSSSlope)
summary(df$NBSSIntercept)

#colour code by median
df <- df %>%
  dplyr::mutate(group_med = ifelse(NBSSSlope <= median(df$NBSSSlope, na.rm = T) & NBSSIntercept <= median(df$NBSSIntercept, na.rm = T), "red",
                              ifelse(NBSSSlope <= median(df$NBSSSlope, na.rm = T) & NBSSIntercept > median(df$NBSSIntercept, na.rm = T), "yellow",
                                    ifelse(NBSSSlope > median(df$NBSSSlope, na.rm = T) & NBSSIntercept <= median(df$NBSSIntercept, na.rm = T),"blue", 
                                          ifelse(NBSSSlope > median(df$NBSSSlope, na.rm = T) & NBSSIntercept > median(df$NBSSIntercept, na.rm = T),"green", "error")))))




                

df$group_med <- as.factor(df$group_med)
plot(df$NBSSSlope, df$NBSSIntercept, col=df$group_med)


#colour code by mid
midSlope <- (min(df$NBSSSlope, na.rm=T)+max(df$NBSSSlope, na.rm=T))/2
midIntercept <- (min(df$NBSSIntercept, na.rm=T)+max(df$NBSSIntercept, na.rm=T))/2

df <- df %>%
  dplyr::mutate(group_mid = ifelse(NBSSSlope <= midSlope & NBSSIntercept <= midIntercept, "red",
                                   ifelse(NBSSSlope <= midSlope & NBSSIntercept > midIntercept, "yellow",
                                          ifelse(NBSSSlope > midSlope & NBSSIntercept <= midIntercept,"blue", 
                                                 ifelse(NBSSSlope > midSlope & NBSSIntercept > midIntercept,"green", "error")))))

df$group_mid <- as.factor(df$group_mid)
plot(df$NBSSSlope, df$NBSSIntercept, col=df$group_mid)

#lets go with median for now

#now split by transect
palette(c("deepskyblue3", "darkolivegreen3", "firebrick2", "gold2"))
CB <- df[df$site2 == "CB",]
CB <- CB[!is.na(CB$NBSSSlope),]

EH <- df[df$site2 == "EH",]
EH <- EH[!is.na(EH$NBSSSlope),]

NS <- df[df$site2 == "NS",]
NS <- NS[!is.na(NS$NBSSSlope),]

DH <- df[df$site2 == "DH",]
DH <- DH[!is.na(DH$NBSSSlope),]



#cape byron
CB <- CB %>%
  dplyr::mutate(group_med = ifelse(NBSSSlope <= median(CB$NBSSSlope, na.rm = T) & NBSSIntercept <= median(CB$NBSSIntercept, na.rm = T), "red",
                                   ifelse(NBSSSlope <= median(CB$NBSSSlope, na.rm = T) & NBSSIntercept > median(CB$NBSSIntercept, na.rm = T), "yellow",
                                          ifelse(NBSSSlope > median(CB$NBSSSlope, na.rm = T) & NBSSIntercept <= median(CB$NBSSIntercept, na.rm = T),"blue", 
                                                 ifelse(NBSSSlope > median(CB$NBSSSlope, na.rm = T) & NBSSIntercept > median(CB$NBSSIntercept, na.rm = T),"green", "error")))))

CB$group_med <- as.factor(CB$group_med)
plot(CB$NBSSSlope, CB$NBSSIntercept, col=CB$group_med, main = "Cape Byron", pch=16)
#Evans head
EH <- EH %>%
  dplyr::mutate(group_med = ifelse(NBSSSlope <= median(EH$NBSSSlope, na.rm = T) & NBSSIntercept <= median(EH$NBSSIntercept, na.rm = T), "red",
                                   ifelse(NBSSSlope <= median(EH$NBSSSlope, na.rm = T) & NBSSIntercept > median(EH$NBSSIntercept, na.rm = T), "yellow",
                                          ifelse(NBSSSlope > median(EH$NBSSSlope, na.rm = T) & NBSSIntercept <= median(EH$NBSSIntercept, na.rm = T),"blue", 
                                                 ifelse(NBSSSlope > median(EH$NBSSSlope, na.rm = T) & NBSSIntercept > median(EH$NBSSIntercept, na.rm = T),"green", "error")))))

EH$group_med <- as.factor(EH$group_med)
plot(EH$NBSSSlope, EH$NBSSIntercept, col=EH$group_med, main = "Evans Head", pch=16)
#north solitary
NS <- NS %>%
  dplyr::mutate(group_med = ifelse(NBSSSlope <= median(NS$NBSSSlope, na.rm = T) & NBSSIntercept <= median(NS$NBSSIntercept, na.rm = T), "red",
                                   ifelse(NBSSSlope <= median(NS$NBSSSlope, na.rm = T) & NBSSIntercept > median(NS$NBSSIntercept, na.rm = T), "yellow",
                                          ifelse(NBSSSlope > median(NS$NBSSSlope, na.rm = T) & NBSSIntercept <= median(NS$NBSSIntercept, na.rm = T),"blue", 
                                                 ifelse(NBSSSlope > median(NS$NBSSSlope, na.rm = T) & NBSSIntercept > median(NS$NBSSIntercept, na.rm = T),"green", "error")))))

NS$group_med <- as.factor(NS$group_med)
plot(NS$NBSSSlope, NS$NBSSIntercept, col=NS$group_med, main = "North Solitary", pch=16)
table(NS$group_med)
#Diamond head
DH <- DH %>%
  dplyr::mutate(group_med = ifelse(NBSSSlope <= median(DH$NBSSSlope, na.rm = T) & NBSSIntercept <= median(DH$NBSSIntercept, na.rm = T), "red",
                                   ifelse(NBSSSlope <= median(DH$NBSSSlope, na.rm = T) & NBSSIntercept > median(DH$NBSSIntercept, na.rm = T), "yellow",
                                          ifelse(NBSSSlope > median(DH$NBSSSlope, na.rm = T) & NBSSIntercept <= median(DH$NBSSIntercept, na.rm = T),"blue", 
                                                 ifelse(NBSSSlope > median(DH$NBSSSlope, na.rm = T) & NBSSIntercept > median(DH$NBSSIntercept, na.rm = T),"green", "error")))))

DH$group_med <- as.factor(DH$group_med)
plot(DH$NBSSSlope, DH$NBSSIntercept, col=DH$group_med, main = "Diamond Head", pch=16)

#pdf('3plots/slopeIntercept scatterplots.pdf', width = 8, height =6)
par(mfrow=c(2,3))
summary(df$NBSSSlope)
summary(df$NBSSIntercept)
plot(CB$NBSSSlope, CB$NBSSIntercept, col=CB$group_med, main = "Cape Byron", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
plot(EH$NBSSSlope, EH$NBSSIntercept, col=EH$group_med, main = "Evans Head", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
plot(NS$NBSSSlope, NS$NBSSIntercept, col=NS$group_med, main = "North Solitary", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
plot(DH$NBSSSlope, DH$NBSSIntercept, col=DH$group_med, main = "Diamond Head", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
plot(df$NBSSSlope, df$NBSSIntercept, col=df$group_med, main = "All Sites", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
#dev.off()
#palette("default")

#plot on a transect
#pdf('3plots/slopeIntercept Transects_090519.pdf', width = 8, height =10)
par(mfrow=c(4,1))
plot(CB$Lon, -CB$Depth, col=CB$group_med, main = "Cape Byron", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-100,0),cex=2)
plot(EH$Lon, -EH$Depth, col=EH$group_med, main = "Evans Head", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-100,0),cex=2)
plot(NS$Lon, -NS$Depth, col=NS$group_med, main = "North Solitary", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-100,0),cex=2)
plot(DH$Lon, -DH$Depth, col=DH$group_med, main = "Diamond Head", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-100,0),cex=2)
#dev.off()


#plot on a transect
#pdf('3plots/slopeIntercept Transects_090519_bathy.pdf', width = 8, height =10)
par(mfrow=c(4,1))
plot(CB$Lon, -CB$Depth, col=CB$group_med, main = "Cape Byron", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-200,0),cex=2)
lines(CB$Lon, -CB$GA_depth2, col="blue")
plot(EH$Lon, -EH$Depth, col=EH$group_med, main = "Evans Head", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-200,0),cex=2)
lines(EH$Lon, -EH$GA_depth2, col="blue")
plot(NS$Lon, -NS$Depth, col=NS$group_med, main = "North Solitary", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-200,0),cex=2)
lines(NS$Lon, -NS$GA_depth2, col="blue")
plot(DH$Lon, -DH$Depth, col=DH$group_med, main = "Diamond Head", pch=16, xlab = "Longitude", ylab = "Depth", ylim=c(-200,0),cex=2)
lines(DH$Lon, -DH$GA_depth2, col="blue")
#dev.off()

#pdf('3plots/slopeIntercept Transects_090519_bybathy.pdf', width = 8, height =10)
par(mfrow=c(4,1))
plot(CB$GA_depth2, -CB$Depth, col=CB$group_med, main = "Cape Byron", pch=1, xlab = "Bathymetry", ylab = "Depth", ylim=c(-100,0),cex=2)
plot(EH$GA_depth2, -EH$Depth, col=EH$group_med, main = "Evans Head", pch=1, xlab = "Bathymetry", ylab = "Depth", ylim=c(-100,0),cex=2)
plot(NS$GA_depth2, -NS$Depth, col=NS$group_med, main = "North Solitary", pch=1, xlab = "Bathymetry", ylab = "Depth", ylim=c(-100,0),cex=2)
plot(DH$GA_depth2, -DH$Depth, col=DH$group_med, main = "Diamond Head", pch=1, xlab = "Bathymetry", ylab = "Depth", ylim=c(-100,0),cex=2)
#dev.off()

#4)	Can you plot sample Depth (y-axis) * intercept for all data?
pdf('3plots/Intercept_Depth1.pdf', width = 5, height =5)
df_all <- rbind(CB, EH, NS, DH)
plot(df_all$NBSSIntercept, -df_all$Depth, col=df_all$group_med, pch=1,
     xlab = "NBSS Intercept", ylab = "Sample Depth")
dev.off()
#And sample depth x slope for all data (Lower intercept, lower slope at depth)?
pdf('3plots/Slope_Depth.pdf', width = 5, height =5)
df_all <- rbind(CB, EH, NS, DH)
plot(df_all$NBSSSlope, -df_all$Depth, col=df_all$group_med, pch=1,
     xlab = "NBSS Slope", ylab = "Sample Depth")
dev.off()


#what is going on with DH?
DHtest <- DH
DHtest <- DHtest %>% select(Depth,NBSSSlope)
DHtest <- na.omit(DHtest)
summary(DHtest$Depth)

sum(is.na(DH$NBSSSlope))

#T S plots
#filter to <30m
EH2<- EH %>% dplyr::filter(Depth < 30)
CB2<- CB %>% dplyr::filter(Depth < 30)
NS2<- NS %>% dplyr::filter(Depth < 30)
DH2<- DH %>% dplyr::filter(Depth < 30)
df2 <- df %>% dplyr::filter(Depth < 30)

#salinity on x
#pdf('3plots/TS plots.pdf', width = 8, height =6)
par(mfrow=c(3,2))
plot(CB2$Salt, CB2$Temp, col=CB2$group_med, main = "Cape Byron (<30m)", pch=16, xlab = "Salinity", ylab = "Temperature", xlim = c(35.6, 35.71), ylim=c(18, 22))
plot(EH2$Salt, EH2$Temp, col=EH2$group_med, main = "Evans Head (<30m)", pch=16, xlab = "Salinity", ylab = "Temperature", xlim = c(35.6, 35.71), ylim=c(18, 22))
plot(NS2$Salt, NS2$Temp, col=NS2$group_med, main = "North Solitary (<30m)", pch=16,  xlab = "Salinity", ylab = "Temperature", xlim = c(35.6, 35.71), ylim=c(18, 22))
plot(DH2$Salt, DH2$Temp, col=DH2$group_med, main = "Diamond Head (<30m)", pch=16,  xlab = "Salinity", ylab = "Temperature", xlim = c(35.6, 35.71), ylim=c(18, 22))
plot(df2$Salt, df2$Temp, col=df2$group_med, main = "All Sites (<30m)", pch=16, xlab = "Salinity", ylab = "Temperature", xlim = c(35.6, 35.71), ylim=c(18, 22))
#dev.off()


#Temperature on Fluor
#pdf('3plots/Temp_Fluoro plots.pdf', width = 6, height =6)
par(mfrow=c(2,2))
#plot(CB2$fluorescence, CB2$Temp, col=CB2$group_med, main = "Cape Byron (<30m)", pch=16, xlab = "fluorescence", ylab = "Temperature", xlim = c(35.6, 35.71))
plot(EH2$fluorescence, EH2$Temp, col=EH2$group_med, main = "Evans Head (<30m)", pch=16, xlab = "fluorescence", ylab = "Temperature", xlim = c(52.29,  63.21), ylim=c(18, 22))
plot(NS2$fluorescence, NS2$Temp, col=NS2$group_med, main = "North Solitary (<30m)", pch=16,  xlab = "fluorescence", ylab = "Temperature", xlim = c(52.29,  63.21), ylim=c(18, 22))
plot(DH2$fluorescence, DH2$Temp, col=DH2$group_med, main = "Diamond Head (<30m)", pch=16,  xlab = "fluorescence", ylab = "Temperature", xlim = c(52.29,  63.21), ylim=c(18, 22))
plot(df2$fluorescence, df2$Temp, col=df2$group_med, main = "All Sites (<30m)", pch=16, xlab = "fluorescence", ylab = "Temperature", xlim = c(52.29,  63.21), ylim=c(18, 22))
#dev.off()

#Temp on DO
#pdf('3plots/Temp_DO plots.pdf', width = 6, height =6)
par(mfrow=c(2,2))
plot(EH2$oxygen, EH2$Temp, col=EH2$group_med, main = "Evans Head (<30m)", pch=16, xlab = "DO", ylab = "Temperature", xlim = c(207.2,  247.8), ylim=c(18, 22))
plot(NS2$oxygen, NS2$Temp, col=NS2$group_med, main = "North Solitary (<30m)", pch=16,  xlab = "DO", ylab = "Temperature", xlim = c(207.2,  247.8), ylim=c(18, 22))
plot(DH2$oxygen, DH2$Temp, col=DH2$group_med, main = "Diamond Head (<30m)", pch=16,  xlab = "DO", ylab = "Temperature", xlim = c(207.2,  247.8), ylim=c(18, 22))
plot(df2$oxygen, df2$Temp, col=df2$group_med, main = "All Sites (<30m)", pch=16, xlab = "DO", ylab = "Temperature", xlim = c(207.2,  247.8), ylim=c(18, 22))
#dev.off()



pdf('3plots/slopeIntercept scatterplots_bathycont.pdf', width = 8, height =6)
par(mfrow=c(2,3))
summary(df$NBSSSlope)
summary(df$NBSSIntercept)

u_CB <- interpBarnes(CB2$NBSSSlope, CB2$NBSSIntercept, CB2$GA_depth2)
u_EH <- interpBarnes(EH2$NBSSSlope, EH2$NBSSIntercept, EH2$GA_depth2)
u_NS <- interpBarnes(NS2$NBSSSlope,NS2$NBSSIntercept, NS2$GA_depth2)
u_DH <- interpBarnes(DH2$NBSSSlope, DH2$NBSSIntercept, DH2$GA_depth2)
u_all <- interpBarnes(df2$NBSSSlope, df2$NBSSIntercept, df2$GA_depth2)

plot(CB2$NBSSSlope, CB2$NBSSIntercept, col=CB2$group_med, main = "Cape Byron (<30m)", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_CB$xg, u_CB$yg, u_CB$zg, add=T)
plot(EH2$NBSSSlope, EH2$NBSSIntercept, col=EH2$group_med, main = "Evans Head (<30m)", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_EH$xg, u_EH$yg, u_EH$zg, add=T)
plot(NS2$NBSSSlope, NS2$NBSSIntercept, col=NS2$group_med, main = "North Solitary (<30m)", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_NS$xg, u_NS$yg, u_NS$zg, add=T)
plot(DH2$NBSSSlope, DH2$NBSSIntercept, col=DH2$group_med, main = "Diamond Head (<30m)", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_DH$xg, u_DH$yg, u_DH$zg, add=T)
plot(df2$NBSSSlope, df2$NBSSIntercept, col=df2$group_med, main = "All Sites (<30m)", pch=16, xlim = c(-1.9, -.7), ylim = c(0.3, 2.4), xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_all$xg, u_all$yg, u_all$zg, add=T)
dev.off()


pdf('3plots/slopeIntercept scatterplots_bathycont2.pdf', width = 8, height =6)
par(mfrow=c(2,3))
summary(df$NBSSSlope)
summary(df$NBSSIntercept)

u_CB <- interpBarnes(CB2$NBSSSlope, CB2$NBSSIntercept, CB2$GA_depth2)
u_EH <- interpBarnes(EH2$NBSSSlope, EH2$NBSSIntercept, EH2$GA_depth2)
u_NS <- interpBarnes(NS2$NBSSSlope,NS2$NBSSIntercept, NS2$GA_depth2)
u_DH <- interpBarnes(DH2$NBSSSlope, DH2$NBSSIntercept, DH2$GA_depth2)
u_all <- interpBarnes(df2$NBSSSlope, df2$NBSSIntercept, df2$GA_depth2)

plot(CB2$NBSSSlope, CB2$NBSSIntercept, col=CB2$group_med, main = "Cape Byron (<30m)", pch=16, xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_CB$xg, u_CB$yg, u_CB$zg, add=T)
plot(EH2$NBSSSlope, EH2$NBSSIntercept, col=EH2$group_med, main = "Evans Head (<30m)", pch=16, xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_EH$xg, u_EH$yg, u_EH$zg, add=T)
plot(NS2$NBSSSlope, NS2$NBSSIntercept, col=NS2$group_med, main = "North Solitary (<30m)", pch=16, xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_NS$xg, u_NS$yg, u_NS$zg, add=T)
plot(DH2$NBSSSlope, DH2$NBSSIntercept, col=DH2$group_med, main = "Diamond Head (<30m)", pch=16,  xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_DH$xg, u_DH$yg, u_DH$zg, add=T)
plot(df2$NBSSSlope, df2$NBSSIntercept, col=df2$group_med, main = "All Sites (<30m)", pch=16,  xlab = "NBSS Slope", ylab = "NBSS Intercept")
contour(u_all$xg, u_all$yg, u_all$zg, add=T)
dev.off()


pdf('3plots/slopeIntercept scatterplots_bathycol2.pdf', width = 6, height =6)
ggplot(CB2, aes(x=NBSSSlope, y=NBSSIntercept, color=-GA_depth2)) + geom_point(size=3)+scale_color_gradient(low="navy", high="firebrick1", limits=c(-300, 0))+ ggtitle("Cape Byron (<30m)")
ggplot(EH2, aes(x=NBSSSlope, y=NBSSIntercept, color=-GA_depth2)) + geom_point(size=3)+scale_color_gradient(low="navy", high="firebrick1", limits=c(-300, 0))+ ggtitle("Evans Head (<30m)")
ggplot(NS2, aes(x=NBSSSlope, y=NBSSIntercept, color=-GA_depth2)) + geom_point(size=3)+scale_color_gradient(low="navy", high="firebrick1", limits=c(-300, 0))+ ggtitle("North Solitary (<30m)")
ggplot(DH2, aes(x=NBSSSlope, y=NBSSIntercept, color=-GA_depth2)) + geom_point(size=3)+scale_color_gradient(low="navy", high="firebrick1", limits=c(-300, 0))+ ggtitle("Diamond Head (<30m)")
ggplot(df2, aes(x=NBSSSlope, y=NBSSIntercept, color=-GA_depth2)) + geom_point(size=3)+scale_color_gradient(low="navy", high="firebrick1", limits=c(-300, 0))+ ggtitle("All Sites (<30m)")
dev.off()

ggplot(df2, aes(x=NBSSSlope, y=NBSSIntercept, color=-GA_depth2)) +
  geom_point(size=3)+scale_color_gradient(low="navy", high="firebrick1", limits=c(-300, 0))+
  ggtitle("All Sites (<30m)")

