#Load packages
lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "lattice", "ggplot2",
         "fields","rgdal", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma",
         "gridExtra", "maps", "mapdata", "sp", "rgeos"), require, character.only = TRUE)
#Source Highland Statistics code
source("2code/HighstatLibV6.R")

## three sites
#read data
df	<- read.csv("1data/CleanedData_060519.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
df2 <- df[df$site!="CB",]

#make df_E
df_E <- df2[!is.na(df2$fluorescence), ] 

## GLM no log
M1<- glm(Biomass~Temp+fluorescence+GA_depth2+Dbin+site2+Lon,
         data=df2,family=Gamma(link="log"))
summary(M1)
par(mfrow=c(2,2))
plot(M1)
E1 <- resid(M1, type = "pearson")
df_E$E1 <- E1

F1<-fitted(M1)
par(mfrow=c(1,2), mar=c(5,4,3,3))
hist(E1, breaks=25, main="")
plot(x=F1, y=E1)
abline(0,0, lty = 2, col="blue")

MyVar <- c("Temp", "fluorescence", "GA_depth2", "Lon")
MyxyplotPolygon(df_E, MyVar, "E1")


#### GLM with log
M2<- glm(Biomass~Temp+log10(fluorescence)+log10(GA_depth2)+Dbin+site2+Lon,
         data=df2,family=Gamma(link="log"))
summary(M2)
par(mfrow=c(2,2))
plot(M1)
E2 <- resid(M1, type = "pearson")
df_E$E2 <- E2

F1<-fitted(M1)
par(mfrow=c(1,2), mar=c(5,4,3,3))
hist(E2, breaks=25, main="")
plot(x=F1, y=E2)
abline(0,0, lty = 2, col="blue")

MyVar <- c("Temp", "fluorescence", "GA_depth2", "Lon")
MyxyplotPolygon(df_E, MyVar, "E2")

par(mfrow=c(2,3))
visreg(M1, "fluorescence")
visreg(M1, "Temp")
visreg(M1, "GA_depth2")
visreg(M1, "site2")
visreg(M1, "Dbin")
visreg(M1, "Lon")


summary(df2$fluorescence)
summary(log10(df2$fluorescence))
