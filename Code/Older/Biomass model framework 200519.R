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

M1<- lm(Biomass~Temp+fluorescence+GA_depth2+Dbin+site2,
         data=df2)
summary(M1)
E1 <- resid(M1, type = "pearson")
df_E$E1 <- E1

F1<-fitted(M1)
par(mfrow=c(1,2), mar=c(5,4,3,3))
hist(E1, breaks=25, main="")
plot(x=F1, y=E1)
abline(0,0, lty = 2, col="blue")

MyVar <- c("Temp", "fluorescence", "GA_depth2", "Lon")
MyxyplotPolygon(df_E, MyVar, "E1") 

par(mfrow=c(1,2))
boxplot(E1~site2,data=df_E, ylab="E1")
abline(h = 0, lty = 2, col="blue")
boxplot(E1~Dbin,data=df_E, ylab="E1")
abline(h = 0, lty = 2, col="blue")


#try GLM with Gamma
M2<- glm(Biomass~Temp+log10(fluorescence)+log10(GA_depth2)+Dbin+site2+Lon,
         data=df2,family=Gamma(link="log"))
print(summary(M2), signif.stars=F, digits =2)
E2 <- rstandard(M2) #page 15 &37-38 GAM book
par(mfrow=c(2,2))
plot(M2)

#plot(Variogram(M2,resType="normalized",form=~GA_depth2+Lon), data=df3)

#Overdispersion
N <- nrow(df2)
p <- length(coef(M2))
Dispersion <- sum(E2^2) / (N - p)
Dispersion # 0.5292353

df_E$E2 <- E2
MyxyplotPolygon(df_E, MyVar, "E2") 
# plot(df3$GA_depth2, E2)
# abline(h = 0, lty = 2, col="blue")

par(mfrow=c(1,2))
boxplot(E2~site2,data=df_E, ylab="E2")
abline(h = 0, lty = 2, col="blue")
boxplot(E2~Dbin,data=df_E, ylab="E2")
abline(h = 0, lty = 2, col="blue")

#outliers
par(mfrow=c(1,1))
plot(cooks.distance(M2), type="h", ylim=c(0,1))
abline(h=1, lty=2, col="blue")

F2<-fitted(M2)
par(mfrow=c(1,2), mar=c(5,4,3,3))
hist(E2, breaks=25, main="")
plot(x=F2, y=E2)
abline(0,0, lty = 2, col="blue")

#normality
par(mfrow=c(1,2))
hist(E2)
qqnorm(E2)
qqline(E2)

par(mfrow=c(2,2))
gam.check(M2)

#try a cubic polynomial
#NA's not allowed...
df3 <- df2[!is.na(df2$fluorescence), ] 
M3 <- glm(Biomass~poly(Temp,3)+poly(fluorescence,3)+poly(GA_depth2,3)+Dbin+site2,
               data=df3,family=Gamma(link="log"))
E3 <- rstandard(M3)
df3$E3 <- E3
MyxyplotPolygon(df3, MyVar, "E3")
summary(M3)

#Fit smoothers to check
GAM_E3_Temp <- gam(E3 ~ s(Temp),data = df3)
summary(GAM_E3_Temp)#sig
plot(GAM_E3_Temp)
abline(h = 0, lty = 2, col="blue")
points(x = df3$Temp,
       y = E3,
       pch = 16,
       cex = 0.5)
GAM_E3_GA_depth2 <- gam(E3 ~ s(GA_depth2),data = df3)
summary(GAM_E3_GA_depth2)#sig
plot(GAM_E3_GA_depth2)
abline(h = 0, lty = 2, col="blue")
points(x = df3$GA_depth2,
       y = E3,
       pch = 16,
       cex = 0.5)
GAM_E3_fluorescence <- gam(E3 ~ s(fluorescence),data = df3)
summary(GAM_E3_fluorescence)#sig
plot(GAM_E3_fluorescence)
abline(h = 0, lty = 2, col="blue")
points(x = df3$fluorescence,
       y = E3,
       pch = 16,
       cex = 0.5)

##Decision - non-linear patterns are still there.
##move to GAM



# M6<- glm(Biomass~Temp+fluorescence+GA_depth2+site2,
#          data=df3,family=Gamma(link="log"), correlation = corGaus(form = ~ GA_depth2+Lon| site2))
# 
# M7 <- gam(Biomass~s(Temp)+s(fluorescence)+s(GA_depth2)+Dbin+site2,
#           data=df3,family=Gamma(link="log"), method="REML",
#           correlation = corGaus(form = ~ GA_depth2+Lon| site2)) 



# M8<-gamm(Biomass~s(Temp)+s(fluorescence)+s(GA_depth2)+Dbin+site2,
#          data=df2,family=Gamma(link="log"), method="REML",
#          random=NULL,correlation=corGaus(.1,form =Dbin+Lon))


