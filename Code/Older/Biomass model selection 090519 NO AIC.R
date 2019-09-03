#Load packages
lapply(c("R.matlab","knitr","RODBC","MASS", 
         "tweedie", "nlme", "statmod","mgcv", 
         "raster", "lattice", "ggplot2",
         "fields","rgdal", "plyr","dplyr",
         "gdata", "gstat","car","visreg","tidyr",
         "plotrix","hexbin", "R.matlab", "ggmap","viridis",
         "weathermetrics", "chron", "RColorBrewer","lattice",
         "ncdf4", "geosphere", "maptools", "sp", "oce", "pracma",
         "gridExtra", "maps", "mapdata", "sp", "rgeos", "stringr"), require, character.only = TRUE)
#Source Highland Statistics code
source("2code/HighstatLibV6.R")

#read data
df	<- read.csv("1data/CleanedData_060519.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
#select shortlisted variables
df2<- df %>% select(Biomass,Temp,fluorescence,GA_depth2,Dbin,site2, Lon, Vbin, Ubin, Salt, Nitrate, oxygen,
                    D_coast, slope)

# without CB and fluorescence NAs (retaining fluorescence) 

#remove CB
df3 <- df2[df2$site!="CB",]
df3 <- drop.levels(df3) #n = 642
#df3 <- na.omit(df2) #173 obs where fluorescence = NA (n = 469)
#omitting NAs from Vbin (n = 225) i.e. Ive lost 244 values.

#possible approach - LEAVE NA rows, dont use AIC. do not do model selection. remove suspect variables to 
#investigate potential effects of colinearity. Does that mean I can include CB in one set of analyses?
#I think it still makes sense to omit CB when considering Fluoro
#then also I can use gamm without troubles


#ALL potential terms!
#               s(Temp)+
#               s(Temp, by=Dbin)+
#               s(Temp, by=site2)+
#               s(fluorescence)+
#               s(fluorescence, by = Dbin)+
#               s(fluorescence, by = site2)+
#               s(GA_depth2)+
#               s(GA_depth2, by = Dbin)+
#               s(GA_depth2, by = site2)+
#               site2+Dbin, Vbin
#               correlation = corGaus(form = ~ Dbin+Lon| site2),


M1 <- gam(Biomass ~ s(Temp) + s(fluorescence) + s(GA_depth2) + site2 + Dbin +Vbin,
          data=df3,family=Gamma(link="log"), method="REML") 
summary(M1)
anova(M1) #site P value is 0.71
table(df3$site2, df3$Vbin)

M2 <- gam(Biomass ~ s(Temp) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
          data=df3,family=Gamma(link="log"), method="REML") 
anova(M2) #site p value is 0.00461. exclude Vbin.
summary(M2)
par(mfrow=c(2,2))
gam.check(M2)
gam.vcomp(M2)

#resid vs fitted
E2 <- residuals(M2, type = "deviance")
F2 <- fitted(M2)
par(mfrow=c(1,1))
plot(F2~E2)

#resid versus stuff in the model
dftemp <- df3[!is.na(df3$fluorescence), ] 
plot(x= dftemp$Temp, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
#mtext(text="Residuals versus included variables",side=3,line=0,outer=TRUE, cex=1.5)
plot(x= dftemp$fluorescence, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$GA_depth2, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
#or...
dftemp$E2 <- E2
MyVar <- c("fluorescence", "GA_depth2", "Temp")
MyxyplotPolygon(dftemp, MyVar, "E2") 

#resid versus stuff NOT in the model
dftemp$E2 <- E2
par(mfrow=c(3,3))
plot(x= dftemp$Vbin, y= E2, ylab = "Deviance Residuals")
plot(x= dftemp$Ubin, y= E2, ylab = "Deviance Residuals")
#mtext(text="Residuals versus included variables",side=3,line=0,outer=TRUE, cex=1.5)
plot(x= dftemp$Lon, y= E2, ylab = "Deviance Residuals", col="black") #!!!!!!!!
abline(0,0, lty = 2) #non-linear patterns
plot(x= dftemp[dftemp$site2 == "EH",]$Lon, y= dftemp[dftemp$site2 == "EH",]$E2, ylab = "Deviance Residuals", col="black") 
abline(0,0, lty = 2) #especially at NS and EH
plot(x= dftemp[dftemp$site2 == "NS",]$Lon, y= dftemp[dftemp$site2 == "NS",]$E2, ylab = "Deviance Residuals", col="black") 
abline(0,0, lty = 2)
plot(x= dftemp$Salt, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$Nitrate, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$oxygen, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$D_coast, y= E2, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= as.factor(dftemp$slope), y= E2, ylab = "Deviance Residuals")
abline(0,0, lty = 2)
#or...
dftemp$E2 <- E2
MyVar <- c("Lon", "Salt", "Nitrate", "oxygen", "D_coast")
MyxyplotPolygon(dftemp, MyVar, "E2") 

#resid vs observed
par(mfrow=c(1,1))
plot(F2~dftemp$Biomass)

#resid vs observed
par(mfrow=c(1,1))
plot(E2~dftemp$Biomass)

#Create parameters to indicate the sign and maginitude of the deviance residuals
myCex <-abs(E2)/max(abs(E2))
myCol <- vector(length = length(E2))
myCol[E2 > 0] <- rgb(46,139,87,100,maxColorValue = 255)
myCol[E2 <= 0] <- rgb(250,128,114,100,maxColorValue = 255)

par(mfrow=c(3,1))
xyplot(Dbin ~ Lon, data=dftemp[dftemp$site2=="EH",], pch=16, col=myCol, cex=3*(myCex)^(1/2), main ="EH")
xyplot(Dbin ~ Lon, data=dftemp[dftemp$site2=="NS",], pch=16, col=myCol, cex=3*(myCex)^(1/2), main="NS")
xyplot(Dbin ~ Lon, data=dftemp[dftemp$site2=="DH",], pch=16, col=myCol, cex=3*(myCex)^(1/2), main="DH")
#some residual patterns especially at NS

# try different smoothing for temp
M2k2 <- gam(Biomass ~ s(Temp, fx=TRUE, k=2) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
          data=df3,family=Gamma(link="log"), method="REML") 
M2k4 <- gam(Biomass ~ s(Temp, fx=TRUE, k=4) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
            data=df3,family=Gamma(link="log"), method="REML") 
M2k6 <- gam(Biomass ~ s(Temp, fx=TRUE, k=6) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
            data=df3,family=Gamma(link="log"), method="REML") 
M2k8 <- gam(Biomass ~ s(Temp, fx=TRUE, k=8) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
            data=df3,family=Gamma(link="log"), method="REML")
M2k10 <- gam(Biomass ~ s(Temp, fx=TRUE, k=10) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
            data=df3,family=Gamma(link="log"), method="REML") 

par(mfrow=c(2,3))
visreg(M2k2, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="k=2")
visreg(M2k4, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="k=4")
visreg(M2k6, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="k=6")
visreg(M2k8, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="k=8")
visreg(M2k10, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="k=10")

#remove F
M2b <- gam(Biomass ~ s(Temp) + s(GA_depth2) + site2 + Dbin,
          data=df3,family=Gamma(link="log"), method="REML") 
visreg(M2b, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="without Flouro")
summary(M2b)

M2c <- gam(Biomass ~ s(Temp) +s(fluorescence) + site2 + Dbin,
           data=df3,family=Gamma(link="log"), method="REML") 
visreg(M2c, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="without GA_depth")
summary(M2c)

M2d <- gam(Biomass ~ s(Temp) +s(fluorescence) + site2 + Dbin + s(Lon),
           data=df3,family=Gamma(link="log"), method="REML") 
visreg(M2d, "Temp",scale="response", line=list(col="black", lwd=1), rug=T, main ="with longitude")
summary(M2d)

##############
##############
#############

#need to include spatial autocorrelation term.
M3 <- gamm(Biomass ~ s(Temp) + s(fluorescence) + s(GA_depth2) + site2 + Dbin,
          data=df3,family=Gamma(link="log"), method="REML", correlation = corGaus(form = ~ Dbin+Lon| site2)) 
#E3 <- residuals(M3$gam, type = "deviance")
E3 <- residuals(M3$lme)
F3 <- fitted(M3$gam)
dftemp$E3 <- E3
myCex3 <-abs(E3)/max(abs(E3))
myCol3 <- vector(length = length(E3))
myCol3[E3 > 0] <- rgb(46,139,87,100,maxColorValue = 255)
myCol3[E3 <= 0] <- rgb(250,128,114,100,maxColorValue = 255)

xyplot(Dbin ~ Lon, data=dftemp[dftemp$site2=="EH",], pch=16, col=myCol3, cex=3*(myCex3)^(1/2), main="EH")
xyplot(Dbin ~ Lon, data=dftemp[dftemp$site2=="NS",], pch=16, col=myCol3, cex=3*(myCex3)^(1/2), main="NS")
xyplot(Dbin ~ Lon, data=dftemp[dftemp$site2=="DH",], pch=16, col=myCol3, cex=3*(myCex3)^(1/2), main="DH")

#resid versus stuff in the model
par(mfrow=c(3,3))
plot(x= dftemp$Vbin, y= E3, ylab = "Deviance Residuals")
plot(x= dftemp$Ubin, y= E3, ylab = "Deviance Residuals")
#mtext(text="Residuals versus included variables",side=3,line=0,outer=TRUE, cex=1.5)
plot(x= dftemp$Lon, y= E3, ylab = "Deviance Residuals", col="black") #!!!!!!!!
abline(0,0, lty = 2) #non-linear patterns
plot(x= dftemp[dftemp$site2 == "EH",]$Lon, y= dftemp[dftemp$site2 == "EH",]$E3, ylab = "Deviance Residuals", col="black") 
abline(0,0, lty = 2) #especially at NS and EH
plot(x= dftemp[dftemp$site2 == "NS",]$Lon, y= dftemp[dftemp$site2 == "NS",]$E3, ylab = "Deviance Residuals", col="black") 
abline(0,0, lty = 2)
plot(x= dftemp$Salt, y= E3, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$Nitrate, y= E3, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$oxygen, y= E3, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= dftemp$D_coast, y= E3, ylab = "Deviance Residuals", col="black")
abline(0,0, lty = 2)
plot(x= as.factor(dftemp$slope), y= E3, ylab = "Deviance Residuals")
abline(0,0, lty = 2)

#work around (https://pbreheny.github.io/visreg/faq.html)
M3$gam$data <- df3

pdf('3plots/Biomass_3sites_gamm150519.pdf', width = 6, height =8)
par(mfrow=c(3,2))
visreg(M3$gam, "Temp")#,scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M3$gam, "fluorescence")#, scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M3$gam, "GA_depth2")#, scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M3$gam, "site2")#, scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M3$gam, "Dbin")#, scale="response", line=list(col="black", lwd=1), rug=F)
dev.off()

par(mfrow=c(3,2))
#visreg(M3$gam, "Temp", scale="linear", line=list(col="black", lwd=1), rug=F)
#visreg(M3$gam, "Temp", line=list(col="black", lwd=1), rug=F,trans=Gamma()$linkinv)
visreg(M3$gam, "Temp", line=list(col="black", lwd=1), rug=F,trans=exp, ylab="Biomass")
visreg(M3$gam, "fluorescence", line=list(col="black", lwd=1), rug=F, trans=exp,ylab="Biomass")
visreg(M3$gam, "GA_depth2", line=list(col="black", lwd=1), rug=F, trans=exp,ylab="Biomass")
visreg(M3$gam, "site2", line=list(col="black", lwd=1), rug=F, trans=exp,ylab="Biomass")
visreg(M3$gam, "Dbin",  line=list(col="black", lwd=1), rug=F, trans=exp,ylab="Biomass")

#####
## Trying more GLM
#####
#make df_E
df_E <- df2[!is.na(df2$fluorescence), ] 
M1<- lm(log(Biomass)~Temp+log(fluorescence)+log(GA_depth2)+Dbin+site2,
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
par(mfrow=c(2,2))
plot(M1)

par(mfrow=c(2,3))
visreg(M1, "fluorescence")
visreg(M1, "Temp")
visreg(M1, "GA_depth2")
visreg(M1, "site2")
visreg(M1, "Dbin")

visreg(M1, "fluorescence", cond=list(Temp=16, site2="DH"), print.cond=T)
visreg(M1, "fluorescence", cond=list(Temp=16, site2="EH"), print.cond=T)
visreg(M1, "fluorescence", cond=list(Temp=16, site2="NS"), print.cond=T)

M2<- lm(Biomass~Temp+fluorescence+GA_depth2+Dbin+site2,
        data=df2)
par(mfrow=c(2,2))
plot(M2)

plot(M1)

#####################
#Boxplots of shit
#####################


#read data
df	<- read.csv("1data/CleanedData_060519.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
#select shortlisted variables
df2<- df %>% select(V, Abundance, NBSSSlope, NBSSIntercept, Biomass,Temp,fluorescence,GA_depth2,Dbin,site2, Lon, Vbin, Ubin, Salt, Nitrate, oxygen,
                    D_coast, slope, Depth)


EH <- df2[df2$site2 == "EH" & df2$Depth < 30,]
NS <- df2[df2$site2 == "NS" & df2$Depth < 30,]
DH <- df2[df2$site2 == "DH" & df2$Depth < 30,]
CB <- df2[df2$site2 == "CB" & df2$Depth < 30,]


EH$bathygroup <- as.factor(cut(EH$GA_depth2, 4))
plot(EH$GA_depth2, col=EH$bathygroup)
NS$bathygroup <- as.factor(cut(NS$GA_depth2,4))
plot(NS$GA_depth2, col=NS$bathygroup)
CB$bathygroup <- as.factor(cut(CB$GA_depth2, 4))
plot(CB$GA_depth2, col=CB$bathygroup)
DH$bathygroup <- as.factor(cut(DH$GA_depth2, 4))
plot(DH$GA_depth2, col=DH$bathygroup)

df3 <- rbind(CB, EH, NS, DH)
df3$bathygroup <- as.factor(df3$bathygroup)
df3<- droplevels(df3)

ggplot(df3, aes(x = bathygroup, y = Biomass)) +
  geom_boxplot(size = .75) +
  facet_grid(df3$site2, margins = TRUE)

pdf('3plots/cross shelf boxplots.pdf', width = 9, height =6)
p1<- ggplot(CB, aes(x = bathygroup, y = Biomass)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = Biomass)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = Biomass)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = Biomass)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = Abundance)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = Abundance)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = Abundance)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = Abundance)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = NBSSSlope)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = NBSSSlope)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = NBSSSlope)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = NBSSSlope)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = NBSSIntercept)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = NBSSIntercept)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = NBSSIntercept)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = NBSSIntercept)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = Temp)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = Temp)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = Temp)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = Temp)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = oxygen)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = oxygen)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = oxygen)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = oxygen)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = Nitrate)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = Nitrate)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = Nitrate)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = Nitrate)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

p1<- ggplot(CB, aes(x = bathygroup, y = fluorescence)) +
  geom_boxplot(size = .75) + ggtitle("CB")
p2<-ggplot(EH, aes(x = bathygroup, y = fluorescence)) +
  geom_boxplot(size = .75)+ ggtitle("EH")
p3<-ggplot(NS, aes(x = bathygroup, y = fluorescence)) +
  geom_boxplot(size = .75)+ ggtitle("NS")
p4<-ggplot(DH, aes(x = bathygroup, y = fluorescence)) +
  geom_boxplot(size = .75)+ ggtitle("dh")
grid.arrange(p1,p2,p3,p4)

dev.off()
