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

#read data
df	<- read.csv("1data/CleanedData_060519.csv", header=TRUE, sep=",", dec=".", fill=TRUE)
#select shortlisted variables
df2<- df %>% select(Biomass,Temp,fluorescence,GA_depth2,Dbin,site2, Lon)

# without CB and fluorescence NAs (retaining fluorescence) 

#remove CB
df3 <- df2[df2$site!="CB",]
df3 <- drop.levels(df3)
df3 <- na.omit(df2) #173 obs where fluorescence = NA

#without interactions
M10 <- gam(Biomass~
             s(Temp)+
             s(fluorescence)+
             s(GA_depth2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML",
           correlation = corGaus(form = ~ Dbin+Lon| site2)) 

summary(M10)

par(mfrow=c(3,2))
visreg(M10, "Temp", scale="linear")
visreg(M10, "fluorescence", scale="linear")
visreg(M10, "GA_depth2", scale="linear")
visreg(M10, "site2", scale="linear")
visreg(M10, "Dbin", scale="linear")

#pdf('3plots/Biomass.pdf', width = 6, height =8)
par(mfrow=c(3,2))
visreg(M10, "Temp", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "fluorescence", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "GA_depth2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "site2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "Dbin", scale="response", line=list(col="black", lwd=1), rug=F)
#dev.off()

#with site interactions
M11 <- gam(Biomass~
             s(Temp, by= site2)+
             s(fluorescence, by= site2)+
             s(GA_depth2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML",
           correlation = corGaus(form = ~ Dbin+Lon| site2))

#pdf('3plots/Biomass_3sites_siteint.pdf', width = 8, height =8)
par(mfrow=c(3,3))
visreg(M11, "Temp",by= "site2",scale="response", rug=F, overlay=T)
visreg(M11, "fluorescence",by= "site2", scale="response", rug=F, overlay=T)
visreg(M11, "Temp",by= "site2",scale="linear", rug=F, overlay=T, main="linear")
visreg(M11, "fluorescence",by= "site2", scale="linear", rug=F, overlay=T, main="linear")

visreg(M11, "GA_depth2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M11, "site2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M11, "Dbin", scale="response", line=list(col="black", lwd=1), rug=F)
#dev.off()

#with Dbin interactions
M12 <- gam(Biomass~
             s(Temp, by= Dbin)+
             s(fluorescence, by= Dbin)+
             s(GA_depth2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML",
           correlation = corGaus(form = ~ Dbin+Lon| site2))

#pdf('3plots/Biomass_3sites_Dbinint.pdf', width = 8, height =8)
par(mfrow=c(3,3))
visreg(M12, "Temp",by= "Dbin",scale="response", rug=F, overlay=T)
visreg(M12, "fluorescence",by= "Dbin", scale="response", rug=F, overlay=T)
visreg(M12, "Temp",by= "Dbin",scale="linear", rug=F, overlay=T, main="linear")
visreg(M12, "fluorescence",by= "Dbin", scale="linear", rug=F, overlay=T, main="linear")

visreg(M12, "GA_depth2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M12, "site2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M12, "Dbin", scale="response", line=list(col="black", lwd=1), rug=F)
#dev.off()

#without interactions and without correlation
M13 <- gam(Biomass~
             s(Temp)+
             s(fluorescence)+
             s(GA_depth2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML") 

summary(M13)

#GAMM with correlation
M14 <- gamm(Biomass~
             s(Temp)+
             s(fluorescence)+
             s(GA_depth2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML") 
summary(M14$gam)
summary(M14$lme)
M14$gam$data <- df #https://pbreheny.github.io/visreg/faq.html

#pdf('3plots/Biomass.pdf', width = 6, height =8)
par(mfrow=c(3,2))
visreg(M14$gam, "Temp", scale="linear", line=list(col="black", lwd=1), rug=F)
visreg(M14$gam, "Temp", line=list(col="black", lwd=1), rug=F,trans=Gamma()$linkinv)
visreg(M14$gam, "Temp", line=list(col="black", lwd=1), rug=F,trans=exp)
visreg(M14$gam, "fluorescence", scale="linear", line=list(col="black", lwd=1), rug=F)
visreg(M14$gam, "GA_depth2", scale="linear", line=list(col="black", lwd=1), rug=F)
visreg(M14$gam, "site2", scale="linear", line=list(col="black", lwd=1), rug=F)
visreg(M14$gam, "Dbin", scale="linear", line=list(col="black", lwd=1), rug=F)
#dev.off()

par(mfrow=c(3,1))
visreg(M14$gam, "Temp", scale="linear", line=list(col="black", lwd=1), rug=F)
visreg(M14$gam, "Temp", line=list(col="black", lwd=1), rug=F,trans=Gamma()$linkinv)
visreg(M14$gam, "Temp", scale="linear", line=list(col="black", lwd=1), rug=F,trans=exp, ylab = "Biomass??")
