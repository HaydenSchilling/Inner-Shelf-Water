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
df$GeoMn <- df$GeoMn * 1e+06
#select shortlisted variables
df2<- df %>% select(GeoMn,Temp,fluorescence,GA_depth2,Dbin,site2, Lon)


################################################
# without CB and fluorescence NAs (retaining fluorescence) 
################################################
#remove CB
df3 <- df2[df2$site!="CB",]
df3 <- drop.levels(df3)
df3 <- na.omit(df2) #173 obs where fluorescence = NA

#try a basic lm
M1<- lm(GeoMn~Temp+fluorescence+GA_depth2+Dbin+site2,
        data=df3)
E1 <- resid(M1, type = "pearson")
summary(E1)

#Overdispersion
N <- nrow(df3)
p <- length(coef(M1))
Dispersion <- sum(E1^2) / (N - p)
Dispersion # 364.9411

#Why do you have overdispersion
#A. Outliers                  => plot E1 verses all covariates. Remove them..but subjective
#B. Missing covariates        => plot resid verses vars not in the model, boxplot residuals verses bay and round.
#C. Missing interactions      => Add them (coplot)
#D. Zero inflation            => ZIP/ZINB
#E. Dependency                => Check for temporal or spatial correlation, plot resid verses covariates
#F. Non-linear relationships  => plot resid verses covariates, GAM?
#G. Wrong link function       => Change it
#If nothing can be pinpointed....go to a more complicated model

par(mfrow=c(2,2))
plot(df3$Temp, E1)
abline(h = 0, lty = 2, col="blue")
plot(df3$GA_depth2, E1)
abline(h = 0, lty = 2, col="blue")
plot(df3$fluorescence, E1)
abline(h = 0, lty = 2, col="blue")

par(mfrow=c(1,2))
df3$E1 <- E1
boxplot(E1~site2,data=df3, ylab="E1", xlab="site2")
abline(h = 0, lty = 2, col="blue")
boxplot(E1~Dbin,data=df3, ylab="E1", xlab="Dbin")
abline(h = 0, lty = 2, col="blue")

#Fit smoothers to check
GAM_E1_Temp <- gam(E1 ~ s(Temp),data = df3)
summary(GAM_E1_Temp)#sig
plot(GAM_E1_Temp)
abline(h = 0, lty = 2, col="blue")
points(x = df3$Temp,
       y = E1,
       pch = 16,
       cex = 0.5)
GAM_E1_GA_depth2 <- gam(E1 ~ s(GA_depth2),data = df3)
summary(GAM_E1_GA_depth2)#sig
plot(GAM_E1_GA_depth2)
abline(h = 0, lty = 2, col="blue")
points(x = df3$GA_depth2,
       y = E1,
       pch = 16,
       cex = 0.5)
GAM_E1_fluorescence <- gam(E1 ~ s(fluorescence),data = df3)
summary(GAM_E1_fluorescence)#sig
plot(GAM_E1_fluorescence)
abline(h = 0, lty = 2, col="blue")
points(x = df3$fluorescence,
       y = E1,
       pch = 16,
       cex = 0.5)

###
#Decision, there are patterns in the residuals and overdispersion
###

#try GLM with Gamma
M2<- glm(GeoMn~Temp+fluorescence+GA_depth2+Dbin+site2,
         data=df3,family=Gamma(link="log"))
print(summary(M2), signif.stars=F, digits =2)
E2 <- rstandard(M2) #page 15 &37-38 GAM book

#plot(Variogram(M2,resType="normalized",form=~GA_depth2+Lon), data=df3)

plot(E2)
#Overdispersion
N <- nrow(df3)
p <- length(coef(M2))
Dispersion <- sum(E2^2) / (N - p)
Dispersion # 1.008095

#plot residuals - non-linear patterns
par(mfrow=c(2,2))
plot(df3$Temp, E2)
abline(h = 0, lty = 2, col="blue")
plot(df3$GA_depth2, E2)
abline(h = 0, lty = 2, col="blue")
plot(df3$fluorescence, E2)
abline(h = 0, lty = 2, col="blue")

par(mfrow=c(1,2))
df3$E2 <- E2
boxplot(E2~site2,data=df3, ylab="E2")
abline(h = 0, lty = 2, col="blue")
boxplot(E2~Dbin,data=df3, ylab="E2")
abline(h = 0, lty = 2, col="blue")

#outliers
par(mfrow=c(1,1))
plot(cooks.distance(M2), type="h", ylim=c(0,1))
abline(h=1, lty=2, col="blue")

#normality
par(mfrow=c(1,2))
hist(E2)
qqnorm(E2)
qqline(E2)

df3$E2 <- E2
MyVar <- c("Temp", "fluorescence", "GA_depth2")
MyxyplotPolygon(df3, MyVar, "E2") #with 95% pointwise CI for smoother.

#still non-linear patterns and resiudal patterns

#try a cubic polynomial
M3 <- glm(GeoMn~poly(Temp,3)+poly(fluorescence,3)+poly(GA_depth2,3)+Dbin+site2,
          data=df3,family=Gamma(link="log"))
E3 <- rstandard(M3)
df3$E3 <- E3
MyxyplotPolygon(df3, MyVar, "E3") #better but still there.

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
##move to GAM :)

#use mgcv::gam (GAM book page 43)
M4 <- gam(GeoMn~s(Temp)+s(fluorescence)+s(GA_depth2)+Dbin+site2,
          data=df3,family=Gamma(link="log"), method="REML") #with cross validation to select optimal amount of smoothing (GAM book pg 47)

summary(M4)
anova(M4)
par(mfrow=c(2,2))
plot(M4)


E4<- resid(M4)
F4<-fitted(M4)
par(mfrow=c(1,2), mar=c(5,4,3,3))
hist(E4, breaks=25, main="")
plot(x=F4, y=E4)
abline(0,0, lty = 2, col="blue")

#try Gaussian
M5 <- gam(GeoMn~s(Temp)+s(fluorescence)+s(GA_depth2)+Dbin+site2,
          data=df3,family=gaussian, method="REML") 
summary(M5)
anova(M5)
par(mfrow=c(2,2))
plot(M5)

E5<- resid(M5)
F5<-fitted(M5)
par(mfrow=c(1,2), mar=c(5,5,3,3))
hist(E5, breaks=25, main="")
plot(x=F5, y=E5)
abline(0,0, lty = 2, col="blue") #heterscadasdicity

#go back to gamma GLM. add corgaus (remove Dbin?)
#M6<- glm(GeoMn~Temp+fluorescence+GA_depth2+site2,Dbin,
#  data=df3,family=Gamma(link="log"), correlation = corGaus(form = ~ Dbin+Lon| site2))

M7 <- gam(GeoMn~
            s(Temp, by = Dbin)+
            s(fluorescence, by = Dbin)+
            s(GA_depth2)+
            site2,
          data=df3,family=Gamma(link="log"), method="REML",
          correlation = corGaus(form = ~ Dbin+Lon| site2)) 
summary(M7)
par(mfrow=c(3,3))
plot(M7)

par(mfrow=c(2,2))
visreg(M7, "Temp", by="Dbin", overlay=T, scale="linear")
visreg(M7, "fluorescence", by="Dbin", overlay=T, scale="linear")
visreg(M7, "GA_depth2", scale="linear")
visreg(M7, "site2", scale="linear")

par(mfrow=c(2,2))
visreg(M7, "Temp", by="Dbin", overlay=T, scale="response")
visreg(M7, "fluorescence", by="Dbin", overlay=T, scale="response")
visreg(M7, "GA_depth2", scale="response")
visreg(M7, "site2", scale="response")

########### without correlation
M8 <- gam(GeoMn~
            s(Temp, by = Dbin)+
            s(fluorescence, by = Dbin)+
            s(GA_depth2)+
            site2,
          data=df3,family=Gamma(link="log"), method="REML") 
summary(M8)
par(mfrow=c(3,3))
plot(M8)

par(mfrow=c(2,2))
visreg(M8, "Temp", by="Dbin", overlay=T, scale="linear")
visreg(M8, "fluorescence", by="Dbin", overlay=T, scale="linear")
visreg(M8, "GA_depth2", scale="linear")
visreg(M8, "site2", scale="linear")

par(mfrow=c(2,2))
visreg(M8, "Temp", by="Dbin", overlay=T, scale="response")
visreg(M8, "fluorescence", by="Dbin", overlay=T, scale="response")
visreg(M8, "GA_depth2", scale="response")
visreg(M8, "site2", scale="response")
visreg(M8, "Dbin", scale="response")

par(mfrow=c(1,1))
visreg(M8, "Temp", overlay=T, scale="response")
visreg(M8, "fluorescence", overlay=T, scale="response")

par(mfrow=c(1,1))
plot(cooks.distance(M8), type="h", ylim=c(0,1))
cooks.distance(M8)


E8<- resid(M8)
df3$cooks <- cooks.distance(M8)
plot(df3$cooks, type="h", ylim=c(0,1))

#redo with cooks distance points removed
df4<- df3 %>% dplyr::filter(cooks < 0.1)

M9 <- gam(GeoMn~
            s(Temp, by = Dbin)+
            s(fluorescence, by = Dbin)+
            s(GA_depth2)+
            site2,
          data=df4,family=Gamma(link="log"), method="REML") 
par(mfrow=c(2,2))
visreg(M9, "Temp", by="Dbin", overlay=T, scale="response")
visreg(M9, "fluorescence", by="Dbin", overlay=T, scale="response")
visreg(M9, "GA_depth2", scale="response")
visreg(M9, "site2", scale="response")

#without interactions
M10 <- gam(GeoMn~
             s(Temp)+
             s(fluorescence)+
             s(GA_depth2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML",
           correlation = corGaus(form = ~ Dbin+Lon| site2)) 

summary(M10)
par(mfrow=c(3,3))
plot(M10)

par(mfrow=c(2,2))
visreg(M10, "Temp", scale="linear")
visreg(M10, "fluorescence", scale="linear")
visreg(M10, "GA_depth2", scale="linear")
visreg(M10, "site2", scale="linear")
visreg(M10, "Dbin", scale="linear")

#pdf('3plots/GeoMn.pdf', width = 6, height =8)
par(mfrow=c(3,2))
visreg(M10, "Temp", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "fluorescence", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "GA_depth2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "site2", scale="response", line=list(col="black", lwd=1), rug=F)
visreg(M10, "Dbin", scale="response", line=list(col="black", lwd=1), rug=F)
#dev.off()

#w
M11 <- gam(GeoMn~
             s(Temp, by=site2)+
             s(fluorescence, by=site2)+
             s(GA_depth2, by=site2)+
             site2+Dbin,
           data=df3,family=Gamma(link="log"), method="REML",
           correlation = corGaus(form = ~ Dbin+Lon| site2)) 

summary(M11)
par(mfrow=c(3,3))
plot(M11)

par(mfrow=c(2,3))
visreg(M11, "Temp", by="site2", scale="linear", overlay=T)
visreg(M11, "fluorescence", by="site2", scale="linear", overlay=T)
visreg(M11, "GA_depth2", by="site2", scale="linear", overlay=T)
visreg(M11, "site2", scale="linear")
visreg(M11, "Dbin", scale="linear")

par(mfrow=c(2,3))
visreg(M11, "Temp", by="site2", scale="response", overlay=T)
visreg(M11, "fluorescence", by="site2", scale="response", overlay=T)
visreg(M11, "GA_depth2", by="site2", scale="response", overlay=T)
visreg(M11, "site2", scale="response")
visreg(M11, "Dbin", scale="response")

#cooks distance removed and with correlation term
M12 <- gam(GeoMn~
             s(Temp, by = Dbin)+
             s(fluorescence, by = Dbin)+
             s(GA_depth2)+
             site2,
           data=df4,family=Gamma(link="log"), method="REML",
           correlation = corGaus(form = ~ Dbin+Lon| site2)) 

#compare AIC
AIC_M4 <- cbind("AIC_M4",round(AIC(M4),3), "") 
AIC_M5 <- cbind("AIC_M5",round(AIC(M5),3), "") #
AIC_M7<- cbind("AIC_M7",round(AIC(M7),3), "") #
AIC_M8<- cbind("AIC_M8",round(AIC(M8),3), "") #
AIC_M9<- cbind("AIC_M9",round(AIC(M9),3), "") #
AIC_M10<- cbind("AIC_M10",round(AIC(M10),3), "") #
AIC_M11<- cbind("AIC_M11",round(AIC(M11),3), "") # Different data set!
AIC_M12<- cbind("AIC_M12",round(AIC(M12),3), "") # Different data set!

AIC_table <- rbind(AIC_M4,AIC_M5,AIC_M7,AIC_M8,AIC_M9,AIC_M10,AIC_M11, AIC_M12)
colnames(AIC_table) <- c("Model", "AIC", "Description")
AIC_table <- as.data.frame(AIC_table)
AIC_table<- AIC_table[order(AIC_table$AIC),]


