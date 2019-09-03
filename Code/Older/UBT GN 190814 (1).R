#This protocol was written by Peter Yates on 18/08/14 and should not be used without permission and acknowledgement


#ANALYSIS OF UBT.IM in LONGLINE


setwd("c:/Users/jc229679/Desktop/ENVIRO CHAPTER 5 250814/Analysis 180814/WD")
library(pscl)
library(car)
library(lattice)
source("HighstatLibV7.R")
source("my.zeroinfl.R")
library(MuMIn)
library(effects)
library(visreg)
library(PBSmapping)
library(mgcv) 
library(MASS)

#Import and prepare data
Data1 <- read.table(file = "FINAL dataset 260314.txt", header = TRUE)
Data1$bigp=NULL
Data1$bigc=NULL
Extra1 <- read.table(file = "bay and round from FINAL dataset 260314 shotcode created 190814.txt", header = TRUE)
Data2 <- merge(Data1,Extra1, by="shot", all=TRUE)
Data3<-Data2[Data2$gear=="gillnet", ]
Data3$gear<- factor(Data3$gear)
Data3$logeffort<-log((Data3$length/100)*Data3$soak)
Data3$shark<-Data3$UBT.IM
Data3$mangkm<-Data3$mang/1000
Data4<-Data3[,c("shot","shark","logeffort","depth","temp","sal","sec","mangkm","bay","round")]
colSums(is.na(Data4))#NAS is sec, temp, sal, and mangkm #sum(is.na(a column))
Data<-na.omit(Data4)#lost 33 samples
colSums(is.na(Data))#NAS gone
str(Data)
names(Data)
#write.table(Data4,file="gillnet effort.txt",sep=",")

Data$shark<-ifelse(Data$shark>0,1,0)
UBTl<-glm(shark~bay+round+offset(logeffort), family=binomial, data=Data,na.action=na.pass,)
summary(UBTl)

##############   Data exploration   #######################

#           A Outliers in Y / Outliers in X
MyVar <- c("shark","logeffort","depth","temp","sal","sec","mangkm")
Mydotplot(Data[,MyVar])#no outliers, salinity is skewed, watch for influential points in shark, I checked the largest secchi is real
#also 12 sharks point is real

#           B Collinearity X
#DO removed due to collinearity
MyVar <- c("shark","logeffort","depth","temp","sal","sec","mangkm")
Mypairs(Data[,MyVar])

# GAM1 <- gam(depth ~ s(mangkm),data = Data)
# summary(GAM1)
# visreg(GAM1,scale="response")
# plot(Data$depth~Data$mangkm)

# GAM2 <- gam(sec ~ s(sal),data = Data)
# summary(GAM2)#significant smoother but it only explains 4%, could be due to chance alone
# visreg(GAM2)

MyVar <- c("depth","temp","sal","sec","mangkm")#all <0.5 and no clear non-linear relationships
corvif(Data[,MyVar])#all bellow 3
Data$baynumeric<-as.numeric(Data$bay)
MyVar <- c("depth","temp","sal","sec","mangkm","baynumeric")
corvif(Data[,MyVar])#all below 3
MyVar <- c("depth","temp","sal","sec","mangkm")
Mybwplot(Data, MyVar, "bay")#collinearity!
Mybwplot(Data, MyVar, "round")#collinearity!

#           C Relationships Y vs X
#check y row in pairs above, hard to see with so many zeros
MyVar  <- c("depth","temp","sal","sec","mangkm")
Myxyplot(Data, MyVar, "shark", MyYlab = "shark count")#remember this does not use offset
Data$effort<-exp(Data$logeffort)
Data$density<-log(Data$shark/Data$effort)
Myxyplot(Data, MyVar, "density", MyYlab = "shark count")#unclear

#           D Spatial aspects of sampling design
#the data is clumped into bay and grid zone

#           E Interactions (can you see any and is the enough data to test them?)
coplot(shark ~ depth | sec,
       data = Data,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
coplot(shark ~ sal | sec,
       data = Data,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

#           F Zero inflation Y
sum(Data$shark == 0) #252
100*(sum(Data$shark == 0) / nrow(Data)) #79.74684% zeros
table(Data$shark)
sum(Data$shark)#141 sharks
plot(table(Data$shark))

#           G Are categorical covariates balanced? Do all levels have enough obs (10-15)
# table(Data$bigp)

#           Outcomes of data exploration:
#removed DO due to collinearity with temperature
#no outliers, salinity is skewed, watch for influential points in shark
#78% zeros, more sharks than in longlines
#keep an eye on collinearity between sec and sal
#also depth and mang, remove one at a time and check for significance changes in both

############ GLM(P) ##################


M1<- glm(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort),data=Data,family=poisson,na.action=na.pass)

#     remove mang/depth
M1ad<- glm(shark~depth+temp+sal+sec+sal:sec+offset(logeffort),data=Data,family=poisson,na.action=na.pass)
M1am<- glm(shark~temp+sal+sec+mangkm+sal:sec+offset(logeffort),data=Data,family=poisson,na.action=na.pass)
summary(M1a)
summary(M1ad)#both still sig
summary(M1am)


#Overdispersion
E1 <- resid(M1, type = "pearson")
N <- nrow(Data)
p <- length(coef(M1))
Dispersion <- sum(E1^2) / (N - p)
Dispersion#2.424737

#Why do you have overdispersion
#A. Outliers                  => plot E1 verses all covariates. Remove them..but subjective
#B. Missing covariates        => plot resid verses vars not in the model, boxplot residuals verses bay and round.
#C. Missing interactions      => Add them (coplot)
#D. Zero inflation            => ZIP/ZINB
#E. Dependency                => Check for temporal or spatial correlation, plot resid verses covariates
#F. Non-linear relationships  => plot resid verses covariates, GAM?
#G. Wrong link function       => Change it
#If nothing can be pinpointed....go to NB GLM


############# try poisson GLMM ##################ex 81#####
library(lme4)
MM1 <- glmer(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort) + (1|bay),
             data = Data, 
             family = poisson)
summary(MM1)
summary(M1)#similar significance compared with MM1

EMM1 <- resid(MM1, type = "pearson")
N  <- nrow(Data)
p  <- length(fixef(MM1)) + 1 #+1 is due to the sigma_hive, so number of parameters is interecept plus fixes coeff + random coeff
Overdispersion <- sum(EMM1^2) / (N - p)
Overdispersion#2.39, same as M1

Data$EMM1<-EMM1
Data$MyCex <- abs(EMM1) / max(abs(EMM1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- EMM1
Data$MyCol[EMM1 >= 0 ] <- 1
Data$MyCol[EMM1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))
#Similar RESIDUAL PATTERNS


#A      Outliers
F1 <- fitted(M1, type = "response")
plot(x= F1, y = E1)
abline(h = 0, lty = 2)#lots of zeros, no outliers

plot(M1, which = c(4))
plot(cooks.distance(M1a),
     type = "h",
     ylim = c(0,1.5))
abline(h=1)
identify(cooks.distance(M1a),
     type = "h",
     ylim = c(0,1.5))#129 is just overly influential

#       rerun without point 129, does it change the result?
Data129 <- Data[c(-129), ]
M1129<- glm(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort),data=Data,family=poisson,na.action=na.pass)
M1dr129<-dredge(M1129,rank="AIC",m.max=8, fixed="offset(logeffort)")
M1dr129[1:10, ]
M1dr[1:10, ]#top models are the same
summary(M1a129)
summary(M1a)#some differences here but given this data is real, there were lots of sharks, and the cooks dist is
#~1.2, I'm going to leave it in

#Overdispersion
E1a129 <- resid(M1a129, type = "pearson")
N <- nrow(Data)
p <- length(coef(M1a129))
Dispersion <- sum(E1a129^2) / (N - p)
Dispersion#2.4
#overdispersion is the same

plot(x = F1, 
     y = Data$shark,
     xlab = "Fitted values",
     ylab = "Observed data")
abline(coef = c(0, 1), lty = 2)

#B      Missing covariates
par(mfrow=c(1,2))
boxplot(E1 ~ bay, data = Data) #fine
boxplot(E1 ~ round, data = Data) #13A?

#C      missing interactions, no

#D      Zero inflation...done already

#E      Dependency
Extra3 <- read.table(file = "spatial coords from dataset 260314 created 180814.txt", header = TRUE)
Data6<-merge(Data,Extra3, by="shot", all=F)
Data6<-na.omit(Data6)
loc<-data.frame(X=Data6$Longitude,Y=Data6$Latitude)
attr(loc,"zone")<-55
attr(loc,"projection")<-"LL"
xyUTM<-convUL(loc,km=F) # converts coordinates from Lon/Lat to UTM in meters
xyplot(xyUTM$Y~xyUTM$X,aspect="iso")

Data$E1<-E1
Data$MyCex <- abs(E1) / max(abs(E1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- E1
Data$MyCol[E1 >= 0 ] <- 1
Data$MyCol[E1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))#need xyplot for aspect="iso"
#it is our intention to....p

#F      Non-linear patterns
MyVar  <- c("depth","temp","sal","sec","mangkm")
Myxyplot(Data, MyVar, "E1a") #no clear non-linear patterns
#Fit a smoother to check
GAME1a <- gam(E1a ~ s(sec),data = Data)
summary(GAME1a)#non sig
# plot(GAME1a, ylim = c(-3, 3))
# abline(h = 0, lty = 2)
# points(x = Data$sec,
#        y = E1a,
#        pch = 16,
#        cex = 0.5)
GAME1a <- gam(E1a ~ s(sal),data = Data)
summary(GAME1a)#non sig
GAME1a <- gam(E1a ~ s(depth),data = Data)
summary(GAME1a)#non sig
GAME1a <- gam(E1a ~ s(temp),data = Data)
summary(GAME1a)#non sig
GAME1a <- gam(E1a ~ s(mangkm),data = Data)
summary(GAME1a)#non sig

#       Large variance?
dotchart(Data$shark)#no


# Outcome of GLM(P)
#Use GLM(NB), also check whether ZINB gives a better fit 
#There might be some spatial dependency between grid sections, however bay is confounded with sal, sec and temp so cant be
#included as a fixed term and I'm not rushing into GLMM esp. given I am heading towards ZI!

############ GLM(NB) ##################

M2 <- glm.nb(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort),
             link = "log",data = Data,na.action=na.pass)
summary(M2) #K is the theta, report it!

E2 <- resid(M2, type = "pearson")
N  <- nrow(Data)
p  <- length(coef(M2)) + 1  #+1 is due to k
M2Disp <- sum(E2^2) / (N - p)
M2Disp   #1.237689

M2dredge<-dredge(M2,rank="AIC",m.max=8, fixed="offset(logeffort)")
M2dredge[1:10, ]

top<-get.models(M2dredge,subset=delta<2)
topav<-model.avg(top, beta=TRUE)
summary(topav)
coef<-topav$coef.shrinkage
coef
expcoef<-exp(coef)
expcoef
names(topav)
RVI<-topav$importance
RVI

M2a <- glm.nb(shark~depth+mangkm+sec+offset(logeffort), link = "log",data = Data)
summary(M2a)

E2a <- resid(M2a, type = "pearson")
N  <- nrow(Data)
p  <- length(coef(M2a)) + 1  #+1 is due to k
M2Disp <- sum(E2a^2) / (N - p)
M2Disp#1.153821

#       get The R2
M2aRsq<-100*((M2a$null.deviance-M2a$deviance)/M2a$null.deviance)
M2aRsq#18.02243


################# COMPARE WITH ZI(P) #############

#I should also run the dredge again on a ZI global model??

M3a<- zeroinfl(shark~depth+mangkm+sec+offset(logeffort)|depth+mangkm+sec,dist="poisson",link="logit",data=Data)
summary(M3a)

E3a <- resid(M3a, type = "pearson")
N  <- nrow(Data)
p  <- length(coef(M3a))
M3aDisp <- sum(E3a^2) / (N - p)
M3aDisp#1.3, slightly more than GLM NB

#what is significant? manual drop1 page 194. P values are approximate.

#fit the NB, check for overdispersion. Even if its ok, 'we argue that a count of zero may occur because the habitat 
  #was unsuitable (true zero), or because it was present but not caught (false zero). Hence ZI models were fit and compared
  #with the GLM NB using the liklihood ratio test (between two ZI models (page 198)) and AIC (ZI book).
  #"the difference in fit is often trivial" (see favourites bar).

# #compare ZIP and ZINB with LR test
# L<-2*abs(as.numeric(LogLik(ZIP)-LogLik(ZINB)))
# Pval<- 1-pchisq(L,1)
# c(L,Pval)
# #if significant, ZINB is better

#Does ZIP provide a better fit than GLM(P)? (page 198)

M3asub<- zeroinfl(shark~depth+mangkm+sec+offset(logeffort)|1,dist="poisson",link="logit",data=Data)
#refitting the model using only
#an intercept in the logistic link function

#if ZINB: fix theta
# M3sub<-Myzeroinfl(shark~mangkm+offset(logeffort)|1,dist="negbin",link="logit",data=data,
#                    fixed.log.theta=log(ZINB$theta), control = zeroinfl.control(trace=0))ensuring that the 
#parameter k from the full ZINB was used and kept fixed in the estimation process.

summary(M3asub)

L<-2*abs(as.numeric(M3a$loglik-M3asub$loglik))# check this.....
Pval<- 1-pchisq(L,1)#need to change df (difference in number of params)
c(L,Pval)# they are significantly different

#         go back and run the dredge for ZINB
M3<- zeroinfl(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort)|depth+temp+sal+sec+mangkm+sal:sec+depth:sec,
              dist="poisson",link="logit",data=Data, na.action=na.pass)
M3dredge<-dredge(M3,rank="AIC",m.max=8, fixed="offset(logeffort)")
M3dredge[1:10, ]#close enough!
M3aa<-zeroinfl(shark~depth+mangkm+sal+sec+offset(logeffort)|sec+temp,
               dist="poisson",link="logit",data=Data, na.action=na.pass)
range(Data$mangkm)#0.0451,5.5937
range(Data$sal)#16,37
range(Data$sec)#0.1,4.5
range(Data$logeffort)#-0.9538628,1.6221567
range(Data$temp)#18.9 32.4
range(Data$depth)#0.85,5.90

ZIPdata<-expand.grid(
  mangkm=seq(),
  depth=seq(0.85,5.90,length=50),
  logeffort=mean(Data$logeffort))

ZIPdata$Pcount<-predict(M3aaa,newdata=ZIPdata,type="count")#see ex4a(2)
ZIPdata$Pzero<-predict(M3aaa,newdata=ZIPdata,type="zero")

par(mfrow=c(1,2))
plot(ZIPdata$Pcount~ZIPdata$depth)
plot(ZIPdata$Pzero~ZIPdata$depth)
visreg(M3aaa,"depth",scale="response")

#if significant 'some of the regression parameters in the logistic link function (the part modelling the predicted 
#probability of false zeros) are significantly different from zero at the 5% level' (page 198).#If the two ZINB models provide
#the same fit and if the intercept of the sub-model is a large negaitive number, then I should use NB GLM.
#lots of zeros but OD is ok in NB or P? thats cool! it just means that the zeros are explained well by the covariates!!
#if it is, manual drop 1,plotting and validation are on page 199


########## Extra Model validation of whatever model's are chosen ######################
M2a <- glm.nb(shark~depth+mangkm+sec+offset(logeffort), link = "log",data = Data)

#       get The R2
M2aRsq<-100*((M2a$null.deviance-M2a$deviance)/M2a$null.deviance)
M2aRsq#18.02243

#       collinearity between depth and mang, they are only significant together!
M2ad <- glm.nb(shark~depth+sec+offset(logeffort), link = "log",data = Data)
M2am <- glm.nb(shark~mangkm+sec+offset(logeffort), link = "log",data = Data)
drop1(M2a,test="Chi")
drop1(M2ad, test = "Chi")
drop1(M2am, test = "Chi")#they are only significant together!

#       Homogeneity and outliers
F2a <- fitted(M2a)
plot(x = F2a, 
     y = E2a,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)#fine

#       Independence 
Data$E2a <- resid(M2, type = "pearson")
MyVar <- c("depth","temp","sal","sec","mangkm")
Myxyplot(Data, MyVar, "E2a")#if something isnt in the best, but prevalent in the confidence set, check resiudals patterns against that variable.

par(mfrow=c(1,2))
boxplot(E2a ~ bay, data = Data) #fine
boxplot(E2a ~ round, data = Data) #fine

#       Influential observations
par(mfrow = c(1, 1))
plot(M2a, which = c(4))#fine, that 1.2 ob gone in GLMNB

#plot fitted values vs observed values
plot(x = F2a,y = Data$shark)
abline(coef = c(0, 1), lty = 2)

#Spatial independence?
#use confounding identified by Rhondda, this plot, and only 5 levels as justification for exclusion of 
#bay as fixed and random, also boxplot residuals per bay. The variation between bays is explained by 
#the environmental covariates!! (or there is no variation between bays, but based on yates et al 2014 we know thats not the case)
#Dependency and missing covariate explorations look the same as for GLMP
Data$E2a<-E2a
Data$MyCex <- abs(E2a) / max(abs(E2a))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- E2a
Data$MyCol[E2a >= 0 ] <- 1
Data$MyCol[E2a < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))#same


####### PLOT THE MODEL ####################
#use visreg fist then try manually, see excersice numbers below
#plot(allEffects(llplot))

par(mfrow=c(1,1))
visreg(M2,"mangkm",overlay=TRUE,partial=TRUE,type="conditional",scale="response",print.cond=TRUE,legend=TRUE,band=T,
       ylab="log (shark abundance)")


M2a <- glm.nb(shark~depth+mangkm+sec+offset(logeffort), link = "log",data = Data)
par(mfrow=c(1,3))
visreg(M2a,"sec",xlab="Secchi depth (m)",ylab="log(shark abundance)", 
       partial=TRUE,type="conditional", scale="linear",
       print.cond=TRUE,legend=TRUE,band=TRUE,line.par=c(col="black"))
visreg(M2a,"mangkm",xlab="Distance to mangroves (km)",
       ylab=" ",partial=TRUE,type="conditional", scale="linear",
       print.cond=TRUE,legend=TRUE,band=TRUE,line.par=c(col="black"))
visreg(M2a,"depth",xlab="Depth (m)",ylab=" ",partial=TRUE,type="conditional", 
       scale="linear",print.cond=TRUE,legend=TRUE,band=TRUE,line.par=c(col="black"))

#9x4

















########### TRY GLMM P #########################

Extra3 <- read.table(file = "spatial coords from dataset 260314 created 180814.txt", header = TRUE)
Data6<-merge(Data,Extra3, by="shot", all=F)
Data6<-na.omit(Data6)
loc<-data.frame(X=Data6$Longitude,Y=Data6$Latitude)
attr(loc,"zone")<-55
attr(loc,"projection")<-"LL"
xyUTM<-convUL(loc,km=F) # converts coordinates from Lon/Lat to UTM in meters
xyplot(xyUTM$Y~xyUTM$X,aspect="iso")


#bay as RI
library(lme4)
MM1 <- glmer(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort) + (1|bay),
             data = Data, 
             family = poisson)
summary(MM1)#less significance compared with MM1
summary(M1)

EMM1 <- resid(MM1, type = "pearson")
N  <- nrow(Data)
p  <- length(fixef(MM1)) + 1 #+1 is due to the sigma_hive, so number of parameters is interecept plus fixes coeff + random coeff
Overdispersion <- sum(EMM1^2) / (N - p)
Overdispersion#2.08

Data$EMM1<-EMM1
Data$MyCex <- abs(EMM1) / max(abs(EMM1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- EMM1
Data$MyCol[EMM1 >= 0 ] <- 1
Data$MyCol[EMM1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))
#actually worse :)

#site as RI
Extra4 <- read.table(file = "siteid.txt", header = TRUE)
Data <- merge(Data,Extra4, by="shot", all=F)
MM1 <- glmer(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort) + (1|site),
             data = Data, 
             family = poisson)
summary(MM1)#less significance compared with M1

EMM1 <- resid(MM1, type = "pearson")
N  <- nrow(Data)
p  <- length(fixef(MM1)) + 1 #+1 is due to the sigma_hive, so number of parameters is interecept plus fixes coeff + random coeff
Overdispersion <- sum(EMM1^2) / (N - p)
Overdispersion#1.95

Data$EMM1<-EMM1
Data$MyCex <- abs(EMM1) / max(abs(EMM1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- EMM1
Data$MyCol[EMM1 >= 0 ] <- 1
Data$MyCol[EMM1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))
#worse

#round as RI
MM1 <- glmer(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort) + (1|round),
             data = Data, 
             family = poisson)
summary(MM1)#less significance compared with M1

EMM1 <- resid(MM1, type = "pearson")
N  <- nrow(Data)
p  <- length(fixef(MM1)) + 1 #+1 is due to the sigma_hive, so number of parameters is interecept plus fixes coeff + random coeff
Overdispersion <- sum(EMM1^2) / (N - p)
Overdispersion#1.82

Data$EMM1<-EMM1
Data$MyCex <- abs(EMM1) / max(abs(EMM1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- EMM1
Data$MyCol[EMM1 >= 0 ] <- 1
Data$MyCol[EMM1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))
#worse :)



##############            Negative binomial GLMM:################

#install.packages("glmmADMB", repos=c("http://www.hafro.is/~arnima/repos", getOption("repos")))
library(glmmADMB)


Extra3 <- read.table(file = "spatial coords from dataset 260314 created 180814.txt", header = TRUE)
Data6<-merge(Data,Extra3, by="shot", all=F)
Data6<-na.omit(Data6)
loc<-data.frame(X=Data6$Longitude,Y=Data6$Latitude)
attr(loc,"zone")<-55
attr(loc,"projection")<-"LL"
xyUTM<-convUL(loc,km=F) # converts coordinates from Lon/Lat to UTM in meters
xyplot(xyUTM$Y~xyUTM$X,aspect="iso")


############bay as random intercept


MM1 <- glmmadmb(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort), 
                random =~ 1|bay, 
                family="nbinom", data=Data)
summary(MM1)

# p <- 6 + 1 + 1  #6 betas, 1 sigma, 1 k
# Overdispersion2 <-sum(E2^2) / (N - p)
# Overdispersion2


EMM1 <- resid(MM1, type = "pearson")

Data$EMM1<-EMM1
Data$MyCex <- abs(EMM1) / max(abs(EMM1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- EMM1
Data$MyCol[EMM1 >= 0 ] <- 1
Data$MyCol[EMM1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))
#worse

############site as random intercept
Extra4 <- read.table(file = "siteid.txt", header = TRUE)
Data <- merge(Data,Extra4, by="shot", all=F)

MM1 <- glmmadmb(shark~depth+temp+sal+sec+mangkm+sal:sec+depth:sec+offset(logeffort), 
                random =~ 1|site, 
                family="nbinom", data=Data)
summary(MM1)



EMM1 <- resid(MM1, type = "pearson")
N  <- nrow(Data)
p  <- length(fixef(MM1)) + 1 #+1 is due to the sigma_hive, so number of parameters is interecept plus fixes coeff + random coeff
Overdispersion <- sum(EMM1^2) / (N - p)
Overdispersion#1.66

Data$EMM1<-EMM1
Data$MyCex <- abs(EMM1) / max(abs(EMM1))#or 3 * abs(E2) / max(E2), #3 * Data$MyCex
Data$MyCol <- EMM1
Data$MyCol[EMM1 >= 0 ] <- 1
Data$MyCol[EMM1 < 0 ]  <- 2

xyplot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
       data = Data,main = "Spatial plot of residuals")
par(mfrow=c(2,3))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rock",xlim=c(394000,415500),ylim=c(7975000,7995000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Bowl",xlim=c(505000,547000),ylim=c(7850000,7865000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Upst",xlim=c(565000,582000),ylim=c(7805000,7815000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Edge",xlim=c(638000,655000),ylim=c(7765000,7780000))
plot(xyUTM$Y~xyUTM$X,aspect="iso",cex = 4 * sqrt(Data$MyCex),pch = 16,col = Data$MyCol,
     data = Data,main = "Spatial plot of residuals: Rep",xlim=c(675000,691000),ylim=c(7725000,7740000))
#worse






