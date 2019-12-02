# Fluoresence

mydata <- read.csv("Data/MNF_SS200408_ctd_trawler/MNF_SS200408_ctd_trawler.csv")
str(mydata)

plot(mydata$START_LON, mydata$START_LAT, col = as.factor(mydata$site))

mydata_CTD <- read.csv("Data/HYD cleaned 080419.csv")

mydata$site[mydata$START_LAT  <= -31.5] <- "DiamondHead"
#mydata$Zone[mydata$Latitude < -34] <- "Southern NSW"
#mydata$Zone[mydata$Latitude > -34 & mydata$Latitude <= -31] <- "Central NSW"
mydata$site[mydata$START_LAT > -29.1] <- "EvansHead"
mydata$site[mydata$START_LAT > -31 & mydata$START_LAT < -29.7 & mydata$START_LON > 153.4] <- "NorthSolitary"

mydata$site <- as.factor(mydata$site)

plot(mydata$START_LON, mydata$START_LAT, col = as.factor(mydata$site))


table(mydata_CTD$OPC_site, mydata_CTD$long3)
hist(mydata$START_LAT)

sites <- levels(mydata$site)



### Get distance from shore
library(geosphere)

mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "DiamondHead") {
    mydata$Distance_Coast[i] = distm(c(152.782016, -31.749982), c(mydata$START_LON[i], mydata$START_LAT[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EvansHead") {
    mydata$Distance_Coast[i] = distm(c(153.481200, -28.994824), c(mydata$START_LON[i], mydata$START_LAT[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NorthSolitary") {
    mydata$Distance_Coast[i] = distm(c(153.224309, -29.995614), c(mydata$START_LON[i], mydata$START_LAT[i]), fun = distHaversine)
  }
}



for (j in sites){
  mydata2 <- subset(mydata, site == j)
  #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
  fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$PRESSURE, z = mydata2$FLUORESCENCE)
  pdf(paste0('plots/CTD/',j,"_Flu",'.pdf'), width=10, height=5)
  print(filled.contour(fit1, plot.title = title(main = c(j)),
                       zlim = c(min(mydata$FLUORESCENCE), max(mydata$FLUORESCENCE))))
  dev.off()
  png(paste0('plots/CTD/',j,"_Flu",'.png'), width=6000, height=3000, res = 600)
  print(filled.contour(fit1, plot.title = title(main = c(j)),
                       zlim = c(min(mydata$FLUORESCENCE), max(mydata$FLUORESCENCE))))
  dev.off()
  pdf(paste0('plots/CTD/',j,"_Flu",i,'_lines.pdf'), width=10, height=5)
  print(contour(fit1, plot.title = title(main = c(j))))
  dev.off()
}

# now only top 200
for (j in sites){
  mydata2 <- subset(mydata, site == j)
  #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
  fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$PRESSURE, z = mydata2$FLUORESCENCE)
  pdf(paste0('plots/CTD/',j,"_Flu_200",'.pdf'), width=10, height=5)
  print(filled.contour(fit1, plot.title = title(main = c(j)),
                       zlim = c(min(mydata$FLUORESCENCE), max(mydata$FLUORESCENCE)),
                       ylim = c(-200,0)))
  dev.off()
  png(paste0('plots/CTD/',j,"_Flu_200",'.png'), width=6000, height=3000, res = 600)
  print(filled.contour(fit1, plot.title = title(main = c(j)),
                       zlim = c(min(mydata$FLUORESCENCE), max(mydata$FLUORESCENCE)),
                       ylim = c(-200,0)))
  dev.off()
  pdf(paste0('plots/CTD/',j,"_Flu_200",i,'_lines.pdf'), width=10, height=5)
  print(contour(fit1, plot.title = title(main = c(j))),
        ylim = c(-200,0))
  dev.off()
}

mydata2 <- subset(mydata, site == "NorthSolitary")

fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$PRESSURE, z = mydata2$FLUORESCENCE)
