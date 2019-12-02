# Zooplankton data interpolation

# Interpolating plots

#install.packages("akima")

library(akima)

# load data
mydata <- read.csv("Data/Original OPC Data cleaned.csv")
str(mydata)
head(mydata)

### Get distance from shore
library(geosphere)

mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "CapeByron") {
    mydata$Distance_Coast[i] = distm(c(153.638598, -28.635808), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "DiamondHead") {
    mydata$Distance_Coast[i] = distm(c(152.782016, -31.749982), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EvansHead") {
    mydata$Distance_Coast[i] = distm(c(153.481200, -28.994824), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NorthSolitary") {
    mydata$Distance_Coast[i] = distm(c(153.224309, -29.995614), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
}

# try a single plot

mydata2 <- subset(mydata, site == "DiamondHead")
fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$depth, z = log10(mydata2$biomass))
filled.contour(fit1, zlim = c(min(log10(mydata$biomass)), log10(mydata$biomass)))


sites <- levels(mydata$site)

### Biomass interpolation and plots
for (j in sites){
  mydata2 <- subset(mydata, site == j)
  
  fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$depth, z = log10(mydata2$biomass))
  pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  print(filled.contour(fit1, zlim = c(min(log10(mydata$biomass)), log10(mydata$biomass))),
        plot.title = title(main = c(j)))
  dev.off()
  png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  print(filled.contour(fit1, zlim = c(min(log10(mydata$biomass)), log10(mydata$biomass))),
        plot.title = title(main = c(j)))
  dev.off()
}

