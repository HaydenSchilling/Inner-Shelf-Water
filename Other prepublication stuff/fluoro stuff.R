# Fluoresence
library(akima)
library(tidyverse)
library(reshape2)

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

mydata <- mydata %>% drop_na(site)

## Chl_a calibration from Data/Chl Calibration
mydata$Chl_a <- 0

Fl = c(39, 37, 13, 54, 63, 93, 104, 18, 27, 30, 33, 19, 24, 33, 18, 26, 28, 33, 56, 62, 40, 72, 78)

Chl = c(1.3590425, 1.21585, 0.5718425, 1.5171025, 1.926405, 1.9881475, 2.1498575, 0.822945,
       1.06137, 1.564785, 1.0204725, 1.3818275, 0.58839125, 1.128423333, 0.7805025, 0.5479125,
       0.30752375, 0.2520275, 0.991435, 1.4254225, 0.38643, 1.25026125, 1.60385)

cal_fit <- lm(Chl ~ Fl) # Chl = 0.01567 * FL + 0.44211

coefs_cal_fit <- coefficients(cal_fit)


mydata$Chl_a <- coefs_cal_fit[2]*mydata$FLUORESCENCE + coefs_cal_fit[1]

plot(mydata$Chl_a ~ mydata$FLUORESCENCE)

hist(mydata$Chl_a)
### Get distance from shore
library(geosphere)

mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "DiamondHead") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$START_LON[i], mydata$START_LAT[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EvansHead") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$START_LON[i], mydata$START_LAT[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NorthSolitary") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$START_LON[i], mydata$START_LAT[i]), fun = distHaversine)
  }
}

### Get Bathymetry and add distance from coast
Bathy <- read.csv("Data/Transect Bathymetry.csv", header = T)
Bathy <- subset(Bathy, Bathymetry <= 0)


Bathy$Distance_Coast = 0
for (i in 1:nrow(Bathy)){
  if (Bathy$site[i] == "CapeByron") {
    Bathy$Distance_Coast[i] = distm(c(153.58, -28.6), c(Bathy$Longitude[i], Bathy$Latitude[i]), fun = distHaversine)
  }
  if (Bathy$site[i] == "DiamondHead") {
    Bathy$Distance_Coast[i] = distm(c(152.75, -31.8), c(Bathy$Longitude[i], Bathy$Latitude[i]), fun = distHaversine)
  }
  if (Bathy$site[i] == "EvansHead") {
    Bathy$Distance_Coast[i] = distm(c(153.48, -29.0), c(Bathy$Longitude[i], Bathy$Latitude[i]), fun = distHaversine)
  }
  if (Bathy$site[i] == "NorthSolitary") {
    Bathy$Distance_Coast[i] = distm(c(153.23, -30.0), c(Bathy$Longitude[i], Bathy$Latitude[i]), fun = distHaversine)
  }
}
sitesB = c("DiamondHead", "NorthSolitary", "EvansHead", "CapeByron")


### Make plots
# for (j in sites){
#   mydata2 <- subset(mydata, site == j)
#   #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
#   fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$PRESSURE, z = mydata2$FLUORESCENCE)
#   pdf(paste0('plots/CTD/',j,"_Flu",'.pdf'), width=10, height=5)
#   print(filled.contour(fit1, plot.title = title(main = c(j)),
#                        zlim = c(min(mydata$FLUORESCENCE), max(mydata$FLUORESCENCE))))
#   dev.off()
#   png(paste0('plots/CTD/',j,"_Flu",'.png'), width=6000, height=3000, res = 600)
#   print(filled.contour(fit1, plot.title = title(main = c(j)),
#                        zlim = c(min(mydata$FLUORESCENCE), max(mydata$FLUORESCENCE))))
#   dev.off()
#   pdf(paste0('plots/CTD/',j,"_Flu",i,'_lines.pdf'), width=10, height=5)
#   print(contour(fit1, plot.title = title(main = c(j))))
#   dev.off()
# }

### GGplot plots
for (j in 1:length(sites)){
  mydata2 <- mydata %>% 
    filter(site == sites[j] & is.na(PRESSURE)==FALSE)
  Bathy2 <- filter(Bathy, site == sitesB[j] & Bathymetry > (-(max(mydata2$PRESSURE))-100))
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -PRESSURE, z = Chl_a, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Chl_a")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = Chl_a)) + 
    geom_tile(data = df, aes(fill = Chl_a)) +
    geom_contour(colour = "white", binwidth = 0.125) + 
    scale_fill_distiller(palette = "Greens", direction = 1, limits = c(min(mydata$Chl_a), max(mydata$Chl_a))) + 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -PRESSURE, alpha = 0.5)) +
    geom_line(data= Bathy2, aes(x = Distance_Coast, y = Bathymetry), inherit.aes = FALSE) +
    ggtitle(paste0("Chl_a at ", sites[j]))
  
  ggsave(paste0('plots/CTD/',sites[j],"Chl_a",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/CTD/',sites[j],"Chl_a",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}



# now only top 200
for (j in 1:length(sites)){
  mydata2 <- mydata %>% 
    filter(site == sites[j] & is.na(PRESSURE)==FALSE)
  Bathy2 <- filter(Bathy, site == sitesB[j] & Bathymetry > (-(max(mydata2$PRESSURE))-100))
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -PRESSURE, z = Chl_a, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Chl_a")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = Chl_a)) + 
    geom_tile(data = df, aes(fill = Chl_a)) +
    geom_contour(colour = "white", binwidth = 0.125) + 
    coord_cartesian(xlim = c(min(mydata2$Distance_Coast), max(mydata2$Distance_Coast)),
                    ylim = c(-200, 0)) +
    scale_fill_distiller(palette = "Greens", direction = 1, limits = c(min(mydata$Chl_a), max(mydata$Chl_a))) + 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -PRESSURE, alpha = 0.5)) +
    geom_line(data= Bathy2, aes(x = Distance_Coast, y = Bathymetry), inherit.aes = FALSE) +
    ggtitle(paste0("Chl_a at ", sites[j]))
  
  ggsave(paste0('plots/CTD/',sites[j],"Chl_a_200m",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/CTD/',sites[j],"Chl_a_200m",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}

