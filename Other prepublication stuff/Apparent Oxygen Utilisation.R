### Apparent Oxygen Potential

### FROM MATLAB CODE

# % aou.m                                            by:  Edward T Peltzer, MBARI
# %                                                  revised:  2007 Apr 26.
# %
# % CALCULATE OXYGEN CONCENTRATION AT SATURATION
# %
# % Source:  The solubility of nitrogen, oxygen and argon in water and
# %         seawater - Weiss (1970) Deep Sea Research V17(4): 721-735.
# %
# % Molar volume of oxygen at STP obtained from NIST website on the
# %          thermophysical properties of fluid systems:
#   %
# %          http://webbook.nist.gov/chemistry/fluid/
#   %
# %
# % CALCULATE AOU BY DIFFERENCE:
#   %
# %         AOU (umol/kg) = sat O2 (umol/kg) - obs o2 (umol/kg).
# %
# %
# % Input:       S = Salinity (pss-78)
# %              T = Potential Temp (deg C)
# %              O2 = Meas'd Oxygen Conc (umol/kg)
# %
# % Output:      Apparant Oxygen Utilization (umol/kg).
# %
# %                        AOU = aou(S,T,O2).
# 
# function [AOU]=aou(S,T,O2)
# 
# 
# % DEFINE CONSTANTS, ETC FOR SATURATION CALCULATION
# 
# %    The constants -177.7888, etc., are used for units of ml O2/kg.
# 
#   T1 = (T + 273.15) ./ 100;
# 
#   OSAT = -177.7888 + 255.5907 ./ T1 + 146.4813 .* log(T1) - 22.2040 .* T1;
#   OSAT = OSAT + S .* (-0.037362 + T1 .* (0.016504 - 0.0020564 .* T1));
#   OSAT = exp(OSAT);
# 
# 
# % CONVERT FROM ML/KG TO UM/KG
# 
#   OSAT = OSAT * 1000 ./ 22.392;
# 
# 
# % CALCULATE AOU
# 
#   AOU = OSAT - O2;


## R Code - Hayden Schilling (SIMS/UNSW) 9/12/2019) adapted from above Matlab code

# requires Salinity (S; psu), Temperature (T; deg C), Oxygen (O2; umol / kg)
# I think my O2 is in the correct format - currently uM/L which should be equivalent to umol/kg

aou <- function(S, T, O2){ # does predicted minus observed
   T1 = (T + 273.15) / 100;
   OSAT = -177.7888 + 255.5907 / T1 + 146.4813 * log(T1) - 22.2040 * T1
   OSAT = OSAT + S * (-0.037362 + T1 * (0.016504 - 0.0020564 * T1))
   OSAT = exp(OSAT)
   OSAT = OSAT * 1000 / 22.392
   AOU = OSAT - O2
   return(AOU)
}


### Now do my own data

mydata <- read.csv("Data/HYD cleaned 080419.csv")
str(mydata)
head(mydata)

plot(mydata$Salinity, mydata$CTD.Sal)

### Get distance from shore
library(geosphere)

mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$OPC_site[i] == "CB") {
    mydata$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
  if (mydata$OPC_site[i] == "DH") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
  if (mydata$OPC_site[i] == "EH") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
  }
  if (mydata$OPC_site[i] == "NS") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$long[i], mydata$lat[i]), fun = distHaversine)
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



mydata$AOU <- aou(mydata$CTD.Sal, mydata$CTD.Temp, mydata$Oxygen)


library(akima)

sites <- levels(mydata$OPC_site)

# for (j in sites){
#   mydata2 <- subset(mydata, OPC_site == j)
#     #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
#     fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = mydata2$AOU)
#     pdf(paste0('plots/CTD/',j,"_AOU",'.pdf'), width=10, height=5)
#     print(filled.contour(fit1, plot.title = title(main = c(j)),
#           zlim = c(min(mydata$AOU), max(mydata$AOU))))
#     dev.off()
#     png(paste0('plots/CTD/',j,"_AOU",'.png'), width=6000, height=3000, res = 600)
#     print(filled.contour(fit1, plot.title = title(main = c(j)),
#                          zlim = c(min(mydata$AOU), max(mydata$AOU))))
#     dev.off()
#     pdf(paste0('plots/CTD/',j,"_AOU",i,'_lines.pdf'), width=10, height=5)
#     print(contour(fit1, plot.title = title(main = c(j))))
#     dev.off()
# }

# now in ggplot2
for (j in 1:length(sites)){
  mydata2 <- mydata %>% 
    filter(OPC_site == sites[j] & is.na(Depth)==FALSE)
  Bathy2 <- filter(Bathy, site == sitesB[j] & Bathymetry > (-(max(mydata2$Depth))-100))
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = AOU, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "AOU")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = AOU)) + 
    geom_tile(data = df, aes(fill = AOU)) +
    geom_contour(colour = "white", binwidth = 20) + 
    #coord_cartesian(xlim = c(min(mydata2$Distance_Coast), max(mydata2$Distance_Coast)),
    #                ylim = c(-200, 0)) +
    scale_fill_distiller(palette = "Greens", direction = 1, limits = c(min(mydata$AOU), max(mydata$AOU))) + 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth, alpha = 0.5)) +
    geom_line(data= Bathy2, aes(x = Distance_Coast, y = Bathymetry), inherit.aes = FALSE) +
    ggtitle(paste0("AOU at ", sites[j]))
  
  ggsave(paste0('plots/CTD/',sites[j],"_AOU",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/CTD/',sites[j],"_AOU",'.png'),width = 10, height = 5, dpi = 600)
  
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
# for (j in sites){
#   mydata2 <- subset(mydata, OPC_site == j)
#   #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
#   fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = mydata2$AOU)
#   pdf(paste0('plots/CTD/',j,"_AOU_200",'.pdf'), width=10, height=5)
#   print(filled.contour(fit1, plot.title = title(main = c(j)),
#                        zlim = c(min(mydata$AOU), max(mydata$AOU)),
#                        ylim = c(-200,0)))
#   dev.off()
#   png(paste0('plots/CTD/',j,"_AOU_200",'.png'), width=6000, height=3000, res = 600)
#   print(filled.contour(fit1, plot.title = title(main = c(j)),
#                        zlim = c(min(mydata$AOU), max(mydata$AOU)),
#                        ylim = c(-200,0)))
#   dev.off()
#   pdf(paste0('plots/CTD/',j,"_AOU_200",i,'_lines.pdf'), width=10, height=5)
#   print(contour(fit1, plot.title = title(main = c(j))),
#         ylim = c(-200,0))
#   dev.off()
# }


# now only top 200
for (j in 1:length(sites)){
  mydata2 <- mydata %>% 
    filter(OPC_site == sites[j] & is.na(Depth)==FALSE)
  Bathy2 <- filter(Bathy, site == sitesB[j] & Bathymetry > (-(max(mydata2$Depth))-100))
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = AOU, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "AOU")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = AOU)) + 
    geom_tile(data = df, aes(fill = AOU)) +
    geom_contour(colour = "white", binwidth = 20) + 
    coord_cartesian(xlim = c(min(mydata2$Distance_Coast), max(mydata2$Distance_Coast)),
                    ylim = c(-200, 0)) +
    scale_fill_distiller(palette = "Greens", direction = 1, limits = c(min(mydata$AOU), max(mydata$AOU))) + 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth, alpha = 0.5)) +
    geom_line(data= Bathy2, aes(x = Distance_Coast, y = Bathymetry), inherit.aes = FALSE) +
    ggtitle(paste0("AOU at ", sites[j]))
  
  ggsave(paste0('plots/CTD/',sites[j],"_AOU_200m",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/CTD/',sites[j],"_AOU_200m",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}
