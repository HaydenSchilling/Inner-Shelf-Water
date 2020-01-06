### ADCP plots

#install.packages("akima")

library(akima)
library(tidyverse)
library(reshape2)

mydata <- read.csv("Data/ADP_tows_final_300419.csv")
str(mydata)
head(mydata)



### Get distance from shore
library(geosphere)

mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$OPC_site[i] == "CB") {
    mydata$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$OPC_site[i] == "DH") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$OPC_site[i] == "EH") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$OPC_site[i] == "NS") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
}


### Get Bathymetry and add distance from coast
Bathy <- read.csv("Data/Transect Bathymetry.csv", header = T)
Bathy <- subset(Bathy, Bathymetry < -1 & Bathymetry > -300)


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


 
# velocity rotation (to align with coastline):
# # Cb = 356 degree, EH = 13 degree, NS = 15 degree, DH = 19 degree

## Matlab code from amandine 
##  rot_deg_angle=-22 # to change
#  
#
## UCUR_shore=cosd(rot_deg_angle).*UCUR+sind(rot_deg_angle).*VCUR; % across-shelf 
## VCUR_shore=-sind(rot_deg_angle).*UCUR+cosd(rot_deg_angle).*VCUR; % along-shelf

mydata$U_shore = 0
mydata$V_shore = 0
for (i in 1:nrow(mydata)){
  if (mydata$OPC_site[i] == "CB") {
    rot_deg_angle= -356
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
  if (mydata$OPC_site[i] == "DH") {
    rot_deg_angle= -19
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
  if (mydata$OPC_site[i] == "EH") {
    rot_deg_angle= -13
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
  if (mydata$OPC_site[i] == "NS") {
    rot_deg_angle= -15
    mydata$U_shore[i] = cos(rot_deg_angle*pi/180)*mydata$U[i] + sin(rot_deg_angle*pi/180)*mydata$V[i]
    mydata$V_shore[i] = sin(rot_deg_angle*pi/180)*mydata$U[i] + cos(rot_deg_angle*pi/180)*mydata$V[i]
  }
}


### Test summary
datdat <- mydata %>% group_by(OPC_site) %>% summarise(maxV = min(V_shore), minV = max(V_shore),
                                                      maxU = min(U_shore), minU = max(U_shore))
datdat

#vars = c("U_shore", "V_shore", "U", "V")
sites = c("DH", "NS", "EH", "CB")
sitesB = c("DiamondHead", "NorthSolitary", "EvansHead", "CapeByron")

#get(vars)

library(ggplot2)

for (i in vars){
  
  p1 <- ggplot(mydata, aes(x = Distance_Coast, y = -Depth, col = get(i))) + geom_point() +
    facet_wrap(~OPC_site, scales = "free_x") + theme_bw()
  print(p1)
}
mydata2 <- subset(mydata, OPC_site == "EH")
str(mydata2)

## Base R code
# 
# for (j in sites){
#   mydata2 <- subset(mydata, OPC_site == j)
#   for (i in vars){
#     #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
#     fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = mydata2[[i]])
#     pdf(paste0('plots/ADCP/ADCP_',j,"_",i,'.pdf'), width=10, height=5)
#     print(filled.contour(fit1, color.palette = function(n) hcl.colors(n, "RdBu", rev = TRUE),
#                          zlim = c(-2,2), plot.title = title(main = c(j, i))))
#     dev.off()
#     png(paste0('plots/ADCP/ADCP_',j,"_",i,'.png'), width=6000, height=3000, res = 600)
#     print(filled.contour(fit1, color.palette = function(n) hcl.colors(n, "RdBu", rev = TRUE), 
#                          zlim = c(-2,2), plot.title = title(main = c(j, i))))
#     dev.off()
#     pdf(paste0('plots/ADCP/ADCP_',j,"_",i,'_lines.pdf'), width=10, height=5)
#     print(contour(fit1, plot.title = title(main = c(j, i))))
#     dev.off()
#     png(paste0('plots/ADCP/ADCP_',j,"_",i,'_lines.png'), width=6000, height=3000, res = 600)
#     print(contour(fit1, plot.title = title(main = c(j, i))))
#     dev.off()
#   }
# }


### V_Shore interpoLation and plots
for (j in 1:length(sites)){
  mydata2 <- mydata %>% 
    filter(OPC_site == sites[j] & is.na(Depth)==FALSE)
  Bathy2 <- filter(Bathy, site == sitesB[j])
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = V_shore, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "V_shore")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = V_shore)) + 
    geom_tile(data = df, aes(fill = V_shore)) +
    geom_contour(colour = "white", binwidth = 0.125) + 
    scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(-2,  2)) + 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth, alpha = 0.5)) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-300), inherit.aes = FALSE, fill = "grey60") +
    ggtitle(paste0("V_shore at ", sites[j]))
  
  ggsave(paste0('plots/ADCP/',sites[j],"_V_shore",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/ADCP/',sites[j],"_V_shore",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}


### U_Shore interpoLation and plots
for (j in 1:length(sites)){
  mydata2 <- mydata %>% 
    filter(OPC_site == sites[j] & is.na(Depth)==FALSE)
  Bathy2 <- filter(Bathy, site == sitesB[j])
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = U_shore, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "U_shore")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = U_shore)) + 
    geom_tile(data = df, aes(fill = U_shore)) +
    geom_contour(colour = "white", binwidth = 0.125) + 
    scale_fill_distiller(palette = "RdBu", direction = -1, limits = c(-0.5,  0.5)) + 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth, alpha = 0.5)) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-300), inherit.aes = FALSE, fill = "grey60") +
    ggtitle(paste0("U_shore at ", sites[j]))
  
  ggsave(paste0('plots/ADCP/',sites[j],"_U_shore",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/ADCP/',sites[j],"_U_shore",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}



#### Quiver plots
library(dplyr)
#install.packages("ggquiver")
library(ggquiver)

top_dat <- mydata %>% group_by(Lat, Lon) %>% filter(Depth == min(Depth))
head(top_dat)

library(ggplot2)

#Load map data
library(rgdal)
library(raster)
Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
#plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea" )

head(Aus_coast)

#plot(Aus_coast)

min_lon <- 150
max_lon <- 155
min_lat <- -32
max_lat <- -28

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)

Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run

shelf <- read.csv("Hayden_200m_contour.csv", header = T)
shelf <- subset(shelf, Var2 >= -32)
shelf <- subset(shelf, Var2 <= -28)
shelf <- subset(shelf, Var1 > 150)



pQ <- ggplot(top_dat, aes(x = Lon, y = Lat)) + geom_point() +
  coord_quickmap() + #coord_map()
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), 
               fill = "gray60", colour = "gray60")+ labs(x = "Longitude", y = "Latitude")+
  geom_quiver(aes(u = U, v = V), center = T, vecsize = 1000) + theme_classic()+
  geom_path(data=shelf, aes(x=Var1, y = Var2)) + 
  scale_x_continuous(expand = c(0,0.1))+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position=c(0.92,0.05), legend.direction = "horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())
pQ

ggsave("plots/ADCP/Surface Velocity Map.pdf", height = 15, width = 12, units = "cm")
ggsave("plots/ADCP/Surface Velocity Map.png", height = 15, width = 12, units = "cm", dpi = 600)
