### ADCP plots

#install.packages("akima")

library(akima)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(patchwork)
library(metR)
library(geosphere)

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

sites = c("CB", "EH",  "NS", "DH")
sitesB = c("CapeByron", "EvansHead", "NorthSolitary","DiamondHead" )
sites <- sites[c(1,2,3,4)]

Bathy$site2[Bathy$site == "CapeByron"] <- "CB"
Bathy$site2[Bathy$site == "EvansHead"] <- "EH"
Bathy$site2[Bathy$site == "NorthSolitary"] <- "NS"
Bathy$site2[Bathy$site == "DiamondHead"] <- "DH"

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

### Prepare Temp to overlay

mydataT <- read_csv("Data/SS2004_SeaSoarData.csv")
#str(mydataT)
#head(mydataT)

mydataT <- mydataT %>%
  mutate(site = case_when(site = str_detect(File,"SS0408_023") ~ "CapeByron",
                          site = str_detect(File,"SS0408_021") ~ "EvansHead",
                          site = str_detect(File,"SS0408_010") ~ "NorthSolitary",
                          site = str_detect(File,"SS0408_008") ~ "DiamondHead"),
         site = as.factor(site))

mydataT <- subset(mydataT, Depth > 13)

### Get distance from shore
mydataT$Distance_Coast = 0
for (i in 1:nrow(mydataT)){
  if (mydataT$site[i] == "CapeByron") {
    mydataT$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydataT$Lon[i], mydataT$Lat[i]), fun = distHaversine)
  }
  if (mydataT$site[i] == "DiamondHead") {
    mydataT$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydataT$Lon[i], mydataT$Lat[i]), fun = distHaversine)
  }
  if (mydataT$site[i] == "EvansHead") {
    mydataT$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydataT$Lon[i], mydataT$Lat[i]), fun = distHaversine)
  }
  if (mydataT$site[i] == "NorthSolitary") {
    mydataT$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydataT$Lon[i], mydataT$Lat[i]), fun = distHaversine)
  }
}


# # variables to loop through
# vars = c("Temp", "Salt","GeoMn")# "NBSS.Slope",
# #sites to loop through
sitesT <- levels(mydataT$site)
sitesT <- sitesT[c(1,3,4,2)]



site_labels <- c("Cape Byron (28.6°S)", "Evans Head (29°S)" ,"North Solitary (30°S)", "Diamond Head (31.7°S)")

### V_Shore interpoLation and plots


### NEW Plotting
pl <- list()

for (j in sites){
  mydata2 <- mydata %>%
    filter(OPC_site == j & is.na(Depth) == FALSE & is.na(V_shore)==FALSE) %>%
    select(c(Distance_Coast, Depth, V_shore, OPC_site)) #, cast_no, Temp # from biomass code
  
  # Get the max depth of each cast
  Limits <- mydata2 %>%
    group_by(OPC_site) %>%
    summarise(maxD = max(Depth),
              minD = min(Depth),
              Distance_Coast = Distance_Coast[1]) %>%
    ungroup()
  
  # #Limits2 <- tibble(maxD = c(250, 250),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   OPC_site = c(max(Limits$OPC_site), max(Limits$OPC_site)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site2 == j)
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = V_shore, nx = 100, ny = 100))
  #fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Alongshore_Velocity")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  #df2 <- melt(fit2$z, na.rm = TRUE)
  #names(df2) <- c("x", "y", "Temp")
  #df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = Alongshore_Velocity)) +
    geom_tile(aes(fill = Alongshore_Velocity)) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Alongshore_Velocity), colour = "grey10", binwidth = 0.2, size = 0.2) +
    #geom_contour(aes(x = Distance_Coast, y = Depth, z = Temp), colour = "red", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Alongshore_Velocity), breaks = seq(-1.8, 1.6, by = 0.2)) + # 
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-1.5,  1.5), oob = scales::squish,
                         name=expression(bold("Alongshore Velocity "(m~s^-1)))) +
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE, inherit.aes = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-300), inherit.aes = FALSE, fill = "grey60") +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    #scale_x_continuous( expand = c(0, 0)) + #limits = c(12, 48),
    #scale_y_continuous(expand = c(0, 0), limits = c(-250,0)) +
    coord_cartesian(xlim = c(12, 48), ylim = c(-250,0), expand = TRUE,
                    default = FALSE, clip = "on") +
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}


letters <- c("A) ", "B) ", "C) ", "D) " )



#Add Temperature line (21 deg C isotherm from Seasoar)
for (j in 1:length(sitesT)){
  mydata2T <- mydataT %>%
    filter(site == sitesT[j] & is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
    select(c(Distance_Coast, Depth, Biomass, cast_no, Temp))
  
  pl[[j]]  
  fit2 <- with(mydata2T, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df2$Distance_Coast <- fit2$x[df2$x]
  df2$Depth <- fit2$y[df2$y]
  
  pl[[j]] <- pl[[j]] + geom_contour(data = df2, inherit.aes = F,
                                    aes(x = Distance_Coast/1000, y = Depth, z = Temp),
                                    colour = "red", breaks = c(21), size = 1) +
    #geom_text_contour(data = df2, inherit.aes = F,
    #  aes(x = Distance_Coast/1000, y = Depth, z = Temp), 
    #  breaks = c(21), colour = "brown")
    geom_text(x = 12.01, y = -230, 
              label = paste0(letters[j],site_labels[j]), stat = "identity", 
              inherit.aes = FALSE, hjust = 0)
  
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/ADCP/V_rotated_All_with Temp','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/ADCP/V_rotated_All_with_Temp','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")

### U_Shore interpoLation and plots
pl <- list()

for (j in sites){
  mydata2 <- mydata %>%
    filter(OPC_site == j & is.na(Depth) == FALSE & is.na(U_shore)==FALSE) %>%
    select(c(Distance_Coast, Depth, U_shore, OPC_site)) #, cast_no, Temp # from biomass code
  
  # Get the max depth of each cast
  Limits <- mydata2 %>%
    group_by(OPC_site) %>%
    summarise(maxD = max(Depth),
              minD = min(Depth),
              Distance_Coast = Distance_Coast[1]) %>%
    ungroup()
  
  # #Limits2 <- tibble(maxD = c(250, 250),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   OPC_site = c(max(Limits$OPC_site), max(Limits$OPC_site)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site2 == j)
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = U_shore, nx = 100, ny = 100))
  #fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Cross_shelf_Velocity")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  #df2 <- melt(fit2$z, na.rm = TRUE)
  #names(df2) <- c("x", "y", "Temp")
  #df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = Cross_shelf_Velocity)) +
    geom_tile(aes(fill = Cross_shelf_Velocity)) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Cross_shelf_Velocity), colour = "grey10", binwidth = 0.2, size = 0.2) +
    #geom_contour(aes(x = Distance_Coast, y = Depth, z = Temp), colour = "red", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Cross_shelf_Velocity), breaks = seq(-1.8, 1.6, by = 0.2)) + # 
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-0.25,  0.25), oob = scales::squish,
                         name=expression(bold("Cross Shelf Velocity "(m~s^-1)))) +
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE, inherit.aes = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-300), inherit.aes = FALSE, fill = "grey60") +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    #scale_x_continuous( expand = c(0, 0)) + #limits = c(12, 48),
    #scale_y_continuous(expand = c(0, 0), limits = c(-250,0)) +
    coord_cartesian(xlim = c(12, 48), ylim = c(-250,0), expand = TRUE,
                    default = FALSE, clip = "on") +
    guides(size = "none", shape = "none")+
    theme(legend.title = element_text(angle = 270, hjust= 0.5),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}


letters <- c("A) ", "B) ", "C) ", "D) " )



#Add Temperature line (21 deg C isotherm from Seasoar)
for (j in 1:length(sitesT)){
  mydata2T <- mydataT %>%
    filter(site == sitesT[j] & is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
    select(c(Distance_Coast, Depth, Biomass, cast_no, Temp))
  
  pl[[j]]  
  fit2 <- with(mydata2T, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df2$Distance_Coast <- fit2$x[df2$x]
  df2$Depth <- fit2$y[df2$y]
  
  pl[[j]] <- pl[[j]] + geom_contour(data = df2, inherit.aes = F,
                                    aes(x = Distance_Coast/1000, y = Depth, z = Temp),
                                    colour = "red", breaks = c(21), size = 1) +
    #geom_text_contour(data = df2, inherit.aes = F,
    #  aes(x = Distance_Coast/1000, y = Depth, z = Temp), 
    #  breaks = c(21), colour = "brown")
    geom_text(x = 12.01, y = -230, 
              label = paste0(letters[j],site_labels[j]), stat = "identity", 
              inherit.aes = FALSE, hjust = 0)
  
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/ADCP/U_rotated_All_with Temp','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/ADCP/U_rotated_All_with_Temp','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")




# #### Quiver plots
# library(dplyr)
# #install.packages("ggquiver")
# library(ggquiver)
# 
# top_dat <- mydata %>% group_by(Lat, Lon) %>% filter(Depth == min(Depth))
# head(top_dat)
# 
# library(ggplot2)
# 
# #Load map data
# library(rgdal)
# library(raster)
# Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
# #plot(Aus)
# Aus_coast <- subset(Aus, FEAT_CODE != "sea" )
# 
# head(Aus_coast)
# 
# #plot(Aus_coast)
# 
# min_lon <- 150
# max_lon <- 155
# min_lat <- -32
# max_lat <- -28
# 
# geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
# 
# Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
#                           lat_bound = c(geo_bounds[2], geo_bounds[4]))
# 
# coordinates(Sites.grid) <- ~ lon_bound + lat_bound
# 
# Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run
# 
# shelf <- read.csv("Hayden_200m_contour.csv", header = T)
# shelf <- subset(shelf, Var2 >= -32)
# shelf <- subset(shelf, Var2 <= -28)
# shelf <- subset(shelf, Var1 > 150)
# 
# 
# 
# pQ <- ggplot(top_dat, aes(x = Lon, y = Lat)) + geom_point() +
#   coord_quickmap() + #coord_map()
#   geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), 
#                fill = "gray60", colour = "gray60")+ labs(x = "Longitude", y = "Latitude")+
#   geom_quiver(aes(u = U, v = V), center = T, vecsize = 1000) + theme_classic()+
#   geom_path(data=shelf, aes(x=Var1, y = Var2)) + 
#   scale_x_continuous(expand = c(0,0.1))+
#   scale_y_continuous(expand = c(0,0))+
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#         axis.text.x  = element_text(colour="black", size = 12), 
#         axis.title.y = element_text(face="bold", colour="black", size = 18),
#         axis.text.y  = element_text(colour="black", size = 14),
#         axis.ticks = element_line(colour="black"),
#         strip.text = element_text(colour="black", face = "bold", size = 14),
#         strip.background = element_rect(colour = NULL),
#         legend.justification=c(1,0), legend.position=c(0.92,0.05), legend.direction = "horizontal",
#         panel.border = element_rect(colour = "black", fill=NA, size = 1),
#         legend.key.size = unit(1, "cm"),
#         legend.text = element_text(size = 12, face = "bold"),
#         legend.title=element_blank(),
#         legend.background = element_blank())
# pQ
# 
# ggsave("plots/ADCP/Surface Velocity Map.pdf", height = 15, width = 12, units = "cm")
# ggsave("plots/ADCP/Surface Velocity Map.png", height = 15, width = 12, units = "cm", dpi = 600)
