# Chemical Oceanography Interpolations


library(akima)
#library(ggplot2)
library(reshape2)
library(tidyverse)
library(patchwork)
library(metR)
library(geosphere)


mydata <- read_csv("Data/SS2004_SeaSoarData.csv")
str(mydata)
head(mydata)

mydata <- mydata %>%
  mutate(site = case_when(site = str_detect(File,"SS0408_023") ~ "CapeByron",
                          site = str_detect(File,"SS0408_021") ~ "EvansHead",
                          site = str_detect(File,"SS0408_010") ~ "NorthSolitary",
                          site = str_detect(File,"SS0408_008") ~ "DiamondHead"),
         site = as.factor(site))

mydata <- subset(mydata, Depth > 13)

# # Make summary table of transects (where and when)
# library(lubridate)
# mydata$datestr <- dmy_hms(mydata$datestr)
# 
# sum_table <- mydata %>% group_by(site) %>% summarise(start_lon = min(Lon), end_lon = max(Lon),
#                                                      start_time = min(datestr), end_time = max(datestr))
# sum_table
# 
# sum_table$start_time_local <- with_tz(sum_table$start_time, tzone = "Australia/Sydney") 
# sum_table$end_time_local <- with_tz(sum_table$end_time, tzone = "Australia/Sydney") 
# 
# sum_table
# 
# #write.csv(sum_table, "Transect Summary table.csv", row.names = F)
#          
# # get Latitudes to go with start and ends
# sum_dat2 <- mydata %>% filter(site == "Cape Byron (28.6° S)")
# sum_dat2 <- mydata %>% filter(site == "Evans Head (29° S)")
# sum_dat2 <- mydata %>% filter(site == "North Solitary (30° S)")
# sum_dat2 <- mydata %>% filter(site == "Diamond Head (31.75° S)")

### Get distance from shore
mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "CapeByron") {
    mydata$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "DiamondHead") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EvansHead") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NorthSolitary") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
}


### Get Bathymetry and add distance from coast
Bathy <- read.csv("Data/Transect Bathymetry.csv", header = T)
Bathy <- subset(Bathy, Bathymetry < -1 & Bathymetry >= -200)


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



# # variables to loop through
# vars = c("Temp", "Salt","GeoMn")# "NBSS.Slope",
# #sites to loop through
sites <- levels(mydata$site)
sites <- sites[c(1,3,4,2)]

#label_list <- c("Cape Byron (28.6° S)", "Evans Head (29° S)", "North Solitary (30° S)", "Diamond Head (31.75° S)")
site_labels <- c("Cape Byron (28.6°S)", "Evans Head (29°S)" ,"North Solitary (30°S)", "Diamond Head (31.7°S)")

letters <- c("A) ", "B) ", "C) ", "D) " )

### Biomass interpoLation and plots
pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Salt)==FALSE) %>%
    select(c(Distance_Coast, Depth, Salt, cast_no, Temp))
  
  # Get the max depth of each cast
  Limits <- mydata2 %>%
    group_by(cast_no) %>%
    summarise(maxD = max(Depth),
              minD = min(Depth),
              Distance_Coast = Distance_Coast[1]) %>%
    ungroup()
  
  Limits2 <- tibble(maxD = c(150, 150),
                    minD = c(0, 0),
                    Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
                    cast_no = c(max(Limits$cast_no), max(Limits$cast_no)))
  
  Limits <- Limits[order(Limits$Distance_Coast),]
  Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Salt, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Salt")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Salt))) +
    geom_tile(aes(fill = Salt)) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(35.43,  35.72),# oob = scales::squish,# trans = "log10",
                         name="Salinity")+#name=expression(bold("Biomass "(mg~m^-3)))) + # 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -120, label = paste0(letters[j],"Salinity at \n", site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(13, 48), expand = TRUE, ylim = c(-140,-10)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-125,-100,-75,-50,-25)) +
    #theme(legend.title.align = 0)+
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Salt_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Salt_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")




### Nutrients
# Nitrate
mydata <- read_csv("Data/HYD cleaned 080419.csv")
str(mydata)
head(mydata)

mydata <- mydata %>%
  mutate(site = as.factor(OPC_site), Lat = lat3, Lon = long3, Temp=CTD.Temp)

#mydata <- subset(mydata, Depth > 13)

### Get distance from shore
mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "CB") {
    mydata$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "DH") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EH") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NS") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
}


### Get Bathymetry and add distance from coast
Bathy <- read.csv("Data/Transect Bathymetry.csv", header = T)
Bathy <- subset(Bathy, Bathymetry < -1 & Bathymetry >= -200)


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

Bathy <- Bathy %>% mutate(site = case_when(site == "DiamondHead" ~ "DH",
                                           site == "EvansHead" ~ "EH",
                                           site == "NorthSolitary" ~ "NS"))

# # variables to loop through
# vars = c("Temp", "Salt","GeoMn")# "NBSS.Slope",
# #sites to loop through
sites <- levels(mydata$site)
sites
sites <- sites[c(2,3,1)]

#label_list <- c("Cape Byron (28.6° S)", "Evans Head (29° S)", "North Solitary (30° S)", "Diamond Head (31.75° S)")
site_labels <- c("Evans Head (29°S)" ,"North Solitary (30°S)", "Diamond Head (31.7°S)")

letters <- c("A) ", "B) ", "C) ")

### Nitrate interpoLation and plots
pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Nitrate)==FALSE & Depth <200) %>%
    select(c(Distance_Coast, Depth, Nitrate, Sal_Bot, Temp))
  
  # Get the max depth of each cast
  # Limits <- mydata2 %>%
  #   group_by(Sal_Bot) %>%
  #   summarise(maxD = max(Depth),
  #             minD = min(Depth),
  #             Distance_Coast = Distance_Coast[1]) %>%
  #   ungroup()
  # 
  # Limits2 <- tibble(maxD = c(150, 150),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   cast_no = c(max(Limits$Sal_Bot), max(Limits$Sal_Bot)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Nitrate, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Nitrate")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Nitrate))) +
    geom_tile(aes(fill = Nitrate)) + ylab("Depth (m)") +
    #geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(0, 15),# oob = scales::squish,# trans = "log10",
                         name="Nitrate")+#name=expression(bold("Biomass "(mg~m^-3)))) + # 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 10.010, y = -180, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(10, 48), expand = TRUE, ylim = c(-200,0)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-200,-150,-100,-50,0)) +
    #theme(legend.title.align = 0)+
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12, face="bold"),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[3]] <- pl[[3]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(2.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Nitrate_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Nitrate_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")

#######
# Phosphate
### Phosphate interpoLation and plots
pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Phosphate)==FALSE & Depth <200) %>%
    select(c(Distance_Coast, Depth, Phosphate, Sal_Bot, Temp))
  
  # Get the max depth of each cast
  # Limits <- mydata2 %>%
  #   group_by(Sal_Bot) %>%
  #   summarise(maxD = max(Depth),
  #             minD = min(Depth),
  #             Distance_Coast = Distance_Coast[1]) %>%
  #   ungroup()
  # 
  # Limits2 <- tibble(maxD = c(150, 150),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   cast_no = c(max(Limits$Sal_Bot), max(Limits$Sal_Bot)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Phosphate, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Phosphate")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Phosphate))) +
    geom_tile(aes(fill = Phosphate)) + ylab("Depth (m)") +
    #geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(0, 1),# oob = scales::squish,# trans = "log10",
                         name="Phosphate")+#name=expression(bold("Biomass "(mg~m^-3)))) + # 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 10.010, y = -120, label = paste0(letters[j],"Phosphate at \n", site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(10, 48), expand = TRUE, ylim = c(-200,0)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-200,-150,-100,-50,0)) +
    #theme(legend.title.align = 0)+
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[3]] <- pl[[3]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(2.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Phosphate_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Phosphate_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")

## Silicate
### Silicate interpoLation and plots
pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Silicate)==FALSE & Depth <200) %>%
    select(c(Distance_Coast, Depth, Silicate, Sal_Bot, Temp))
  
  # Get the max depth of each cast
  # Limits <- mydata2 %>%
  #   group_by(Sal_Bot) %>%
  #   summarise(maxD = max(Depth),
  #             minD = min(Depth),
  #             Distance_Coast = Distance_Coast[1]) %>%
  #   ungroup()
  # 
  # Limits2 <- tibble(maxD = c(150, 150),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   cast_no = c(max(Limits$Sal_Bot), max(Limits$Sal_Bot)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Silicate, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Silicate")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Silicate))) +
    geom_tile(aes(fill = Silicate)) + ylab("Depth (m)") +
    #geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(0, 6),# oob = scales::squish,# trans = "log10",
                         name="Silicate")+#name=expression(bold("Biomass "(mg~m^-3)))) + # 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 10.010, y = -180, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(10, 48), expand = TRUE, ylim = c(-200,0)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-200,-150,-100,-50,0)) +
    #theme(legend.title.align = 0)+
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12, face="bold"),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[3]] <- pl[[3]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(2.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Silicate_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Silicate_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


### Oxygen
### Oxygen interpoLation and plots
pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Oxygen)==FALSE & Depth <200) %>%
    select(c(Distance_Coast, Depth, Oxygen, Sal_Bot, Temp))
  
  # Get the max depth of each cast
  # Limits <- mydata2 %>%
  #   group_by(Sal_Bot) %>%
  #   summarise(maxD = max(Depth),
  #             minD = min(Depth),
  #             Distance_Coast = Distance_Coast[1]) %>%
  #   ungroup()
  # 
  # Limits2 <- tibble(maxD = c(150, 150),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   cast_no = c(max(Limits$Sal_Bot), max(Limits$Sal_Bot)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Oxygen, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Oxygen")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Oxygen))) +
    geom_tile(aes(fill = Oxygen)) + ylab("Depth (m)") +
    #geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(180, 250),# oob = scales::squish,# trans = "log10",
                         name="Oxygen")+#name=expression(bold("Biomass "(mg~m^-3)))) + # 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 10.010, y = -180, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(10, 48), expand = TRUE, ylim = c(-200,0)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-200,-150,-100,-50,0)) +
    #theme(legend.title.align = 0)+
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12, face="bold"),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[3]] <- pl[[3]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(2.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Oxygen_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Oxygen_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


### CHL_a Interpolations
# CHL_a
mydata <- read_csv("Data/CHL_a_Cleaned.csv")
str(mydata)
head(mydata)

mydata <- mydata %>%
  mutate(site = as.factor(Site), Lat = START_LAT, Lon = START_LON, Temp=TEMPERATURE, Depth=PRESSURE)
levels(mydata$site)

#mydata <- subset(mydata, Depth > 13)

### Get distance from shore
mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "CB") {
    mydata$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "Diamond Head") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "Evans Head") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "North Solitary") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
}


### Get Bathymetry and add distance from coast
Bathy <- read.csv("Data/Transect Bathymetry.csv", header = T)
Bathy <- subset(Bathy, Bathymetry < -1 & Bathymetry >= -200)


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

Bathy <- Bathy %>% mutate(site = case_when(site == "DiamondHead" ~ "Diamond Head",
                                           site == "EvansHead" ~ "Evans Head",
                                           site == "NorthSolitary" ~ "North Solitary"))

# # variables to loop through
# vars = c("Temp", "Salt","GeoMn")# "NBSS.Slope",
# #sites to loop through
sites <- levels(mydata$site)
sites
sites <- sites[c(2,3,1)]

#label_list <- c("Cape Byron (28.6° S)", "Evans Head (29° S)", "North Solitary (30° S)", "Diamond Head (31.75° S)")
site_labels <- c("Evans Head (29°S)" ,"North Solitary (30°S)", "Diamond Head (31.7°S)")

letters <- c("A) ", "B) ", "C) ")

pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Chl_a)==FALSE & Depth <200) %>%
    select(c(Distance_Coast, Depth, Chl_a, Temp))
  
  # Get the max depth of each cast
  # Limits <- mydata2 %>%
  #   group_by(Sal_Bot) %>%
  #   summarise(maxD = max(Depth),
  #             minD = min(Depth),
  #             Distance_Coast = Distance_Coast[1]) %>%
  #   ungroup()
  # 
  # Limits2 <- tibble(maxD = c(150, 150),
  #                   minD = c(0, 0),
  #                   Distance_Coast = c(max(Limits$Distance_Coast), min(Limits$Distance_Coast)),
  #                   cast_no = c(max(Limits$Sal_Bot), max(Limits$Sal_Bot)))
  # 
  # Limits <- Limits[order(Limits$Distance_Coast),]
  # Limits <- rbind(Limits, Limits2)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Chl_a, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Chl_a")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Chl_a))) +
    geom_tile(aes(fill = Chl_a)) + ylab("Depth (m)") +
    #geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(1, 1.5),# oob = scales::squish,# trans = "log10",
                         name=expression(bold("Chlorophyll"~italic("a"))))+#name=expression(bold("Biomass "(mg~m^-3)))) + # 
    #geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 10.010, y = -180, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(10, 48), expand = TRUE, ylim = c(-200,0)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-200,-150,-100,-50,0)) +
    #theme(legend.title.align = 0)+
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[3]] <- pl[[3]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(2.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Chl_a_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Chl_a_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")

