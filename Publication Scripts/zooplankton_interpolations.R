# Zooplankton data interpolation

# Interpolating plots

library(akima)
library(ggplot2)
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

## Summary Table
sum_table_biomass <- mydata %>%
  filter(is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
  select(c(site, Distance_Coast, Depth, Biomass, cast_no,
           Temp, ParetoSlope, Abundance, GeoMn)) %>%
  mutate(DepthBin = case_when(Depth<25 ~ "<25m",
                              Depth>50 ~ ">50",
                              T ~ "25-50m"),
         GeoMn = GeoMn* 1e3*1000) %>%
  group_by(site, DepthBin) %>% summarise(mean_biomass = mean(Biomass),
                                   sd_biomass = sd(Biomass),
                                   mean_abund = mean(Abundance),
                                   sd_abund = sd(Abundance),
                                   mean_pareto = mean(ParetoSlope),
                                   sd_pareto = sd(ParetoSlope),
                                   mean_GMS = mean(GeoMn),
                                   sd_GMS = sd(GeoMn),
                                   n = n())
write_csv(sum_table_biomass, "Other prepublication stuff/Round 14 JGR Oceans/Reviews/summary by depth and area.csv")

sum_table_biomass <- mydata %>%
  filter(is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
  select(c(site, Distance_Coast, Depth, Biomass, cast_no,
           Temp, ParetoSlope, Abundance, GeoMn)) %>%
  mutate(DepthBin = case_when(Depth<25 ~ "<25m",
                              Depth>50 ~ ">50",
                              T ~ "25-50m"),
         GeoMn = GeoMn* 1e3*1000) %>%
  group_by(site) %>% summarise(mean_biomass = mean(Biomass),
                                         sd_biomass = sd(Biomass),
                                         mean_abund = mean(Abundance),
                                         sd_abund = sd(Abundance),
                                         mean_pareto = mean(ParetoSlope),
                                         sd_pareto = sd(ParetoSlope),
                                         mean_GMS = mean(GeoMn),
                                         sd_GMS = sd(GeoMn),
                                         n = n())
write_csv(sum_table_biomass, "Other prepublication stuff/Round 14 JGR Oceans/Reviews/summary by area.csv")

sum_table_temp <- mydata %>%
  filter(is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
  select(c(site, Distance_Coast, Depth, Biomass, cast_no,
           Temp, ParetoSlope, Abundance, GeoMn)) %>%
  mutate(DepthBin = case_when(Depth<25 ~ "<25m",
                              Depth>50 ~ ">50",
                              T ~ "25-50m"),
         GeoMn = GeoMn* 1e3*1000,
         Ocean_Zone = case_when(Temp <20 ~ "Tasman",
                                Temp >21 ~ "EAC",
                                T ~ "Frontal")) %>%
  group_by(site, Ocean_Zone) %>% summarise(mean_biomass = mean(Biomass),
                               sd_biomass = sd(Biomass),
                               mean_abund = mean(Abundance),
                               sd_abund = sd(Abundance),
                               mean_pareto = mean(ParetoSlope),
                               sd_pareto = sd(ParetoSlope),
                               mean_GMS = mean(GeoMn),
                               sd_GMS = sd(GeoMn),
                               n = n())
write_csv(sum_table_temp, "Other prepublication stuff/Round 14 JGR Oceans/Reviews/summary by area and zone.csv")

sum_table_temp2 <- mydata %>%
  filter(is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
  select(c(site, Distance_Coast, Depth, Biomass, cast_no,
           Temp, ParetoSlope, Abundance, GeoMn)) %>%
  mutate(DepthBin = case_when(Depth<25 ~ "<25m",
                              Depth>50 ~ ">50",
                              T ~ "25-50m"),
         GeoMn = GeoMn* 1e3*1000,
         Ocean_Zone = case_when(Temp <20 ~ "Tasman",
                                Temp >21 ~ "EAC",
                                T ~ "Frontal")) %>%
  group_by(site, Ocean_Zone, DepthBin) %>% summarise(mean_biomass = round(mean(Biomass),2),
                                           sd_biomass = round(sd(Biomass),2),
                                           mean_abund = round(mean(Abundance),2),
                                           sd_abund = round(sd(Abundance),2),
                                           mean_pareto = round(mean(ParetoSlope),2),
                                           sd_pareto = round(sd(ParetoSlope),2),
                                           mean_GMS = round(mean(GeoMn),2),
                                           sd_GMS = round(sd(GeoMn),2),
                                           n = n())
write_csv(sum_table_temp2, "Other prepublication stuff/Round 14 JGR Oceans/Reviews/summary by area and zone and depth.csv")

### DVM plot for reviewer
EAC_water <- mydata %>%
  filter(is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
  select(c(site, Distance_Coast, Depth, Biomass, cast_no,
           Temp, ParetoSlope, Abundance, GeoMn)) %>%
  mutate(DepthBin = case_when(Depth<25 ~ "<25m",
                              Depth>50 ~ ">50m",
                              T ~ "25-50m"),
         GeoMn = GeoMn* 1e3*1000,
         Ocean_Zone = case_when(Temp <20 ~ "Tasman",
                                Temp >21 ~ "EAC",
                                T ~ "Frontal")) %>% filter(Ocean_Zone == "EAC") %>%
  pivot_longer(cols=c(Biomass, ParetoSlope, Abundance, GeoMn),names_to = "Parameter", values_to = "value")

ggplot(EAC_water, aes(site,value, col = factor(DepthBin, level = c("<25m", "25-50m",">50m")))) +
  geom_boxplot()+ facet_wrap(~Parameter, scales = "free")+
  scale_color_viridis_d(name="Depth") + theme_bw()+
  scale_x_discrete(labels=c("Cape Byron\n(Day)", "Evans Head\n(Day)","North Solitary\n(Night)"))+
  xlab("Transect")+
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        legend.title = element_text(face="bold", size=12))

#ggsave("Other prepublication stuff/plots/zoop/EAC Core Day Night Comparison.png", width=21, height=14.8, units="cm", dpi=600)
#ggsave("Other prepublication stuff/plots/zoop/EAC Core Day Night Comparison.pdf", width=21, height=14.8, units="cm", dpi=600)

### Biomass interpoLation and plots
pl <- list()

for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
    select(c(Distance_Coast, Depth, Biomass, cast_no, Temp))

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

  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Biomass, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))

  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Biomass")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]

  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp

  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = (Biomass))) +
    geom_tile(aes(fill = Biomass)) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(10,  1000), oob = scales::squish, trans = "log10",
                         name=expression(bold("Biomass "(mg~m^-3)~or~Biovolume~(mm^3~m^-3)))) + # 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -130, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
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

#ggsave(paste0('Other prepublication stuff/plots/zoop/Biomass_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
#ggsave(paste0('Other prepublication stuff/plots/zoop/Biomass_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


### Geo_Mn Size #  done
### interpoLation and plots
site_labels

pl <- list()
for (j in 1:length(sites)){
  mydata2 <- filter(mydata, site == sites[j])
  mydata2 <-  mydata2 %>%
    drop_na(GeoMn, Depth) %>%
    filter(GeoMn != Inf) %>%
    mutate(GeoMn = GeoMn * 1e3)

  Bathy2 <- filter(Bathy, site == sites[j])

  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass),
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = GeoMn*1000, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))

  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "GeoMn")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]

  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df2$Depth <- fit2$y[df2$y]
  df2$Distance_Coast <- fit2$x[df2$x]

  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = GeoMn)) +
    geom_tile(aes(fill = GeoMn)) + ylab("Depth (m)") + 
    geom_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(400, 500), oob = scales::squish,
                         name=expression(bold("Geometric Mean Size (µm ESD)"))) +
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 11.5, y = -130, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(13, 48), expand = TRUE, ylim = c(-140,-10)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-125,-100,-75,-50,-25)) +
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/GeoMn_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/GeoMn_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")



### Pareto Slope #  done
pl <- list()
for (j in 1:length(sites)){
  mydata2 <- filter(mydata, site == sites[j])
  mydata2 <-  mydata2 %>%
    drop_na(ParetoSlope) %>%
    filter(ParetoSlope != Inf)

  Bathy2 <- filter(Bathy, site == sites[j])

  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = ParetoSlope, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))

  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "ParetoSlope")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]

  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df2$Depth <- fit2$y[df2$y]
  df2$Distance_Coast <- fit2$x[df2$x]


  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = ParetoSlope)) +
    geom_tile(aes(fill = ParetoSlope)) + ylab("Depth (m)") +
    geom_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-1.4, -0.8), oob = scales::squish,
                         name=expression(bold("Pareto Shape Parameter")~bolditalic("c"))) +
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth),alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.000, y = -130, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(13, 48), expand = TRUE, ylim = c(-140,-10)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-125,-100,-75,-50,-25)) +
    guides(size = "none", shape = "none") +
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))

}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect') & theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/ParetoSlope_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/ParetoSlope_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


### NBSS plots
pl <- list()
for (j in 1:length(sites)){
  mydata2 <- filter(mydata, site == sites[j])
  mydata2 <-  mydata2 %>%
    drop_na(NBSSSlope) %>%
    filter(NBSSSlope != Inf)
  
  Bathy2 <- filter(Bathy, site == sites[j])
  
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = NBSSSlope, nx = 100, ny = 100))
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "NBSSSlope")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df2$Depth <- fit2$y[df2$y]
  df2$Distance_Coast <- fit2$x[df2$x]
  
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = NBSSSlope)) +
    geom_tile(aes(fill = NBSSSlope)) + ylab("Depth (m)") +
    geom_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(data = df2, aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25)) +
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-1.4, -0.8), oob = scales::squish,
                         name=expression(bold("NBSS Slope"))) +
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth),alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -130, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black"),
          axis.title = element_text(face = "bold")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(13, 48), expand = TRUE, ylim = c(-140,-10)) +
    #scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-125,-100,-75,-50,-25)) +
    guides(size = "none", shape = "none")+
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')& theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/NBSS_Slope_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/NBSS_Slope_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")

cor.test(mydata$NBSSSlope, mydata$ParetoSlope)
plot(mydata$NBSSSlope, mydata$ParetoSlope)

### Abundance #  correlated to other metrics
pl <- list()
for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Abundance)==FALSE & is.finite(Abundance)==TRUE & Abundance > 0) %>%
    select(c(Distance_Coast, Depth, Abundance, cast_no, Temp))

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

  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Abundance, nx = 100, ny = 100)) #z = log10(Abundance)
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))

  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Abundance")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]

  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp

  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = Abundance)) +
    geom_tile(aes(fill = (Abundance))) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25,1)) +
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth),alpha = 0.5, size = 0.2) +
    scale_fill_distiller(palette = "Spectral", direction = -1,limits = c(1000,  11000), oob = scales::squish,
                         name =expression(bold("Abundance "(individuals~m^-3)))) + #
    #scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-1.4, -0.8), oob = scales::squish,
    #                     name=expression(bold("NBSS Slope"))) +geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -130, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(13, 48), expand = TRUE, ylim = c(-140,-10)) +
    #scale_x_continuous(limits = c(12000, 48000), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-125,-100,-75,-50,-25)) +
    guides(size = "none", shape = "none")+
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')& theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Abundance_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Abundance_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")





## Salinity Interpolations

### Salinity #  done
pl <- list()
for (j in 1:length(sites)){
  mydata2 <- mydata %>%
    filter(site == sites[j] & is.na(Depth) == FALSE & is.na(Salt) == FALSE & is.finite(Salt) == TRUE & Salt > 0) %>%
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
  
  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast/1000, y = Depth, z = Salt)) +
    geom_tile(aes(fill = Salt)) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast/1000, y = Depth, z = Temp), breaks = seq(16, 25)) +
    scale_fill_distiller(palette = "Spectral", direction = -1 ,limits = c(min(mydata$Salt),  max(mydata$Salt)), oob = scales::squish,
                         name = expression(bold("Salinity"))) + #,limits = c(0.45,  0.58), oob = scales::squish
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -130, label = paste0(letters[j], site_labels[j]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    coord_cartesian(xlim = c(13, 48), expand = TRUE, ylim = c(-140,-10)) +
    #scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = c(-125,-100,-75,-50,-25)) +
    guides(size = "none", shape = "none")+
    theme(legend.title = element_text(angle = 270, hjust= 0.5, size = 12),
          axis.title = element_text(face = "bold")) + guides(fill = guide_colorbar(title.position = "right"))
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')& theme(legend.key.height = unit(3.9, "cm"))

ggsave(paste0('Other prepublication stuff/plots/zoop/Salinity_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('Other prepublication stuff/plots/zoop/Salinity_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


#### Plots of Biomass against distance to coast (Supplementary Figures) ####

mydata$site <- factor(mydata$site, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
mydata$Distance_Coast_km <- mydata$Distance_Coast/1000

labels <- c("CapeByron" = "a) Cape Byron (28.6°S)", "EvansHead" = "b) Evans Head (29°S)",
            "NorthSolitary" = "c) North Solitary (30°S)", "DiamondHead" = "d) Diamond Head (31.7°S)")


pD <- ggplot(mydata, aes(x = Distance_Coast_km, y = Biomass)) + geom_point(alpha = 0.5) + facet_wrap(~site, ncol = 1, labeller = labeller(site = labels)) +
  theme_classic() + geom_smooth(method = "lm") + scale_y_log10()+ xlab("Distance to Coast (km)") +
  ylab(expression(bold("Biomass "(mg~m^-3))))+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(face = "bold", size = 14, hjust = 0),
        panel.background = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(colour=NA, fill=NA))
pD

ggsave("../plots/zoop/Biomass by distance to coast.png", width=12, height=20, dpi = 600, units = "cm")



## now top 50m
mydata50 <- mydata %>% filter(Depth <=50)
labels2 <- c("CapeByron" = "e) Cape Byron (28.6°S; top 50m)", "EvansHead" = "f) Evans Head (29°S; top 50m)",
            "NorthSolitary" = "g) North Solitary (30°S; top 50m)", "DiamondHead" = "h) Diamond Head (31.7°S; top 50m)")


pD2a <- ggplot(mydata50, aes(x = Distance_Coast_km, y = Biomass)) + geom_point(alpha = 0.5) + facet_wrap(~site, ncol = 1, labeller = labeller(site = labels2)) +
  theme_classic() + geom_smooth(method = "lm") + scale_y_log10()+ xlab("Distance to Coast (km)") +
  ylab(NULL)+ #expression(bold("Biomass "(mg~m^-3)))
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(face = "bold", size = 14, hjust = 0),
        panel.background = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(colour=NA, fill=NA))
pD2a

library(patchwork)
pD + pD2a

ggsave("Other prepublication stuff/plots/zoop/Biomass by distance to coast revised.png",
       width=21, height=27, dpi = 600, units = "cm")



# Biomass against Depth
pD2 <- ggplot(mydata, aes(x = Depth, y = Biomass)) + geom_point(alpha = 0.5) + facet_wrap(~site, ncol = 1, labeller = labeller(site = labels)) +
  theme_classic() + geom_smooth(method = "lm") + scale_y_log10()+ xlab("Depth (m)") +
  ylab(expression(bold("Biomass "(mg~m^-3)))) + scale_x_continuous(breaks = seq(25,125,25)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(face = "bold", size = 14, hjust = 0),
        panel.background = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(colour=NA, fill=NA))
pD2

ggsave("../plots/zoop/Biomass by Depth.png", width=12, height=20, dpi = 600, units = "cm")

# only after a certain bathymetry
# only after 25km offshore

mydata3 <- mydata %>% filter(Distance_Coast_km >=25)
labels3 <- c("CapeByron" = "e) Cape Byron (28.6°S;\n>25km Offshore)", "EvansHead" = "f) Evans Head (29°S;\n>25km Offshore)",
             "NorthSolitary" = "g) North Solitary (30°S;\n>25km Offshore)", "DiamondHead" = "h) Diamond Head (31.7°S;\n>25km Offshore)")


pD2f <- ggplot(mydata3, aes(x = Depth, y = Biomass)) + geom_point(alpha = 0.5) + 
  facet_wrap(~site, ncol = 1, labeller = labeller(site = labels3)) +
  theme_classic() + geom_smooth(method = "lm") + scale_y_log10()+ xlab("Depth (m)") +
  ylab(NULL) + #expression(bold("Biomass "(mg~m^-3)))
  scale_x_continuous(breaks = seq(25,125,25)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        strip.text = element_text(face = "bold", size = 14, hjust = 0),
        panel.background = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(colour=NA, fill=NA))
pD2f

library(patchwork)
pD + pD2f
ggsave("Other prepublication stuff/plots/zoop/Biomass by depth to coast revised.png",
       width=21, height=27, dpi = 600, units = "cm")

# Plots of Pareto against distance to coast

mydata$site <- factor(mydata$site, levels = c("Cape Byron", "Evans Head", "North Solitary", "Diamond Head"))
mydata$Distance_Coast_km <- mydata$Distance_Coast/1000

pP <- ggplot(mydata, aes(x = Distance_Coast_km, y = ParetoSlope)) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pP

ggsave("../plots/zoop/Pareto by distance to coast.png", width=10, height=10, dpi = 600)


# Plots of Size against distance to coast

mydata$site <- factor(mydata$site, levels = c("Cape Byron", "Evans Head", "North Solitary", "Diamond Head"))
mydata$Distance_Coast_km <- mydata$Distance_Coast/1000



pS <- ggplot(mydata, aes(x = Distance_Coast_km, y = GeoMn*1000000)) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16)) +
  ylab("Geometric Mean Size (um)")
pS

ggsave("../plots/zoop/Size by distance to coast.png", width=10, height=10, dpi = 600)




