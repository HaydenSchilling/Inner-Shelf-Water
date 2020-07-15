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


### Biomass interpoLation and plots
pl <- list()

for (j in sites){
  mydata2 <- mydata %>%
    filter(site == j & is.na(Depth) == FALSE & is.na(Biomass)==FALSE) %>%
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

  Bathy2 <- filter(Bathy, site == j)

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
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(10,  1000), oob = scales::squish, trans = "log10") + # 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -185, label = paste0("Biomass at ", j[1]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(size = "none", shape = "none")
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')

ggsave(paste0('plots/zoop/Biomass_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('plots/zoop/Biomass_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


# Plots of Biomass against distance to coast

mydata$site <- factor(mydata$site, levels = c("Cape Byron", "Evans Head", "North Solitary", "Diamond Head"))
mydata$Distance_Coast_km <- mydata$Distance_Coast/1000

pD <- ggplot(mydata, aes(x = Distance_Coast_km, y = log10(Biomass))) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pD

ggsave("plots/zoop/Biomass by distance to coast.png", width=10, height=10, dpi = 600)


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

ggsave("plots/zoop/Pareto by distance to coast.png", width=10, height=10, dpi = 600)

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

ggsave("plots/zoop/Size by distance to coast.png", width=10, height=10, dpi = 600)



### Geo_Mn Size #  done
### interpoLation and plots
pl <- list()
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>%
    drop_na(GeoMn, Depth) %>%
    filter(GeoMn != Inf) %>%
    mutate(GeoMn = GeoMn * 1e3)

  Bathy2 <- filter(Bathy, site == j)

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
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(400, 500), oob = scales::squish) +
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -185, label = paste0("GeoMn at ", j[1]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(size = "none", shape = "none")
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')

ggsave(paste0('plots/zoop/GeoMn_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('plots/zoop/GeoMn_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")



### Pareto Slope #  done
pl <- list()
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>%
    drop_na(ParetoSlope) %>%
    filter(ParetoSlope != Inf)

  Bathy2 <- filter(Bathy, site == j)

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
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-1.4, -0.8), oob = scales::squish) +
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth),alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -185, label = paste0("NBSS Pareto Slope at ", j[1]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(size = "none", shape = "none")

}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')

ggsave(paste0('plots/zoop/ParetoSlope_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('plots/zoop/ParetoSlope_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")





### Abundance #  done
pl <- list()
for (j in sites){
  mydata2 <- mydata %>%
    filter(site == j & is.na(Depth) == FALSE & is.na(Abundance)==FALSE & is.finite(Abundance)==TRUE & Abundance > 0) %>%
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

  Bathy2 <- filter(Bathy, site == j)

  fit1 <- with(mydata2, akima::interp(x = Distance_Coast, y = -Depth, z = Abundance, nx = 100, ny = 100)) #z = log10(Abundance)
  fit2 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))

  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Abundance")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]

  df2 <- melt(fit2$z, na.rm = TRUE)
  names(df2) <- c("x", "y", "Temp")
  df$Temp <- df2$Temp

  pl[[j]] <- ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = log10(Abundance))) +
    geom_tile(aes(fill = log10(Abundance))) + ylab("Depth (m)") +
    geom_contour(aes(x = Distance_Coast, y = Depth, z = Temp), colour = "grey10", binwidth = 0.25, size = 0.2) +
    geom_contour(aes(x = Distance_Coast, y = Depth, z = Temp), colour = "grey10", binwidth = 1, size = 0.5) +
    geom_text_contour(aes(x = Distance_Coast, y = Depth, z = Temp), breaks = seq(16, 25)) +
    scale_fill_distiller(palette = "Spectral", direction = -1 ) + #,limits = c(0.45,  0.58), oob = scales::squish
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -185, label = paste0("Abundance at ", j[1]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    scale_x_continuous(limits = c(12000, 48000), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(size = "none", shape = "none")
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')

ggsave(paste0('plots/zoop/Abundance_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")





## Salinity Interpolations

### Salinity #  done
pl <- list()
for (j in sites){
  mydata2 <- mydata %>%
    filter(site == j & is.na(Depth) == FALSE & is.na(Salt) == FALSE & is.finite(Salt) == TRUE & Salt > 0) %>%
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
  
  Bathy2 <- filter(Bathy, site == j)
  
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
    scale_fill_distiller(palette = "Spectral", direction = -1 ,limits = c(min(mydata$Salt),  max(mydata$Salt)), oob = scales::squish) + #,limits = c(0.45,  0.58), oob = scales::squish
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth), alpha = 0.5, size = 0.2) +
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast/1000, y = -Depth, alpha = 0.5), size = 0.1, show.legend = FALSE) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast/1000, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    geom_text(x = 12.010, y = -185, label = paste0("Salinity at ", j[1]), stat = "identity", inherit.aes = FALSE, hjust = 0) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "mm"),
          axis.text  = element_text(colour="black")) +
    xlab(element_blank()) +
    scale_x_continuous(limits = c(12, 48), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(size = "none", shape = "none")
  # + geom_polygon(data = Limits, mapping = aes(x = Distance_Coast, y = -maxD), inherit.aes = FALSE, colour = "white")
}

pl[[4]] <- pl[[4]] + xlab("Distance from Coastline (km)")
pl[[1]] + pl[[2]] + pl[[3]] + pl[[4]] + plot_layout(ncol = 1, guides = 'collect')

ggsave(paste0('plots/zoop/Salinity_All','.png'), dpi = 600, height = 21, width = 18, units = "cm")
ggsave(paste0('plots/zoop/Salinity_All','.pdf'), dpi = 600, height = 21, width = 18, units = "cm")


### TS Plots

#install.packages("sommer")
library(sommer) # for jet colour scheme

#mydata3 <- subset(mydata, Depth > 10)

mydata$site <- factor(mydata$site, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

pTS <- ggplot(mydata, aes(x = Salt, y = Temp, col = log10(Biomass))) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pTS

ggsave("plots/zoop/TS plot by log10 biomass.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by log10 biomass.png", width=10, height=8, dpi = 600)

pSB <- ggplot(mydata, aes(x = ParetoSlope, y = log10(Biomass), col = Distance_Coast/1000)) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pSB

ggsave("plots/zoop/Biomass Pareto Scatterplot.pdf", width=10, height=8)
ggsave("plots/zoop/Biomass Pareto Scatterplot.png", width=10, height=8, dpi = 600)


pSB2 <- ggplot(mydata, aes(x = ParetoSlope, y = log10(Biomass), col = Distance_Coast/1000)) + geom_point() +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pSB2
ggsave("plots/zoop/Biomass Pareto Scatterplot combined.pdf", width=10, height=8)
ggsave("plots/zoop/Biomass Pareto Scatterplot combined.png", width=10, height=8, dpi = 600)


pXX <- ggplot(mydata, aes(x = Temp, y = ParetoSlope)) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pXX



pTSL <- ggplot(mydata, aes(x = Salt, y = Temp, col = Depth)) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTSL

ggsave("plots/zoop/TS plot by Depth.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by Depth.png", width=10, height=8, dpi = 600)

pTSL <- ggplot(mydata, aes(x = Salt, y = Temp, col = Depth)) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTSL

ggsave("plots/zoop/TS plot by Depth.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by Depth.png", width=10, height=8, dpi = 600)


### Now do only top 50m
mydata2 <- subset(mydata, Depth < 51)

pTS <- ggplot(mydata2, aes(x = Salt, y = Temp, col = log10(Biomass))) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pTS

ggsave("plots/zoop/TS plot by log10 biomass top 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by log10 biomass top 50.png", width=10, height=8, dpi = 600)

pTA <- ggplot(mydata2, aes(x = Salt, y = Temp, col = log10(Abundance))) + geom_point() +# facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTA

ggsave("plots/zoop/TS plot by log10 Abundance top 50 combined.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by log10 Abundance top 50 combined.png", width=10, height=8, dpi = 600)

pTA <- ggplot(mydata2, aes(x = Salt, y = Temp, col = ParetoSlope)) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTA

ggsave("plots/zoop/TS plot by Pareto Slope top 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by Pareto Slope top 50.png", width=10, height=8, dpi = 600)

pTA <- ggplot(mydata2, aes(x = Salt, y = Temp, col = GeoMn*1000000)) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTA

ggsave("plots/zoop/TS plot by GeoMn Size top 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by GeoMn Size top 50.png", width=10, height=8, dpi = 600)

pTSL <- ggplot(mydata2, aes(x = Salt, y = Temp, col = Depth)) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTSL

ggsave("plots/zoop/TS plot by Depth top 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by Depth top 50.png", width=10, height=8, dpi = 600)

### Now do only below 50m
mydata2 <- subset(mydata, Depth >= 51)

pTS <- ggplot(mydata2, aes(x = Salt, y = Temp, col = log10(Biomass))) + geom_point() + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTS

ggsave("plots/zoop/TS plot by log10 biomass below 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by log10 biomass below 50.png", width=10, height=8, dpi = 600)


pTSL <- ggplot(mydata2, aes(x = Salt, y = Temp, col = Depth)) + geom_point(alpha = 0.5) + facet_wrap(~site) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTSL

ggsave("plots/zoop/TS plot by Depth below 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by Depth below 50.png", width=10, height=8, dpi = 600)



