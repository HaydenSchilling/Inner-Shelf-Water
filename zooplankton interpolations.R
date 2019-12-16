# Zooplankton data interpolation

# Interpolating plots

#install.packages("akima")

library(akima)
library(ggplot2)
library(reshape2)
library(tidyverse)

mydata <- read_csv("Data/SS2004_SeaSoarData.csv")
str(mydata)
head(mydata)

mydata <- mydata %>% 
  mutate(site = case_when(site = str_detect(File,"SS0408_023") ~ "CapeByron",
                          site = str_detect(File,"SS0408_021") ~ "EvansHead",
                          site = str_detect(File,"SS0408_010") ~ "NorthSolitary",
                          site = str_detect(File,"SS0408_008") ~ "DiamondHead"),
         site = as.factor(site))
### Get distance from shore


### Get distance from shore
library(geosphere)

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

p1 <- ggplot(mydata, aes(x = Lon, y = -Depth)) + geom_point(aes(col = Abundance)) +
  facet_wrap(~site, scales = "free_x") + theme_bw() +
  geom_line(data= Bathy, aes(x = Longitude, y = Bathymetry)) +
  scale_color_gradient2(low = "blue", high = "red", mid = "white")
p1


# # variables to loop through
# vars = c("Temp", "Salt","GeoMn")# "NBSS.Slope",
# #sites to loop through
sites <- levels(mydata$site)
# 
# # # Testing
# # for (i in vars){
# # 
# #   p1 <- ggplot(mydata, aes(x = long3, y = -Depth, col = get(i))) + geom_point() +
# #     facet_wrap(~OPC_site, scales = "free_x") + theme_bw()
# #   print(p1)
# # }
# # mydata2 <- subset(mydata, site == "EvansHead")
# # # str(mydata2)
# # 
# # plot(mydata2$depth, mydata2$long, col = mydata2$temp)
# 
# for (j in sites){
#   mydata2 <- subset(mydata, site == j)
#   for (i in vars){
#     mydata2 <- mydata2 %>% drop_na(i) %>% filter(i != Inf)
#       #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
#     fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = mydata2[[i]], nx = 100, ny = 100)
#    
#     df <- melt(fit1$z, na.rm = TRUE)
#     names(df) <- c("x", "y", i)
#     df$Distance_Coast <- fit1$x[df$x]
#     df$Depth <- fit1$y[df$y]
#     
#     ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = i)) + 
#       geom_tile(aes(fill = df[[i]])) +
#       geom_contour(colour = "white") + # , binwidth = 0.125
#       scale_fill_distiller(palette = "YlOrRd", direction = 1) + #limits = c(min(mydata[[i]]), max(mydata[[i]]))
#       geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth)) + 
#       geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth)) +
#       ggtitle(paste0(i, " at ", j))
#     
#     ggsave(paste0('plots/zoop/',j,"_",i,'.pdf'),width = 10, height = 5)
#     ggsave(paste0('plots/zoop/',j,"_",i,'.png'),width = 10, height = 5, dpi = 600)
#     
#     
#     #  pdf(paste0('plots/zoop/',j,"_",i,'.pdf'), width=10, height=5)
#     # print(filled.contour(fit1, zlim = c(min(mydata[[i]]), max(mydata[[i]])),
#     #                      plot.title = title(main = c(j, i))))
#     # dev.off()
#     # png(paste0('plots/zoop/',j,"_",i,'.png'), width=6000, height=3000, res = 600)
#     # print(filled.contour(fit1, zlim = c(min(mydata[[i]]), max(mydata[[i]])),
#     #                      plot.title = title(main = c(j, i))))
#     # dev.off()
#     # pdf(paste0('plots/zoop/',j,"_",i,'_lines.pdf'), width=10, height=5)
#     # print(contour(fit1, plot.title = title(main = c(j, i))))
#     # dev.off()
#     # png(paste0('plots/zoop/',j,"_",i,'_lines.png'), width=6000, height=3000, res = 600)
#     # print(contour(fit1, plot.title = title(main = c(j, i))))
#     # dev.off()
#   }
# }
# str(fit1)


### Biomass interpoLation and plots
for (j in sites){
  mydata2 <- mydata %>% 
    filter(site == j & is.na(Depth)==FALSE & is.na(Biomass)==FALSE)
  Bathy2 <- filter(Bathy, site == j)
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = log10(Biomass), nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Biomass")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = log10(Biomass))) + 
    geom_tile(aes(fill = log10(Biomass))) +
    geom_contour(colour = "white", binwidth = 0.125) + 
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(-0.3,  0.5)) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth, alpha = 0.5)) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    ggtitle(paste0("Biomass at ", j)) + theme_classic()
  
  ggsave(paste0('plots/zoop/',j,"_Biomass",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/zoop/',j,"_Biomass",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}


### Now abundance

### Abundance interpoLation and plots
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>% drop_na(Abundance, Depth) %>% filter(Abundance != Inf)
  Bathy2 <- filter(Bathy, site == j)
  
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Abundance, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Abundance")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = Abundance)) + 
    geom_tile(aes(fill = Abundance)) +
    geom_contour(colour = "white") + #, binwidth = 0.125
    scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) +
    geom_line(data= Bathy2, aes(x = Distance_Coast, y = Bathymetry), inherit.aes = FALSE) +
      ggtitle(paste0("Abundance at ", j))
  
  ggsave(paste0('plots/zoop/',j,"_Abundance",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/zoop/',j,"_Abundance",'.png'),width = 10, height = 5, dpi = 600)
  
  # pdf(paste0('plots/zoop/',j,"_Biomass",'.pdf'), width=10, height=5)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
  # png(paste0('plots/zoop/',j,"_Biomass",'.png'), width=6000, height=3000, res = 600)
  # print(filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass))),
  #       plot.title = title(main = c(j)))
  # dev.off()
}




# ### Mormalised Geo_Mn Size #  done
# #mydata <- na.omit(mydata) # check this
# for (j in sites){
#   mydata2 <- subset(mydata, site == j)
#   
#   fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$depth, z = mydata2$Norm.GeoMnESD)
#   pdf(paste0('plots/zoop/',j,"_Normalised_GeoMnSize",'.pdf'), width=10, height=5)
#   print(filled.contour(fit1, zlim = c(min(mydata$Norm.GeoMnESD), max(mydata$Norm.GeoMnESD))),
#         plot.title = title(main = c(j)))
#   dev.off()
#   png(paste0('plots/zoop/',j,"_Normalised_GeoMnSize",'.png'), width=6000, height=3000, res = 600)
#   print(filled.contour(fit1, zlim = c(min(mydata$Norm.GeoMnESD), max(mydata$Norm.GeoMnESD)),
#                        plot.title = title(main = c(j))))
#   dev.off()
# }
# 

### Geo_Mn Size #  done
### interpoLation and plots
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>% drop_na(GeoMn, Depth) %>% filter(GeoMn != Inf)
  Bathy2 <- filter(Bathy, site == j)
  
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = GeoMn*1000, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "GeoMn")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = GeoMn)) + 
    geom_tile(aes(fill = GeoMn)) +
    geom_contour(colour = "white") + #, binwidth = 0.125
    scale_fill_distiller(palette = "Spectral", direction = 1, # "YlOrRd" 
                         limits = c(min(mydata$GeoMn*1000, na.rm = TRUE), max(mydata$GeoMn*1000, na.rm = TRUE))) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    ggtitle(paste0("Geometric Mean Size at ", j))
  
  ggsave(paste0('plots/zoop/',j,"_GeoMn",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/zoop/',j,"_GeoMn",'.png'),width = 10, height = 5, dpi = 600)
  
}
### Slope #  done
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>% drop_na(NBSSSlope) %>% filter(NBSSSlope != Inf)
  
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = NBSSSlope, nx = 100, ny = 100))
  Bathy2 <- filter(Bathy, site == j)
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "NBSSSlope")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = NBSSSlope)) + 
    geom_tile(aes(fill = NBSSSlope)) +
    geom_contour(colour = "white") + #, binwidth = 0.125
    scale_fill_distiller(palette = "Spectral", direction = 1, # "YlOrRd"
                         limits = c(min(mydata$NBSSSlope, na.rm = TRUE), max(mydata$NBSSSlope, na.rm = TRUE))) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) +
    geom_line(data= Bathy2, aes(x = Distance_Coast, y = Bathymetry), inherit.aes = FALSE) +
    ggtitle(paste0("NBSS Slope at ", j))
  
  ggsave(paste0('plots/zoop/',j,"_NBSS_Slope",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/zoop/',j,"_NBSS_Slope",'.png'),width = 10, height = 5, dpi = 600)
  
}


### Pareto Slope #  done
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>% drop_na(ParetoSlope) %>% filter(ParetoSlope != Inf)
  Bathy2 <- filter(Bathy, site == j)
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = ParetoSlope, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "ParetoSlope")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = ParetoSlope)) + 
    geom_tile(aes(fill = ParetoSlope)) +
    geom_contour(colour = "white") + #, binwidth = 0.125
    scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(min(mydata$ParetoSlope), max(mydata$ParetoSlope))) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth),alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
    ggtitle(paste0("Pareto Slope at ", j)) + theme_classic()
  
  ggsave(paste0('plots/zoop/',j,"_Pareto_Slope",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/zoop/',j,"_Pareto_Slope",'.png'),width = 10, height = 5, dpi = 600)
  
}


### Temperature #  done
for (j in sites){
  mydata2 <- filter(mydata, site == j)
  mydata2 <-  mydata2 %>% drop_na(Temp) %>% filter(Temp != Inf)
  Bathy2 <- filter(Bathy, site == j)
  #fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass), 
  #               nx = 100, ny = 100)
  fit1 <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = Temp, nx = 100, ny = 100))
  
  df <- melt(fit1$z, na.rm = TRUE)
  names(df) <- c("x", "y", "Temp")
  df$Distance_Coast <- fit1$x[df$x]
  df$Depth <- fit1$y[df$y]
  
  ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = Temp)) + 
    geom_tile(aes(fill = Temp)) +
    geom_contour(colour = "white", binwidth = 1) + #, binwidth = 0.125
    scale_fill_distiller(palette = "Spectral", direction = -1, limits = c(min(mydata$Temp), max(mydata$Temp))) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth),alpha = 0.5) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth), alpha = 0.5) +
    geom_ribbon(data= Bathy2, aes(x = Distance_Coast, ymax = Bathymetry, ymin=-250), inherit.aes = FALSE, fill = "grey60") +
    ggtitle(paste0("Temperature at ", j))
  
  ggsave(paste0('plots/zoop/',j,"_Temperature",'.pdf'),width = 10, height = 5)
  ggsave(paste0('plots/zoop/',j,"_Temperature",'.png'),width = 10, height = 5, dpi = 600)
  
}



### TS Plots
#install.packages("sommer")
library(sommer) # for jet colour scheme

#mydata3 <- subset(mydata, Depth > 10)

pTS <- ggplot(mydata, aes(x = Salt, y = Temp, col = log10(Biomass))) + geom_point() + facet_wrap(~site) + 
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTS

ggsave("plots/zoop/TS plot by log10 biomass.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by log10 biomass.png", width=10, height=8, dpi = 600)


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
  theme_classic() + scale_color_gradientn(colours = jet.colors(100))
pTS

ggsave("plots/zoop/TS plot by log10 biomass top 50.pdf", width=10, height=8)
ggsave("plots/zoop/TS plot by log10 biomass top 50.png", width=10, height=8, dpi = 600)


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


### SOME SIMPLE ZOOPLANKTON MODELS

cor.test(mydata$Temp, mydata$Salt)
cor.test(mydata$Temp, mydata$Depth)
cor.test(mydata$Temp, mydata$Distance_Coast)
cor.test(mydata$Lon, mydata$Distance_Coast)



mydataX <- mydata %>% drop_na(Biomass)


fit1 <- lm(log10(Biomass) ~ Temp*Depth + Distance_Coast + site*Lon, data = mydataX)
plot(fit1)
summary(fit1)

library(effects)
plot(allEffects(fit1))

fit2 <- lm(ParetoSlope ~ Temp*Depth + Distance_Coast + site*Lon, data = mydata)
plot(fit2)
summary(fit2)

library(effects)
plot(allEffects(fit2))

fit3 <- lm(GeoMn*1000 ~ Temp*Depth + Distance_Coast + site*Lon, data = mydata)
plot(fit3)
summary(fit3)

library(effects)
plot(allEffects(fit3))


### Spatial Autocorrelation Morans I (code from Charlie)
library(lctools)

res <- residuals(fit2)
#res


Coords <-cbind(mydataX$Lat, mydataX$Lon)

bw <- 10

mI <-moransI(Coords,bw,res)

moran.table <-matrix(data=NA,nrow=1,ncol=6)

col.names <-c("Moran's I", "Expected I", "Z resampling", "P-value resampling","Z randomization", "P-value randomization")

colnames(moran.table) <- col.names

moran.table[1,1] <- mI$Morans.I

moran.table[1,2] <- mI$Expected.I

moran.table[1,3] <- mI$z.resampling

moran.table[1,4] <- mI$p.value.resampling

moran.table[1,5] <- mI$z.randomization

moran.table[1,6] <- mI$p.value.randomization

print(moran.table)

