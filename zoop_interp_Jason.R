# Zooplankton data interpoLation

# InterpoLating plots

#install.packages("akima")

library(akima)
library(tidyverse)
library(geosphere)
library(reshape2) # I only load this to use 'melt' below. I coulnd't figure out how to do it with tidyverse :-( 

# load data
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


mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "CapeByron") {
    mydata$Distance_Coast[i] = distm(c(153.638598, -28.635808), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EvansHead") {
    mydata$Distance_Coast[i] = distm(c(153.481200, -28.994824), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NorthSolitary") {
    mydata$Distance_Coast[i] = distm(c(153.224309, -29.995614), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "DiamondHead") {
    mydata$Distance_Coast[i] = distm(c(152.782016, -31.749982), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  
}

# try a single plot
mydata2 <- mydata %>% 
  filter(site == "DiamondHead" & is.na(Depth)==FALSE & is.na(Biomass)==FALSE)

fit1 <- interp(x = mydata2$Distance_Coast, y = -mydata2$Depth, z = log10(mydata2$Biomass))
filled.contour(fit1, zlim = c(min(log10(mydata$Biomass)), log10(mydata$Biomass)))

fld <- with(mydata2, interp(x = Distance_Coast, y = -Depth, z = log10(Biomass), nx = 100, ny = 100))

df <- melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "Biomass")
df$Distance_Coast <- fld$x[df$x]
df$Depth <- fld$y[df$y]


# Now do it with ggplot
ggplot(data = df, mapping = aes(x = Distance_Coast, y = Depth, z = log10(Biomass))) + 
  geom_tile(aes(fill = log10(Biomass))) +
  geom_contour(colour = "white", binwidth = 0.125) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = c(-0.25,  0.75)) + 
  geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth)) + 
  geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth))




sites <- levels(mydata$site)

### Biomass interpoLation and plots
for (j in sites){
  mydata2 <- mydata %>% 
    filter(site == j & is.na(Depth)==FALSE & is.na(Biomass)==FALSE)
  
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
    scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = c(-0.25,  0.75)) + 
    geom_line(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth)) + 
    geom_point(data = mydata2, mapping = aes(x = Distance_Coast, y = -Depth)) +
    ggtitle(paste0("Biomass at ", j))
  
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

