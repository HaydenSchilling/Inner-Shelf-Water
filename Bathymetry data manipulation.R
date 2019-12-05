# Bathymetry along transects

B_dat <- read.csv("Data/Upwelling_Bathymetry_Data.csv", header = T)

names(B_dat)[names(B_dat)=="NaN."] <- "Latitude"

B_dat$Latitude <- as.factor(as.character(B_dat$Latitude))

library(tidyr)

Bdat2  <- B_dat %>% gather(Longitude, Bathymetry, 2:302)

Bdat2 <- gather(B_dat,key= c("Longitude"), value = "Bathymetry", 2:302)

head(Bdat2)

library(stringr)
Bdat2$Longitude <- as.numeric(str_sub(Bdat2$Longitude, start =2))


library(ggplot2)

p1 <- ggplot(Bdat2, aes(x= Longitude, y = Bathymetry)) + geom_line() +
  facet_wrap(~Latitude)
p1


Bdat2$site <- as.character("")
Bdat2$site[Bdat2$Latitude  == "-31.8"] <- "DiamondHead"
Bdat2$site[Bdat2$Latitude  == "-30"] <- "NorthSolitary"
Bdat2$site[Bdat2$Latitude  == "-29"] <- "EvansHead"
Bdat2$site[Bdat2$Latitude  == "-28.6"] <- "CapeByron"

write.csv(Bdat2, "Data/Transect Bathymetry.csv", row.names = FALSE)
