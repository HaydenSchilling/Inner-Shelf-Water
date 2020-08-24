# Hovmoller Plots  for sites

#Load Packages
library(tidyverse)
library(lubridate)
library(ncdf4)


# #inshore locations (15 km from shore):
#  # CB - -28.632 , 153.794
#  # EH - -29 , 153.637
#  # NS - -30 , 153.384
#  # DH - -31.75, 153.944

# sitesX$Latitude_Offshore <- c(-28.63176, -28.99976, -29.99975, -31.64973)
# sitesX$Longitude_Offshore <- c(154.0497, 153.8936, 153.6431, 154.2076)

# Open File
mydata <- nc_open("../../../UNLOC/Charlie/Climatology/IMOS_aggregation_20200117T053823Z.nc")

nc_close(mydata)

library(tidync)
mydata2 <- tidync("../../../UNLOC/Charlie/Climatology/IMOS_aggregation_20200117T053823Z.nc")
mydata2

# Evans Head
EH <- tidync("../../../UNLOC/Charlie/Climatology/IMOS_aggregation_20200117T053823Z.nc") %>% 
  hyper_filter(LATITUDE = LATITUDE > -29.1 & LATITUDE < -28.9) %>% 
  hyper_filter(LONGITUDE = LONGITUDE < 154.15)
EH

EH_dat <- EH %>% hyper_tibble()

EH_data <- EH_dat %>% group_by(LONGITUDE, DAY_OF_YEAR) %>% summarise(TEMP = mean(TEMP))
EH_data$Site <- "Evans Head"

head(EH_data)

# Cape Byron
CB <- tidync("../../../UNLOC/Charlie/Climatology/IMOS_aggregation_20200117T053823Z.nc") %>% 
  hyper_filter(LATITUDE = LATITUDE > -28.7 & LATITUDE < -28.5) %>% 
  hyper_filter(LONGITUDE = LONGITUDE < 154.15)
CB

CB_dat <- CB %>% hyper_tibble()

CB_data <- CB_dat %>% group_by(LONGITUDE, DAY_OF_YEAR) %>% summarise(TEMP = mean(TEMP))
CB_data$Site <- "Cape Byron"

head(CB_data)

# North Solitary
NS <- tidync("../../../UNLOC/Charlie/Climatology/IMOS_aggregation_20200117T053823Z.nc") %>% 
  hyper_filter(LATITUDE = LATITUDE > -30.1 & LATITUDE < -29.9) %>% 
  hyper_filter(LONGITUDE = LONGITUDE < 154.15)
NS

NS_dat <- NS %>% hyper_tibble()

NS_data <- NS_dat %>% group_by(LONGITUDE, DAY_OF_YEAR) %>% summarise(TEMP = mean(TEMP))
NS_data$Site <- "North Solitary"

head(NS_data)

# Diamond Head
DH <- tidync("../../../UNLOC/Charlie/Climatology/IMOS_aggregation_20200117T053823Z.nc") %>% 
  hyper_filter(LATITUDE = LATITUDE > -31.85 & LATITUDE < -31.65) %>% 
  hyper_filter(LONGITUDE = LONGITUDE < 154.15)
DH

DH_dat <- DH %>% hyper_tibble()

DH_data <- DH_dat %>% group_by(LONGITUDE, DAY_OF_YEAR) %>% summarise(TEMP = mean(TEMP))
DH_data$Site <- "Diamond Head"

head(DH_data)

# Combine sites
full_dat <- bind_rows(CB_data, EH_data, NS_data, DH_data)
full_dat$Site <- factor(full_dat$Site, levels = c("Cape Byron", "Evans Head", "North Solitary", "Diamond Head"))

# Create dataframe of inshore and offshore sites 
sitesX <- data.frame(Site = c("Cape Byron", "Evans Head", "North Solitary", "Diamond Head"),
                     Longitude_Inshore = c(153.794, 153.637, 153.384, 152.944),
                     Longitude_Offshore <- c(154.1008, 153.9449, 153.6949, 153.2606),
                     Longitude_Inshore_8km = c(153.7224, 153.5652, 153.3115, 152.8702))

# #inshore locations (15 km from shore):
#  # CB - -28.632 , 153.794
#  # EH - -29 , 153.637
#  # NS - -30 , 153.384
#  # DH - -31.75, 152.944

# Jet colour pallete
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


library(ggplot2)
p1 <- ggplot(full_dat, aes(x = LONGITUDE, y = DAY_OF_YEAR, fill = TEMP)) + geom_tile() + #coord_flip() +
  scale_fill_gradientn(colors = jet.colors(7), limits = c(18,27)) + facet_wrap(~Site,  ncol = 1) +
  theme_bw() + scale_x_continuous(expand = c(0, 0)) + geom_vline(data = sitesX, aes(xintercept = Longitude_Inshore)) +
  geom_vline(data = sitesX, aes(xintercept = Longitude_Offshore)) + geom_vline(data = sitesX, aes(xintercept = Longitude_Inshore_8km), lty =2) + 
  ylab("Day of the Year") + xlab("Longitude")
p1

ggsave("plots/Hovmoller plots inshore offshore.png", width = 21.5, height = 29, units = "cm", dpi = 600)
ggsave("plots/Hovmoller plots inshore offshore.pdf", width = 21.5, height = 29, units = "cm", dpi = 600)

