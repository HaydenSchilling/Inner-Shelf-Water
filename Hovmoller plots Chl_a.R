# Hovmoller Plots Chl_a for sites

#Load Packages
library(tidyverse)
library(lubridate)
library(ncdf4)
library(tidync)

# #inshore locations (15 km from shore):
#  # CB - -28.632 , 153.794
#  # EH - -29 , 153.637
#  # NS - -30 , 153.384
#  # DH - -31.75, 153.944

# sitesX$Latitude_Offshore <- c(-28.63176, -28.99976, -29.99975, -31.64973)
# sitesX$Longitude_Offshore <- c(154.0497, 153.8936, 153.6431, 154.2076)

# Open File to investigate data (not analyse)
mydata <- nc_open("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc")
str(mydata) 
# time units are chr "days since 1800-01-01 00:00:00.0"
nc_close(mydata)


# Now actually analyse data using tidync
mydata2 <- tidync("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc")
mydata2

# Evans Head

# restrict to a thin latitude band to a certain distance offshore
EH <- tidync("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc") %>% 
  hyper_filter(latitude = latitude > -29.1 & latitude < -28.9) %>% 
  hyper_filter(longitude = longitude < 154.15)
EH

# Make dataframe
EH_dat <- EH %>% hyper_tibble()

# set date/day of year
EH_dat$date <- as.Date(EH_dat$time, origin = "1800-01-01")
EH_dat$DAY_OF_YEAR <- yday(EH_dat$date)
summary(EH_dat$DAY_OF_YEAR)

EH_data <- EH_dat %>% group_by(longitude, DAY_OF_YEAR) %>% summarise(CHL_A = mean(chl_oc3, na.rm = TRUE))
EH_data$Site <- "Evans Head"

head(EH_data)

# repeat forother sites

# Cape Byron
CB <- tidync("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc") %>% 
  hyper_filter(latitude = latitude > -28.7 & latitude < -28.5) %>% 
  hyper_filter(longitude = longitude < 154.15)
CB

CB_dat <- CB %>% hyper_tibble()
CB_dat$date <- as.Date(CB_dat$time, origin = "1800-01-01")
CB_dat$DAY_OF_YEAR <- yday(CB_dat$date)
summary(CB_dat$DAY_OF_YEAR)

CB_data <- CB_dat %>% group_by(longitude, DAY_OF_YEAR) %>% summarise(CHL_A = mean(chl_oc3, na.rm = TRUE))
CB_data$Site <- "Cape Byron"

head(CB_data)

# North Solitary
NS <- tidync("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc") %>% 
  hyper_filter(latitude = latitude > -30.1 & latitude < -29.9) %>% 
  hyper_filter(longitude = longitude < 154.15 & longitude >153)
NS

NS_dat <- NS %>% hyper_tibble()
NS_dat$date <- as.Date(NS_dat$time, origin = "1800-01-01")
NS_dat$DAY_OF_YEAR <- yday(NS_dat$date)
summary(NS_dat$DAY_OF_YEAR)

NS_data <- NS_dat %>% group_by(longitude, DAY_OF_YEAR) %>% summarise(CHL_A = mean(chl_oc3, na.rm = TRUE))
NS_data$Site <- "North Solitary"

head(NS_data)

# Diamond Head
DH <- tidync("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc") %>% 
  hyper_filter(latitude = latitude > -31.85 & latitude < -31.65) %>% 
  hyper_filter(longitude = longitude < 154.15)
DH

DH_dat <- DH %>% hyper_tibble()
DH_dat$date <- as.Date(DH_dat$time, origin = "1800-01-01")
DH_dat$DAY_OF_YEAR <- yday(DH_dat$date)
summary(DH_dat$DAY_OF_YEAR)

DH_data <- DH_dat %>% group_by(longitude, DAY_OF_YEAR) %>% summarise(CHL_A = mean(chl_oc3, na.rm = TRUE))
DH_data$Site <- "Diamond Head"

head(DH_data)


# Sydney
PH <- tidync("Data/Chla_imos_download/IMOS_aggregation_20200324T025801Z.nc") %>% 
  hyper_filter(latitude = latitude < -33.32 & latitude > -33.52) %>% 
  hyper_filter(longitude = longitude < 154.15)
PH

PH_dat <- PH %>% hyper_tibble()
PH_dat$date <- as.Date(PH_dat$time, origin = "1800-01-01")
PH_dat$DAY_OF_YEAR <- yday(PH_dat$date)
summary(PH_dat$DAY_OF_YEAR)

PH_data <- PH_dat %>% group_by(longitude, DAY_OF_YEAR) %>% summarise(CHL_A = mean(chl_oc3, na.rm = TRUE))
PH_data$Site <- "Port Hacking"

head(PH_data)

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
p1 <- ggplot(full_dat, aes(x = longitude, y = DAY_OF_YEAR, fill = CHL_A)) + geom_tile() + #coord_flip() +
  scale_fill_gradientn(colors = jet.colors(7), trans = "log10",limits = c(0.1,2), oob = scales::squish) +
  facet_wrap(~Site,  ncol = 1) + # , limits = c(18,27)
  theme_bw() + scale_x_continuous(expand = c(0, 0)) + geom_vline(data = sitesX, aes(xintercept = Longitude_Inshore)) +
  geom_vline(data = sitesX, aes(xintercept = Longitude_Offshore)) + geom_vline(data = sitesX, aes(xintercept = Longitude_Inshore_8km), lty =2) + 
  ylab("Day of the Year") + xlab("Longitude")
p1

ggsave("plots/Hovmoller plots inshore offshore chla.png", width = 21.5, height = 29, units = "cm", dpi = 600)
ggsave("plots/Hovmoller plots inshore offshore chla.pdf", width = 21.5, height = 29, units = "cm", dpi = 600)


## Add in Port Hacking for context
# Combine sites
full_dat <- bind_rows(CB_data, EH_data, NS_data, DH_data, PH_data)
full_dat$Site <- factor(full_dat$Site, levels = c("Cape Byron", "Evans Head", "North Solitary", "Diamond Head", "Port Hacking"))

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
p1 <- ggplot(full_dat, aes(x = longitude, y = DAY_OF_YEAR, fill = CHL_A)) + geom_tile() + #coord_flip() +
  scale_fill_gradientn(colors = jet.colors(7), trans = "log10",limits = c(0.1,2), oob = scales::squish) +
  facet_wrap(~Site,  ncol = 1) + # , limits = c(18,27)
  theme_bw() + scale_x_continuous(expand = c(0, 0)) + geom_vline(data = sitesX, aes(xintercept = Longitude_Inshore)) +
  geom_vline(data = sitesX, aes(xintercept = Longitude_Offshore)) + geom_vline(data = sitesX, aes(xintercept = Longitude_Inshore_8km), lty =2) + 
  ylab("Day of the Year") + xlab("Longitude")
p1


ggsave("plots/Hovmoller plots inshore offshore chla with Port Hacking.png", width = 21.5, height = 29, units = "cm", dpi = 600)
ggsave("plots/Hovmoller plots inshore offshore chla with Port Hacking.pdf", width = 21.5, height = 29, units = "cm", dpi = 600)

