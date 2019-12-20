## MODIS Time Series leading up to the transects

## Hayden Schilling using code from Jason Everett for IMOS Download
## Use the end and start of each transect and look at a month prior

library(tidyverse)

# First get Transects

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

### Identify start and end points

datF <- mydata %>% group_by(site) %>% summarise(West_Long = min(Lon), East_Long = max(Lon),
                                               Lat = min(Lat), Date = as.POSIXct(min(datestr), 
                                                                                 format= "%d-%b-%Y %H:%M:%OS"))
datF
str(datF)

### Make a dataframe for each site
sites = levels(datF$site)
sites

df_list <- list()

for (i in sites){
  dat2 <- filter(datF, site == i)
  assign(paste0("dat_",i,"_West"), data.frame(Longitude = dat2$West_Long[1], Latitude = dat2$Lat[1],
                                Date = seq.Date(from = as.Date(dat2$Date[1]) , by = -1, length.out = 30),
                                Location = paste0(i,"_West")))
  assign(paste0("dat_",i,"_East"), data.frame(Longitude = dat2$East_Long[1], Latitude = dat2$Lat[1],
                                      Date = seq.Date(from = as.Date(dat2$Date[1]) , by = -1, length.out = 30),
                                      Location = paste0(i,"_East")))
}

# merge into a single dataframe
df_list <- list(dat_CapeByron_East, dat_CapeByron_West, dat_DiamondHead_East, dat_DiamondHead_West,
                dat_EvansHead_East, dat_EvansHead_West, dat_NorthSolitary_East, dat_NorthSolitary_West)
full_dat <- bind_rows(df_list)
full_dat$Location <- as.factor(full_dat$Location)
str(full_dat)


## Load download code
source("../../../IMOS Data/IMOS_Toolbox/fIMOS_MatchMODIS.R")


# Possible products
# pr <- c("sst_quality", "sst", "picop_brewin2012in", "picop_brewin2010at", "par", 
#         "owtd", "npp_vgpm_eppley_oc3", "npp_vgpm_eppley_gsm", "nanop_brewin2012in",
#         "nanop_brewin2010at", "l2_flags", "ipar", "dt", "chl_oc3", "chl_gsm", "K_490")

pr <- c("chl_oc3", "sst") # select products here eg: "chl_oc3", "sst"
# Set resolutions
res_temp <- "1d" # temporal resolutions
res_spat <- 10 # Return the average of res_spat x res_spat pixels

dat <- fIMOS_MatchMODIS(full_dat, pr, res_temp, res_spat)
str(dat)

dat2 <- str_split_fixed(dat$Location, pattern = "_", n = 2)
dat$Transect <- dat2[,1]
dat$East_West <- dat2[,2]

## Reorder levels of the factors
dat$East_West <- factor(dat$East_West, levels = c("West", "East"))
dat$Transect <- factor(dat$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

# load Transect dates file
date_dat <- read_csv("Transect Sample Dates.csv")
date_dat$Date <- as.Date(date_dat$Date, format = "%d/%m/%Y")
date_dat$East_West <- factor(date_dat$East_West, levels = c("West", "East"))
date_dat$Transect <- factor(date_dat$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
str(date_dat)

dat$chl_oc3_1d[dat$chl_oc3_1d > 10] <- 10

p1 <- ggplot(dat, aes(x = Date, y = log10(chl_oc3_1d + 0.5*min(dat$chl_oc3_1d, na.rm = T)))) + geom_line() + geom_point() +
  facet_grid(Transect~East_West) + theme_classic() +
  geom_vline(data = date_dat, aes(xintercept =  Date), col = "red", lty = 2) +
  ylab ("log10(MODIS Chl_a)")
p1

ggsave("plots/MODIS Chl_a month prior.png", height = 14.8, width = 21, units = "cm", dpi = 600)

## Try larger spatial resolution ##

# Set resolutions
res_temp <- "1d" # temporal resolutions
res_spat <- 20 # Return the average of res_spat x res_spat pixels

datX <- fIMOS_MatchMODIS(full_dat, pr, res_temp, res_spat)
str(datX)

datY <- str_split_fixed(datX$Location, pattern = "_", n = 2)
datX$Transect <- dat2[,1]
datX$East_West <- dat2[,2]

## Reorder levels of the factors
datX$East_West <- factor(datX$East_West, levels = c("West", "East"))
datX$Transect <- factor(datX$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


p1 <- ggplot(datX, aes(x = Date, y = chl_oc3_1d)) + geom_line() +
  facet_grid(Transect~East_West) + theme_classic() + geom_point() +
  geom_vline(data = date_dat, aes(xintercept =  Date), col = "red", lty = 2)
p1

p2 <- ggplot(datX, aes(x = Date, y = sst_1d)) + geom_line() +
  facet_grid(Transect~East_West) + theme_classic() +
  geom_vline(data = date_dat, aes(xintercept =  Date), col = "red", lty = 2)
p2
