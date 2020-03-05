# Looking at the temperature gradient from inner shelf to outer shelf

## Need to get location and date matrix then extract SST from satelites

# As inshore, let's use 15km from shore


library(tidyverse)

#inshore locations (15 km from shore):
# CB - -28.632 , 153.794
# EH - -29 , 153.637
# NS - -30 , 153.384
# DH - -31.75, 152.944
sitesX <- data.frame(Site = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
                     Latitude_Inshore = c(-28.632, -29, -30, -31.75),
                     Longitude_Inshore = c(153.794, 153.637, 153.384, 152.944))

# For 8km offshore as inshore locations
#sitesX <- data.frame(Site = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
#                     Latitude_Inshore = c(-28.63198, -29, -30, -31.75),
#                     Longitude_Inshore = c(153.7224, 153.5652, 153.3115, 152.8702))

# Offshore locations
library(geosphere)
destPoint(p = c(sitesX$Longitude_Inshore[1], sitesX$Latitude_Inshore[1]), b = 90, d = 30000)
destPoint(p = c(sitesX$Longitude_Inshore[2], sitesX$Latitude_Inshore[2]), b = 90, d = 30000)
destPoint(p = c(sitesX$Longitude_Inshore[3], sitesX$Latitude_Inshore[3]), b = 90, d = 30000)
destPoint(p = c(sitesX$Longitude_Inshore[4], sitesX$Latitude_Inshore[4]), b = 90, d = 30000)

# # get 8km offshore locations
# destPoint(p = c(sitesX$Longitude_Inshore[1], sitesX$Latitude_Inshore[1]), b = 270, d = 7000)
# destPoint(p = c(sitesX$Longitude_Inshore[2], sitesX$Latitude_Inshore[2]), b = 270, d = 7000)
# destPoint(p = c(sitesX$Longitude_Inshore[3], sitesX$Latitude_Inshore[3]), b = 270, d = 7000)
# destPoint(p = c(sitesX$Longitude_Inshore[4], sitesX$Latitude_Inshore[4]), b = 270, d = 7000)

# Offshore locations (45km offshore)
sitesX$Latitude_Offshore <- c(-28.63165, -28.99976, -29.99963, -31.74961)
sitesX$Longitude_Offshore <- c(154.1008, 153.9449, 153.6949, 153.2606)

sitesX

# Now make date string
# let's to 2004 - 2013 (10 years)

Date = seq.Date(from = as.Date("2004-01-01") , to = as.Date("2013-12-31"), by = 1)


### Make a dataframe for each site
sites = levels(sitesX$Site)
sites

df_list <- list()

for (i in sites){
  dat2 <- filter(sitesX, Site == i)
  assign(paste0("dat_",i,"_West"), data.frame(Longitude = dat2$Longitude_Inshore[1], Latitude = dat2$Latitude_Inshore[1],
                                              Date =seq.Date(from = as.Date("2004-01-01") , to = as.Date("2013-12-31"), by = 1),
                                              Location = paste0(i,"_West")))
  assign(paste0("dat_",i,"_East"), data.frame(Longitude = dat2$Longitude_Offshore[1], Latitude = dat2$Latitude_Offshore[1],
                                              Date = seq.Date(from = as.Date("2004-01-01") , to = as.Date("2013-12-31"), by = 1),
                                              Location = paste0(i,"_East")))
}

# merge into a single dataframe
df_list <- list(dat_CapeByron_East, dat_CapeByron_West, dat_DiamondHead_East, dat_DiamondHead_West,
                dat_EvansHead_East, dat_EvansHead_West, dat_NorthSolitary_East, dat_NorthSolitary_West)
full_dat <- bind_rows(df_list)
full_dat$Location <- as.factor(full_dat$Location)
str(full_dat)


# 
# 
# ############################
# # 
# Test a smaller dataset (works well)
#full_dat <- head(full_dat)

# # remove dodgy dates from MODIS
# full_dat <- filter(full_dat, Date != "2004-12-04")
# full_dat <- filter(full_dat, Date != "2006-07-05")
# full_dat <- filter(full_dat, Date != "2007-06-02")
# full_dat <- filter(full_dat, Date != "2007-12-02")
#
#
CBE_dat <- filter(full_dat, Location == "CapeByron_East")
CBW_dat <- filter(full_dat, Location == "CapeByron_West")
DHE_dat <- filter(full_dat, Location == "DiamondHead_East")
DHW_dat <- filter(full_dat, Location == "DiamondHead_West")
NSE_dat <- filter(full_dat, Location == "NorthSolitary_East")
NSW_dat <- filter(full_dat, Location == "NorthSolitary_West")
EHE_dat <- filter(full_dat, Location == "EvansHead_East")
EHW_dat <- filter(full_dat, Location == "EvansHead_West")


head(full_dat)

## Load download code
#source("../../../IMOS Data/IMOS_Toolbox/fIMOS_MatchMODIS.R")
source("fIMOS_MatchMODIS.R")


# Possible products
# pr <- c("sst_quality", "sst", "picop_brewin2012in", "picop_brewin2010at", "par",
#         "owtd", "npp_vgpm_eppley_oc3", "npp_vgpm_eppley_gsm", "nanop_brewin2012in",
#         "nanop_brewin2010at", "l2_flags", "ipar", "dt", "chl_oc3", "chl_gsm", "K_490")

pr <- c("sst") # select products here eg: "chl_oc3", "sst"
# # Set resolutions
# res_temp <- "1d" # temporal resolutions
# res_spat <- 5 # Return the average of res_spat x res_spat pixels
# 
# #dat <- fIMOS_MatchMODIS(full_dat, pr, res_temp, res_spat)
# #str(dat)
# 
# 
# 
# #write.csv(dat, "Inshore_Offshore_Gradient.csv", row.names = F)


# 
CBE_dat <- fIMOS_MatchMODIS(CBE_dat, pr, res_temp, res_spat) #
write.csv(CBE_dat, "CB_east.csv", row.names = FALSE)
CBE_dat <- read_csv("CB_east.csv")

# CBW_dat <- fIMOS_MatchMODIS(CBW_dat, pr, res_temp, res_spat) #
# write.csv(CBW_dat_15km, "CB_west.csv", row.names = FALSE)
# CBW_dat <- read_csv("CB_west.csv")
# 
# EHE_dat <- fIMOS_MatchMODIS(EHE_dat, pr, res_temp, res_spat) #
# write.csv(EHE_dat, "EH_east.csv", row.names = FALSE)
# EHE_dat <- read_csv("EH_east.csv")
# 
# EHW_dat <- fIMOS_MatchMODIS(EHW_dat, pr, res_temp, res_spat) #
# write.csv(EHW_dat_15km, "EH_west.csv", row.names = FALSE)
# EHW_dat <- read_csv("EH_west.csv")
# 
# NSE_dat <- fIMOS_MatchMODIS(NSE_dat, pr, res_temp, res_spat) #
# write.csv(NSE_dat, "NS_east.csv", row.names = FALSE)
# NSE_dat <- read_csv("NS_east.csv")
# 
# NSW_dat <- fIMOS_MatchMODIS(NSW_dat, pr, res_temp, res_spat) #
# write.csv(NSW_dat_15km, "NS_west.csv", row.names = FALSE)
# NSW_dat <- read_csv("NS_west.csv")
# 
# DHE_dat <- fIMOS_MatchMODIS(DHE_dat, pr, res_temp, res_spat) #
# write.csv(DHE_dat, "DH_east.csv", row.names = FALSE)
# DHE_dat <- read_csv("DH_east.csv")
# 
# 
# DHW_dat <- fIMOS_MatchMODIS(DHW_dat, pr, res_temp, res_spat) #
# write.csv(DHW_dat_15km, "DH_west.csv", row.names = FALSE)
# DHW_dat <- read_csv("DH_west.csv")