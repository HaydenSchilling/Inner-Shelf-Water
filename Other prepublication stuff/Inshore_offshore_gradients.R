# Looking at the temperature gradient from inner shelf to outer shelf

## Need to get location and date matrix then extract SST from satelites

# As inshore, let's use 15km from shore


library(tidyverse)

# #inshore locations (15 km from shore):
#  # CB - -28.632 , 153.794
#  # EH - -29 , 153.637
#  # NS - -30 , 153.384
#  # DH - -31.75, 152.944
# sitesX <- data.frame(Site = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
#                     Latitude_Inshore = c(-28.632, -29, -30, -31.75),
#                     Longitude_Inshore = c(153.794, 153.637, 153.384, 152.944))
# 
# # For 8km offshore as inshore locations
# #sitesX <- data.frame(Site = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
# #                     Latitude_Inshore = c(-28.63198, -29, -30, -31.75),
# #                     Longitude_Inshore = c(153.7224, 153.5652, 153.3115, 152.8702))
# 
# # Offshore locations
# library(geosphere)
# destPoint(p = c(sitesX$Longitude_Inshore[1], sitesX$Latitude_Inshore[1]), b = 90, d = 30000)
# destPoint(p = c(sitesX$Longitude_Inshore[2], sitesX$Latitude_Inshore[2]), b = 90, d = 30000)
# destPoint(p = c(sitesX$Longitude_Inshore[3], sitesX$Latitude_Inshore[3]), b = 90, d = 30000)
# destPoint(p = c(sitesX$Longitude_Inshore[4], sitesX$Latitude_Inshore[4]), b = 90, d = 30000)
# 
# # # get 8km offshore locations
# # destPoint(p = c(sitesX$Longitude_Inshore[1], sitesX$Latitude_Inshore[1]), b = 270, d = 7000)
# # destPoint(p = c(sitesX$Longitude_Inshore[2], sitesX$Latitude_Inshore[2]), b = 270, d = 7000)
# # destPoint(p = c(sitesX$Longitude_Inshore[3], sitesX$Latitude_Inshore[3]), b = 270, d = 7000)
# # destPoint(p = c(sitesX$Longitude_Inshore[4], sitesX$Latitude_Inshore[4]), b = 270, d = 7000)
# 
# # Offshore locations (45km offshore)
# sitesX$Latitude_Offshore <- c(-28.63165, -28.99976, -29.99963, -31.74961)
# sitesX$Longitude_Offshore <- c(154.1008, 153.9449, 153.6949, 153.2606)
# 
# sitesX
# 
# # Now make date string
# # let's to 2004 - 2013 (10 years)
# 
# Date = seq.Date(from = as.Date("2004-01-01") , to = as.Date("2013-12-31"), by = 1)
# 
# 
# ### Make a dataframe for each site
# sites = levels(sitesX$Site)
# sites
# 
# df_list <- list()
# 
# for (i in sites){
#   dat2 <- filter(sitesX, Site == i)
#   assign(paste0("dat_",i,"_West"), data.frame(Longitude = dat2$Longitude_Inshore[1], Latitude = dat2$Latitude_Inshore[1],
#                                               Date =seq.Date(from = as.Date("2004-01-01") , to = as.Date("2013-12-31"), by = 1),
#                                               Location = paste0(i,"_West")))
#   assign(paste0("dat_",i,"_East"), data.frame(Longitude = dat2$Longitude_Offshore[1], Latitude = dat2$Latitude_Offshore[1],
#                                               Date = seq.Date(from = as.Date("2004-01-01") , to = as.Date("2013-12-31"), by = 1),
#                                               Location = paste0(i,"_East")))
# }
# 
# # merge into a single dataframe
# df_list <- list(dat_CapeByron_East, dat_CapeByron_West, dat_DiamondHead_East, dat_DiamondHead_West,
#                 dat_EvansHead_East, dat_EvansHead_West, dat_NorthSolitary_East, dat_NorthSolitary_West)
# full_dat <- bind_rows(df_list)
# full_dat$Location <- as.factor(full_dat$Location)
# str(full_dat)
# 
# 
# # 
# # 
# # ############################
# # # 
# # Test a smaller dataset (works well)
# #full_dat <- head(full_dat)
# 
# # # remove dodgy dates from MODIS
# # full_dat <- filter(full_dat, Date != "2004-12-04")
# # full_dat <- filter(full_dat, Date != "2006-07-05")
# # full_dat <- filter(full_dat, Date != "2007-06-02")
# # full_dat <- filter(full_dat, Date != "2007-12-02")
# #
# #
# CBE_dat <- filter(full_dat, Location == "CapeByron_East")
# CBW_dat <- filter(full_dat, Location == "CapeByron_West")
# DHE_dat <- filter(full_dat, Location == "DiamondHead_East")
# DHW_dat <- filter(full_dat, Location == "DiamondHead_West")
# NSE_dat <- filter(full_dat, Location == "NorthSolitary_East")
# NSW_dat <- filter(full_dat, Location == "NorthSolitary_West")
# EHE_dat <- filter(full_dat, Location == "EvansHead_East")
# EHW_dat <- filter(full_dat, Location == "EvansHead_West")
# 
# 
# head(full_dat)
# 
# ## Load download code
# source("../../../IMOS Data/IMOS_Toolbox/fIMOS_MatchMODIS.R")
# #source("fIMOS_MatchMODIS.R")
# 
# 
# # Possible products
# # pr <- c("sst_quality", "sst", "picop_brewin2012in", "picop_brewin2010at", "par",
# #         "owtd", "npp_vgpm_eppley_oc3", "npp_vgpm_eppley_gsm", "nanop_brewin2012in",
# #         "nanop_brewin2010at", "l2_flags", "ipar", "dt", "chl_oc3", "chl_gsm", "K_490")
# 
# pr <- c("sst") # select products here eg: "chl_oc3", "sst"
# # # Set resolutions
# # res_temp <- "1d" # temporal resolutions
# # res_spat <- 5 # Return the average of res_spat x res_spat pixels
# # 
# # #dat <- fIMOS_MatchMODIS(full_dat, pr, res_temp, res_spat)
# # #str(dat)
# # 
# 
# 
# #write.csv(dat, "Inshore_Offshore_Gradient.csv", row.names = F)


# 
#CBE_dat <- fIMOS_MatchMODIS(CBE_dat, pr, res_temp, res_spat) #
#write.csv(CBE_dat, "CB_east.csv", row.names = FALSE)
CBE_dat <- read_csv("CB_east.csv")

#CBW_dat <- fIMOS_MatchMODIS(CBW_dat, pr, res_temp, res_spat) #
#write.csv(CBW_dat_15km, "CB_west.csv", row.names = FALSE)
CBW_dat <- read_csv("CB_west.csv")

#EHE_dat <- fIMOS_MatchMODIS(EHE_dat, pr, res_temp, res_spat) #
#write.csv(EHE_dat, "EH_east.csv", row.names = FALSE)
EHE_dat <- read_csv("EH_east.csv")

#EHW_dat <- fIMOS_MatchMODIS(EHW_dat, pr, res_temp, res_spat) #
#write.csv(EHW_dat_15km, "EH_west.csv", row.names = FALSE)
EHW_dat <- read_csv("EH_west.csv")

#NSE_dat <- fIMOS_MatchMODIS(NSE_dat, pr, res_temp, res_spat) #
#write.csv(NSE_dat, "NS_east.csv", row.names = FALSE)
NSE_dat <- read_csv("NS_east.csv")

#NSW_dat <- fIMOS_MatchMODIS(NSW_dat, pr, res_temp, res_spat) #
#write.csv(NSW_dat_15km, "NS_west.csv", row.names = FALSE)
NSW_dat <- read_csv("NS_west.csv")

#DHE_dat <- fIMOS_MatchMODIS(DHE_dat, pr, res_temp, res_spat) #
#write.csv(DHE_dat, "DH_east.csv", row.names = FALSE)
DHE_dat <- read_csv("DH_east.csv")


#DHW_dat <- fIMOS_MatchMODIS(DHW_dat, pr, res_temp, res_spat) #
#write.csv(DHW_dat_15km, "DH_west.csv", row.names = FALSE)
DHW_dat <- read_csv("DH_west.csv")

# # 
df_list <- list(CBE_dat, CBW_dat, EHE_dat,EHW_dat, NSW_dat, NSE_dat, DHE_dat, DHW_dat) # fill with more later
dat <- bind_rows(df_list)


dat2 <- str_split_fixed(dat$Location, pattern = "_", n = 2)
dat$Transect <- as.factor(dat2[,1])
dat$East_West <- as.factor(dat2[,2])
# Remove data columns that don't match for east and west data
dat$Location <- NULL
dat$Latitude <- NULL
dat$Longitude <- NULL
head(dat)

datX <- dat %>% spread(key = East_West, value = sst_1d,  fill = NA) %>% mutate(Offshore_Inshore = East - West)

head(datX)
tail(datX)
hist(datX$Offshore_Inshore)
summary(datX$Offshore_Inshore)
levels(dat$East_West)
levels(dat$Transect)

datX$Transect <- factor(datX$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

write.csv(datX, "Data/Temp gradient all sites.csv", row.names = F)

library(tidyverse)

datX <- read_csv("Data/Temp gradient all sites.csv")
datX$Transect <- factor(datX$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
datX <- filter(datX, Offshore_Inshore <= 8)
head(datX)


# count all samples
dat_sum_all <- datX %>% group_by(Transect) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all
dat_sum_all$Transect <- factor(dat_sum_all$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum <- datX %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect) %>% drop_na() %>%
  summarise(n = n())
dat_sum
dat_sum$Transect <- factor(dat_sum$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



p1 <- ggplot(datX, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Transect) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore Temp (deg C)") + ylab("Proportion") +
geom_label(data = dat_sum, aes(x = 4.8, y = 0.075, 
    label = paste("Mean: ", round(dat_sum_all$mean, 1), "\nMedian: ", 
    round(dat_sum_all$median,1), "\nSD: ", round(dat_sum_all$sd, 1),
    "\n% > 1 deg:", round(n/dat_sum_all$n *100,1))))
p1

ggsave("plots/Inshore_Offshore_Temp_Gradiet.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

# Check Seasonal Patterns
# Cape Byron
# count all samples
datX2_cb <- filter(datX, Transect == "CapeByron")

dat_sum_all_cb <- datX2_cb %>% group_by(Transect, Month) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore)) %>%
  drop_na()
dat_sum_all_cb
dat_sum_all_cb$Transect <- factor(dat_sum_all_cb$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

# count all samples with a difference > 1
dat_sum_cb <- datX2_cb %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month) %>% drop_na() %>%
  summarise(n = n()) %>% drop_na()
dat_sum_cb
dat_sum_cb$Transect <- factor(dat_sum_cb$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


p1 <- ggplot(datX2_cb, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Month) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore Temp (deg C)") + ylab("Proportion") +
  geom_label(data = dat_sum_cb, aes(x = 4.8, y = 0.075, 
                                 label = paste("Mean: ", round(dat_sum_all_cb$mean, 1), "\nMedian: ", 
                                               round(dat_sum_all_cb$median,1), "\nSD: ", round(dat_sum_all_cb$sd, 1),
                                               "\n% > 1 deg:", round(dat_sum_cb$n/dat_sum_all_cb$n *100,1))))
p1
ggsave("plots/Inshore_Offshore_Temp_Gradiet CB Months.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

dat_sum_all_cb$Percent_1 <-  dat_sum_cb$n/dat_sum_all_cb$n *100
dat_sum_all_cb <- dat_sum_all_cb %>% drop_na()
dat_sum_all_cb

# Evans Head
# count all samples
datX2_EH <- filter(datX, Transect == "EvansHead")

dat_sum_all_EH <- datX2_EH %>% group_by(Transect, Month) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_EH
dat_sum_all_EH$Transect <- factor(dat_sum_all_EH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_EH <- datX2_EH %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month) %>% drop_na() %>%
  summarise(n = n())
dat_sum_EH
dat_sum_EH$Transect <- factor(dat_sum_EH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


p1 <- ggplot(datX2_EH, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Month) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore Temp (deg C)") + ylab("Proportion") +
  geom_label(data = dat_sum_EH, aes(x = 4.8, y = 0.075, 
                                 label = paste("Mean: ", round(dat_sum_all_EH$mean, 1), "\nMedian: ", 
                                               round(dat_sum_all_EH$median,1), "\nSD: ", round(dat_sum_all_EH$sd, 1),
                                               "\n% > 1 deg:", round(n/dat_sum_all_EH$n *100,1))))
p1
ggsave("plots/Inshore_Offshore_Temp_Gradiet EH Months.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

dat_sum_all_EH$Percent_1 <-  dat_sum_EH$n/dat_sum_all_EH$n *100
dat_sum_all_EH <- dat_sum_all_EH %>% drop_na()
dat_sum_all_EH

# North Solitary
# count all samples
datX2_NS <- filter(datX, Transect == "NorthSolitary")

dat_sum_all_NS <- datX2_NS %>% group_by(Transect, Month) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_NS
dat_sum_all_NS$Transect <- factor(dat_sum_all_NS$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_NS <- datX2_NS %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month) %>% drop_na() %>%
  summarise(n = n())
dat_sum_NS
dat_sum_NS$Transect <- factor(dat_sum_NS$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


p1 <- ggplot(datX2_NS, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Month) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore Temp (deg C)") + ylab("Proportion") +
  geom_label(data = dat_sum_NS, aes(x = 4.8, y = 0.075, 
                                 label = paste("Mean: ", round(dat_sum_all_NS$mean, 1), "\nMedian: ", 
                                               round(dat_sum_all_NS$median,1), "\nSD: ", round(dat_sum_all_NS$sd, 1),
                                               "\n% > 1 deg:", round(n/dat_sum_all_NS$n *100,1))))
p1
ggsave("plots/Inshore_Offshore_Temp_Gradiet NS Months.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

dat_sum_all_NS$Percent_1 <-  dat_sum_NS$n/dat_sum_all_NS$n *100
dat_sum_all_NS <- dat_sum_all_NS %>% drop_na()
dat_sum_all_NS

# Diamond Head
# count all samples
datX2_DH <- filter(datX, Transect == "DiamondHead")

dat_sum_all_DH <- datX2_DH %>% group_by(Transect, Month) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_DH
dat_sum_all_DH$Transect <- factor(dat_sum_all_DH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_DH <- datX2_DH %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month) %>% drop_na() %>%
  summarise(n = n())
dat_sum_DH
dat_sum_DH$Transect <- factor(dat_sum_DH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


p1 <- ggplot(datX2_DH, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Month) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore Temp (deg C)") + ylab("Proportion") +
  geom_label(data = dat_sum_DH, aes(x = 4.8, y = 0.075, 
                                 label = paste("Mean: ", round(dat_sum_all_DH$mean, 1), "\nMedian: ", 
                                               round(dat_sum_all_DH$median,1), "\nSD: ", round(dat_sum_all_DH$sd, 1),
                                               "\n% > 1 deg:", round(n/dat_sum_all_DH$n *100,1))))
p1
ggsave("plots/Inshore_Offshore_Temp_Gradiet DH Months.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

dat_sum_all_DH$Percent_1 <-  dat_sum_DH$n/dat_sum_all_DH$n *100
dat_sum_all_DH <- dat_sum_all_DH %>% drop_na()
dat_sum_all_DH
### Merge all sites together into dataframe
seasonal_full <- bind_rows(dat_sum_all_cb, dat_sum_all_EH, dat_sum_all_NS, dat_sum_all_DH)
head(seasonal_full)

p_season <- ggplot(seasonal_full, aes(x = Month, y = Percent_1, lty = Transect)) + 
  geom_line(size = 2) + theme_classic() + ylab("Temperature Gradient > 1 °C (% Days)") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 20),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 20),
        axis.text.y  = element_text(colour="black", size = 16)) +
  scale_x_continuous(limits=c(0.8, 12.1), breaks = c(1:12))
p_season

ggsave("plots/Inshore Offshore Seasonal patterns.png", dpi = 600, height = 14.8, width = 21, units ="cm")

#########################################################################################################################
### TO make with error bars
#########################################################################################################################

datX <- read_csv("Data/Temp gradient all sites.csv")
datX$Transect <- factor(datX$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
datX <- filter(datX, Offshore_Inshore <= 8)
head(datX)


# count all samples
dat_sum_all <- datX %>% group_by(Transect) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all
dat_sum_all$Transect <- factor(dat_sum_all$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum <- datX %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect) %>% drop_na() %>%
  summarise(n = n())
dat_sum
dat_sum$Transect <- factor(dat_sum$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



p1 <- ggplot(datX, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Transect) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore Temp (deg C)") + ylab("Proportion") +
  geom_label(data = dat_sum, aes(x = 4.8, y = 0.075, 
                                 label = paste("Mean: ", round(dat_sum_all$mean, 1), "\nMedian: ", 
                                               round(dat_sum_all$median,1), "\nSD: ", round(dat_sum_all$sd, 1),
                                               "\n% > 1 deg:", round(n/dat_sum_all$n *100,1))))
p1

ggsave("plots/Inshore_Offshore_Temp_Gradiet.png", width = 21.8, height = 14.8, dpi = 600, units = "cm")
ggsave("plots/Inshore_Offshore_Temp_Gradiet.pdf", width = 21.8, height = 14.8, units = "cm")

# Check Seasonal Patterns
# Cape Byron ############################################################# This code does work
# count all samples
datX2_cb <- filter(datX, Transect == "CapeByron")
datX2_cb$Month <- as.factor(as.character(datX2_cb$Month))
datX2_cb$Year <- as.factor(as.character(datX2_cb$Year))

dat_sum_all_cb <- datX2_cb %>% group_by(Transect, Month, Year) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n()) #%>%
  #drop_na()
dat_sum_all_cb
dat_sum_all_cb$Transect <- factor(dat_sum_all_cb$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

# count all samples with a difference > 1
dat_sum_cb <- datX2_cb %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop = FALSE) %>% drop_na(Offshore_Inshore) %>%
  summarise(n = n())# %>% drop_na()
dat_sum_cb
dat_sum_cb$Transect <- factor(dat_sum_cb$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

#ggsave("plots/Inshore_Offshore_Temp_Gradiet CB Months_5km.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

### need to combine both dataframes to make them equal....

dat_sum_all_cb$Percent_1 <-  dat_sum_cb$n/dat_sum_all_cb$n *100
dat_sum_all_cb <- dat_sum_all_cb %>% drop_na()
dat_sum_all_cb

# Evans Head
# count all samples
datX2_EH <- filter(datX, Transect == "EvansHead")
datX2_EH$Month <- as.factor(as.character(datX2_EH$Month))
datX2_EH$Year <- as.factor(as.character(datX2_EH$Year))

dat_sum_all_EH <- datX2_EH %>% group_by(Transect, Month, Year, .drop =FALSE) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_EH
dat_sum_all_EH$Transect <- factor(dat_sum_all_EH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_EH <- datX2_EH %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop = FALSE) %>% drop_na() %>%
  summarise(n = n())
dat_sum_EH
dat_sum_EH$Transect <- factor(dat_sum_EH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



dat_sum_all_EH$Percent_1 <-  dat_sum_EH$n/dat_sum_all_EH$n *100
dat_sum_all_EH <- dat_sum_all_EH %>% drop_na()
dat_sum_all_EH

# North Solitary
# count all samples
datX2_NS <- filter(datX, Transect == "NorthSolitary")
datX2_NS$Month <- as.factor(as.character(datX2_NS$Month))
datX2_NS$Year <- as.factor(as.character(datX2_NS$Year))

dat_sum_all_NS <- datX2_NS %>% group_by(Transect, Month, Year, .drop =FALSE) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_NS
dat_sum_all_NS$Transect <- factor(dat_sum_all_NS$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_NS <- datX2_NS %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop =F) %>% drop_na() %>%
  summarise(n = n())
dat_sum_NS
dat_sum_NS$Transect <- factor(dat_sum_NS$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


dat_sum_all_NS$Percent_1 <-  dat_sum_NS$n/dat_sum_all_NS$n *100
dat_sum_all_NS <- dat_sum_all_NS %>% drop_na()
dat_sum_all_NS

# Diamond Head
# count all samples
datX2_DH <- filter(datX, Transect == "DiamondHead")
datX2_DH$Month <- as.factor(as.character(datX2_DH$Month))
datX2_DH$Year <- as.factor(as.character(datX2_DH$Year))

dat_sum_all_DH <- datX2_DH %>% group_by(Transect, Month, Year, .drop = F) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_DH
dat_sum_all_DH$Transect <- factor(dat_sum_all_DH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_DH <- datX2_DH %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop =F) %>% drop_na() %>%
  summarise(n = n())
dat_sum_DH
dat_sum_DH$Transect <- factor(dat_sum_DH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



dat_sum_all_DH$Percent_1 <-  dat_sum_DH$n/dat_sum_all_DH$n *100
dat_sum_all_DH <- dat_sum_all_DH %>% drop_na()
dat_sum_all_DH
### Merge all sites together into dataframe
seasonal_full <- bind_rows(dat_sum_all_cb, dat_sum_all_EH, dat_sum_all_NS, dat_sum_all_DH)
head(seasonal_full)


dat_plot <- seasonal_full %>% group_by(Transect, Month) %>% summarise(mean_percent = mean(Percent_1),
                                                                      sd = sd(Percent_1), n = n(),
                                                                      SE = sd/sqrt(n))
dat_plot$Month <- as.numeric(as.character(dat_plot$Month))
head(dat_plot)

p_season <- ggplot(dat_plot, aes(x = Month, y = mean_percent, lty = Transect)) +  geom_point() +
  geom_line(size = 1.5) + theme_classic() + ylab("Temperature Gradient > 1 °C (% ± SE)") +
  geom_errorbar(aes(ymin=mean_percent-SE, ymax=mean_percent+SE), width=.2, show.legend = FALSE)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        legend.key.width = unit(3, "line"),
        legend.position=c(.4, .9),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=10)) +
  scale_x_continuous(limits=c(0.8, 12.1), breaks = c(1:12)) +
  scale_linetype_discrete(name  ="Transect",
                          breaks=c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
                          labels=c("Cape Byron (28.6 °S)", "Evans Head (29 °S)", "North Solitary (30 °S)", "Diamond Head (31.8 °S)"))
p_season

ggsave("plots/Inshore Offshore Seasonal patterns.png", dpi = 600, height = 14.8, width = 21, units ="cm")



# #########################################################################################################################
# ### TO make with error bars for 8km offshore not 15
# #########################################################################################################################
# # 
# #CBE_dat <- fIMOS_MatchMODIS(CBE_dat, pr, res_temp, res_spat) #
# #write.csv(CBE_dat, "CB_east.csv", row.names = FALSE)
# CBE_dat <- read_csv("CB_east.csv")
# 
# #CBW_dat <- fIMOS_MatchMODIS(CBW_dat, pr, res_temp, res_spat) #
# #write.csv(CBW_dat_15km, "CB_west.csv", row.names = FALSE)
# CBW_dat <- read_csv("CB_west_8km.csv")
# 
# #EHE_dat <- fIMOS_MatchMODIS(EHE_dat, pr, res_temp, res_spat) #
# #write.csv(EHE_dat, "EH_east.csv", row.names = FALSE)
# EHE_dat <- read_csv("EH_east.csv")
# 
# #EHW_dat <- fIMOS_MatchMODIS(EHW_dat, pr, res_temp, res_spat) #
# #write.csv(EHW_dat_15km, "EH_west.csv", row.names = FALSE)
# EHW_dat <- read_csv("EH_west_8km.csv")
# 
# #NSE_dat <- fIMOS_MatchMODIS(NSE_dat, pr, res_temp, res_spat) #
# #write.csv(NSE_dat, "NS_east.csv", row.names = FALSE)
# NSE_dat <- read_csv("NS_east.csv")
# 
# #NSW_dat <- fIMOS_MatchMODIS(NSW_dat, pr, res_temp, res_spat) #
# #write.csv(NSW_dat_15km, "NS_west.csv", row.names = FALSE)
# NSW_dat <- read_csv("NS_west_8km.csv")
# 
# #DHE_dat <- fIMOS_MatchMODIS(DHE_dat, pr, res_temp, res_spat) #
# #write.csv(DHE_dat, "DH_east.csv", row.names = FALSE)
# DHE_dat <- read_csv("DH_east.csv")
# 
# 
# #DHW_dat <- fIMOS_MatchMODIS(DHW_dat, pr, res_temp, res_spat) #
# #write.csv(DHW_dat_15km, "DH_west.csv", row.names = FALSE)
# DHW_dat <- read_csv("DH_west_8km.csv")
# 
# # # 
# df_list <- list(CBE_dat, CBW_dat, EHE_dat,EHW_dat, NSW_dat, NSE_dat, DHE_dat, DHW_dat) # fill with more later
# dat <- bind_rows(df_list)
# 
# 
# dat2 <- str_split_fixed(dat$Location, pattern = "_", n = 2)
# dat$Transect <- as.factor(dat2[,1])
# dat$East_West <- as.factor(dat2[,2])
# # Remove data columns that don't match for east and west data
# dat$Location <- NULL
# dat$Latitude <- NULL
# dat$Longitude <- NULL
# head(dat)
# 
# datX <- dat %>% spread(key = East_West, value = sst_1d,  fill = NA) %>% mutate(Offshore_Inshore = East - West)
# 
# head(datX)
# tail(datX)
# hist(datX$Offshore_Inshore)
# summary(datX$Offshore_Inshore)
# levels(dat$East_West)
# levels(dat$Transect)
# 
# datX$Transect <- factor(datX$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
# 
# write.csv(datX, "Data/Temp gradient all sites 8km.csv", row.names = F)
# 

#################

datX <- read_csv("Data/Temp gradient all sites 8km.csv")
datX$Transect <- factor(datX$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
datX <- filter(datX, Offshore_Inshore <= 8)
head(datX)


# count all samples
dat_sum_all <- datX %>% group_by(Transect) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all
dat_sum_all$Transect <- factor(dat_sum_all$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum <- datX %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect) %>% drop_na() %>%
  summarise(n = n())
dat_sum
dat_sum$Transect <- factor(dat_sum$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



p1 <- ggplot(datX, aes(x = Offshore_Inshore)) + geom_histogram(aes(y = stat(count / sum(count))), binwidth = 0.5) +
  facet_wrap(~Transect) + theme_bw() + geom_vline(aes(xintercept = 0),lty = 2) +
  xlab("Offshore - Inshore SST (°C)") + ylab("Proportion") +
  geom_label(data = dat_sum, aes(x = 4.8, y = 0.075, 
                                 label = paste("Mean: ", round(dat_sum_all$mean, 1), "\nMedian: ", 
                                               round(dat_sum_all$median,1), "\nSD: ", round(dat_sum_all$sd, 1),
                                               "\n% > 1 °C:", round(n/dat_sum_all$n *100,1)))) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14))
p1

ggsave("plots/Inshore_Offshore_Temp_Gradiet_8km.png", width = 21.8, height = 14.8, dpi = 600, units = "cm")
ggsave("plots/Inshore_Offshore_Temp_Gradiet_8km.pdf", width = 21.8, height = 14.8, units = "cm")


# Check Seasonal Patterns
# Cape Byron ############################################################# This code does work
# count all samples
datX2_cb <- filter(datX, Transect == "CapeByron")
datX2_cb$Month <- as.factor(as.character(datX2_cb$Month))
datX2_cb$Year <- as.factor(as.character(datX2_cb$Year))

dat_sum_all_cb <- datX2_cb %>% group_by(Transect, Month, Year) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n()) #%>%
#drop_na()
dat_sum_all_cb
dat_sum_all_cb$Transect <- factor(dat_sum_all_cb$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

# count all samples with a difference > 1
dat_sum_cb <- datX2_cb %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop = FALSE) %>% drop_na(Offshore_Inshore) %>%
  summarise(n = n())# %>% drop_na()
dat_sum_cb
dat_sum_cb$Transect <- factor(dat_sum_cb$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

#ggsave("plots/Inshore_Offshore_Temp_Gradiet CB Months_5km.png", height = 21.8, width = 14.8*2, dpi = 600, units = "cm")

### need to combine both dataframes to make them equal....

dat_sum_all_cb$Percent_1 <-  dat_sum_cb$n/dat_sum_all_cb$n *100
dat_sum_all_cb <- dat_sum_all_cb %>% drop_na()
dat_sum_all_cb

# Evans Head
# count all samples
datX2_EH <- filter(datX, Transect == "EvansHead")
datX2_EH$Month <- as.factor(as.character(datX2_EH$Month))
datX2_EH$Year <- as.factor(as.character(datX2_EH$Year))

dat_sum_all_EH <- datX2_EH %>% group_by(Transect, Month, Year, .drop =FALSE) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_EH
dat_sum_all_EH$Transect <- factor(dat_sum_all_EH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_EH <- datX2_EH %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop = FALSE) %>% drop_na() %>%
  summarise(n = n())
dat_sum_EH
dat_sum_EH$Transect <- factor(dat_sum_EH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



dat_sum_all_EH$Percent_1 <-  dat_sum_EH$n/dat_sum_all_EH$n *100
dat_sum_all_EH <- dat_sum_all_EH %>% drop_na()
dat_sum_all_EH

# North Solitary
# count all samples
datX2_NS <- filter(datX, Transect == "NorthSolitary")
datX2_NS$Month <- as.factor(as.character(datX2_NS$Month))
datX2_NS$Year <- as.factor(as.character(datX2_NS$Year))

dat_sum_all_NS <- datX2_NS %>% group_by(Transect, Month, Year, .drop =FALSE) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_NS
dat_sum_all_NS$Transect <- factor(dat_sum_all_NS$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_NS <- datX2_NS %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop =F) %>% drop_na() %>%
  summarise(n = n())
dat_sum_NS
dat_sum_NS$Transect <- factor(dat_sum_NS$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


dat_sum_all_NS$Percent_1 <-  dat_sum_NS$n/dat_sum_all_NS$n *100
dat_sum_all_NS <- dat_sum_all_NS %>% drop_na()
dat_sum_all_NS

# Diamond Head
# count all samples
datX2_DH <- filter(datX, Transect == "DiamondHead")
datX2_DH$Month <- as.factor(as.character(datX2_DH$Month))
datX2_DH$Year <- as.factor(as.character(datX2_DH$Year))

dat_sum_all_DH <- datX2_DH %>% group_by(Transect, Month, Year, .drop = F) %>% drop_na(Offshore_Inshore) %>% 
  summarise(n = n(), mean = mean(Offshore_Inshore), median = median(Offshore_Inshore), sd = sd(Offshore_Inshore))
dat_sum_all_DH
dat_sum_all_DH$Transect <- factor(dat_sum_all_DH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))


# count all samples with a difference > 1
dat_sum_DH <- datX2_DH %>% filter(Offshore_Inshore >= 1) %>% group_by(Transect, Month, Year, .drop =F) %>% drop_na() %>%
  summarise(n = n())
dat_sum_DH
dat_sum_DH$Transect <- factor(dat_sum_DH$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))



dat_sum_all_DH$Percent_1 <-  dat_sum_DH$n/dat_sum_all_DH$n *100
dat_sum_all_DH <- dat_sum_all_DH %>% drop_na()
dat_sum_all_DH
### Merge all sites together into dataframe
seasonal_full <- bind_rows(dat_sum_all_cb, dat_sum_all_EH, dat_sum_all_NS, dat_sum_all_DH)
head(seasonal_full)


dat_plot <- seasonal_full %>% group_by(Transect, Month) %>% summarise(mean_percent = mean(Percent_1),
                                                                      sd = sd(Percent_1), n = n(),
                                                                      SE = sd/sqrt(n))
dat_plot$Month <- as.numeric(as.character(dat_plot$Month))
head(dat_plot)

p_season <- ggplot(dat_plot, aes(x = Month, y = mean_percent, lty = Transect)) +  geom_point() +
  geom_line(size = 1.5) + theme_classic() + ylab("Temperature Gradient > 1 °C (% ± SE)") +
  geom_errorbar(aes(ymin=mean_percent-SE, ymax=mean_percent+SE), width=.2, show.legend = FALSE)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        legend.key.width = unit(3, "line"),
        legend.position=c(.75, .2),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=10)) +
  scale_x_continuous(limits=c(0.8, 12.1), breaks = c(1:12)) +
  scale_linetype_discrete(name  ="Transect",
                          breaks=c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
                          labels=c("Cape Byron (28.6 °S)", "Evans Head (29 °S)", "North Solitary (30 °S)", "Diamond Head (31.8 °S)"))
p_season

ggsave("plots/Inshore Offshore Seasonal patterns 8km.png", dpi = 600, height = 14.8, width = 21, units ="cm")

# Check change over time

# Check years
dat_plotY <- seasonal_full %>% group_by(Transect, Year) %>% summarise(mean_percent = mean(Percent_1),
                                                                      sd = sd(Percent_1), n = n(),
                                                                      SE = sd/sqrt(n))

p_seasonY <- ggplot(dat_plotY, aes(x = Year, y = mean_percent, lty = Transect)) +  geom_point() +
  geom_line(size = 1.5) + theme_classic() + ylab("Temperature Gradient > 1 °C (% ± SE)") + 
  geom_errorbar(aes(ymin=mean_percent-SE, ymax=mean_percent+SE), width=.2, show.legend = FALSE)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        legend.key.width = unit(3, "line"),
        legend.position=c(.25, .85),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=10)) +
  #scale_x_continuous(limits=c(0.8, 12.1), breaks = c(1:12)) +
  scale_linetype_discrete(name  ="Transect",
                          breaks=c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
                          labels=c("Cape Byron (28.6 °S)", "Evans Head (29 °S)", "North Solitary (30 °S)", "Diamond Head (31.8 °S)"))
p_seasonY


# Look at Monthly patterns

dat_plot <- seasonal_full %>% group_by(Transect, Month) %>% summarise(mean_percent = mean(Percent_1),
                                                                      sd = sd(Percent_1), n = n(),
                                                                      SE = sd/sqrt(n))

p_season <- ggplot(dat_plot, aes(x = Month, y = mean_percent, lty = Transect)) +  geom_point() +
  geom_line(size = 1.5) + theme_classic() + ylab("Temperature Gradient > 1 °C (% ± SE)") +
  geom_errorbar(aes(ymin=mean_percent-SE, ymax=mean_percent+SE), width=.2, show.legend = FALSE)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        legend.key.width = unit(3, "line"),
        legend.position=c(.25, .85),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=10)) +
  scale_x_continuous(limits=c(0.8, 12.1), breaks = c(1:12)) +
  scale_linetype_discrete(name  ="Transect",
                          breaks=c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
                          labels=c("Cape Byron (28.6 °S)", "Evans Head (29 °S)", "North Solitary (30 °S)", "Diamond Head (31.8 °S)"))
p_season

ggsave("plots/Inshore Offshore Seasonal patterns 15km.png", dpi = 600, height = 14.8, width = 21, units ="cm")
