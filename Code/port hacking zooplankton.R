# Port Hacking zooplankton

library(tidyverse)
library(lubridate)

mydata <- read_csv("Data/NRS Zooplankton PH.csv")

str(mydata)

p1 <- ggplot(mydata, aes(x = Month, y = log10(ZoopAbundance_m3))) + geom_point() +
  geom_smooth() + scale_x_continuous(breaks = seq(1,12)) + theme_classic()
p1

ggsave("plots/Port Hacking Seasonality.png", height = 14.8, width = 21, units = "cm", dpi = 600)

mydata <- filter(mydata, Salinity_psu != -999)


p2 <- ggplot(mydata, aes(x = Salinity_psu, y = SST_C, size = log10(ZoopAbundance_m3))) + geom_point() +
   theme_classic()
p2

p2 <- ggplot(mydata, aes(x = Salinity_psu, y = SST_C, size = ZooBiomass_mgCarbon_m3)) + geom_point() +
  theme_classic()
p2

p3 <- ggplot(mydata, aes(x = Salinity_psu, y = SST_C, size = log10(ChlorophyllMonthlyClimatology_mg_m3))) +
 geom_point() +  theme_classic()
p3

p4 <- ggplot(mydata, aes(y = log10(ZoopAbundance_m3), x = log10(ChlorophyllMonthlyClimatology_mg_m3))) +
  geom_point() +  theme_classic()
p4

head(mydata)

### Load PH Velocity Data
vel_dat <- read_csv("Data/Velocity Timeseries/Port Hacking/IMOS_aggregation_20200330T024301Z.csv")

# Rotate coastline
vel_dat$U_shore = 0
vel_dat$V_shore = 0
rot_deg_angle= -24
 vel_dat$U_shore = cos(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)` + sin(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`
 vel_dat$V_shore= sin(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)` + cos(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`

head(vel_dat) 

vel_dat$Date <- vel_dat$`TIME (UTC)`
vel_dat$Month <- month(vel_dat$Date)
vel_dat$Year <- year(vel_dat$Date)

sum_dat <- vel_dat %>% group_by(Month) %>% summarise(Velocity = mean(V_shore*-1), SD = sd(V_shore), n = n(), SE = SD/sqrt(n))
sum_dat

pV <- ggplot(sum_dat, aes(x = Month, y = Velocity)) + geom_line() +
 scale_x_continuous(breaks = seq(1,12)) + theme_classic() +
  geom_errorbar(aes(ymin=Velocity-SE, ymax=Velocity+SE), width=.2, show.legend = FALSE)
pV

# To match plankton to velocity

Year_dat <- vel_dat %>% group_by(Month, Year) %>% summarise(Velocity = mean(V_shore*-1), SD = sd(V_shore), n = n(), SE = SD/sqrt(n))
Year_dat

full_dat <- full_join(Year_dat, mydata, by = c("Month", "Year"))

plot(log10(full_dat$ZoopAbundance_m3), full_dat$Velocity)

ggplot(full_dat, aes(x = Velocity, y = log10(ZoopAbundance_m3))) + geom_point() +
  geom_smooth(method = "lm") + theme_classic() +xlab("Southward Alongshore Velocity (m/s)")

ggsave("plots/velocity plankton mean monthly.png", width = 21, height = 14.8, units = "cm", dpi = 600)
ggsave("plots/velocity plankton mean monthly.pdf", width = 21, height = 14.8, units = "cm", dpi = 600)

# Use Jason code to get velocity on each specific day of sampling
# 
# source("D:/IMOS Data/IMOS_Toolbox/fIMOS_MatchMODIS.R")
# source("D:/IMOS Data/IMOS_Toolbox/fIMOS_MatchAltimetry.R")
# 
# mydata <- mydata %>% 
#   rename(Date = SampleDateLocal)
# 
# dat <- fIMOS_MatchAltimetry(mydata, res_spat)
# 
# write.csv(dat, "Data/Port Hacking Zooplankton with velocity.csv", row.names = F)

dat <- read_csv("Data/Port Hacking Zooplankton with velocity.csv")
ggplot(dat, aes(x = VCUR, y = log10(ZoopAbundance_m3))) + geom_point() +
  geom_smooth(method = "lm")



# Rotate coastline
dat$U_shore = 0
dat$V_shore = 0
rot_deg_angle= -24
dat$U_shore = cos(rot_deg_angle*pi/180)*dat$UCUR + sin(rot_deg_angle*pi/180)*dat$VCUR
dat$V_shore= sin(rot_deg_angle*pi/180)*dat$UCUR + cos(rot_deg_angle*pi/180)*dat$VCUR

ggplot(dat, aes(x = V_shore, y = log10(ZoopAbundance_m3))) + geom_point() +
  geom_smooth(method = "lm")

ggsave("plots/velocity plankton daily Port Hacking.png", width = 21, height = 14.8, units = "cm", dpi = 600)
ggsave("plots/velocity plankton daily Port Hacking.pdf", width = 21, height = 14.8, units = "cm", dpi = 600)

# but diversity seems to increase with southward flow 
ggplot(dat, aes(x = V_shore, y = ShannonCopepodDiversity)) + geom_point() +
  geom_smooth(method = "lm") + theme_classic()

ggsave("plots/velocity plankton daily Port Hacking diversity.png", width = 21, height = 14.8, units = "cm", dpi = 600)
ggsave("plots/velocity plankton daily Port Hacking diversity.pdf", width = 21, height = 14.8, units = "cm", dpi = 600)


head(dat)

dat2 <- filter(dat, Month > 8 & Month < 12)

pTS <- ggplot(dat2, aes(x = Salt, y = Temp, col = log10(Biomass))) + geom_point() + 
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pTS


### Load Biomass Data

BDat <- read_csv("Data/NRS Biomass.csv")
ADat <- read_csv("Data/NRS PH NS zooplankton abundance.csv")
#WDat <- read_csv("Data/NRS Water Quality.csv")

#WDat2 <- WDat %>% filter(SAMPLE_DEPTH_M <51) %>% group_by(STATION_NAME, LOCAL_TRIP_START_TIME) %>% summarise(Salinty = mean(SALINITY, na.rm=T))
#WDat2

head(BDat)

BDat$Date <- str_split(BDat$LOCAL_TRIP_START_TIME, " ", simplify = TRUE)[,1]

BDat$Date <- as.Date(BDat$Date, format = "%d/%m/%Y")
#WDat2$Date <- date(WDat2$LOCAL_TRIP_START_TIME)

#full_BDAT <- left_join(BDat, WDat2, b = c("STATION_NAME", "Date"))
#full_BDAT <- full_BDAT %>% drop_na("Salinty")

ADat$Date <- date(ADat$SampleDateLocal)
ADat$STATION_NAME <- ADat$Station

full_BDAT <- left_join(BDat, ADat, b = c("STATION_NAME", "Date"))

full_BDAT <- full_BDAT %>% filter(Salinity_psu > 0 & SST_C > 0)

pTS <- ggplot(full_BDAT, aes(x = Salinity_psu, y = SST_C, col = log10(MG_PER_M3))) + geom_point() + 
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16)) +
  facet_wrap(~STATION_NAME)
pTS

ggsave("plots/NRS TS Zooplankton Plots.png", width = 21, height = 14.8, units = "cm", dpi = 600)

# Only Sept - Nov
full_BDAT2 <- filter(full_BDAT, Month > 8 & Month < 12)

pTS <- ggplot(full_BDAT2, aes(x = Salinity_psu, y = SST_C, col = log10(MG_PER_M3))) + geom_point() + 
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16)) +
  facet_wrap(~STATION_NAME)
pTS

ggsave("plots/NRS TS Zooplankton Plots spring.png", width = 21, height = 14.8, units = "cm", dpi = 600)
