# Analyse velocity seasonal patterns

library(tidyverse)
library(lubridate)

# Original data downloaded from AODN then combinded to a a single file.
# dat1 <- read_csv("Data/Velocity Timeseries/IMOS_aggregation_20200117T052356Z.csv")
# dat2 <- read_csv("Data/Velocity Timeseries/IMOS_aggregation_20200117T052633Z.csv")
# dat3 <- read_csv("Data/Velocity Timeseries/IMOS_aggregation_20200117T052729Z.csv")
# dat4 <- read_csv("Data/Velocity Timeseries/IMOS_aggregation_20200117T052821Z.csv")
# 
# head(dat1)
# 
# dat_list <- list(dat1, dat2, dat3, dat4)
# 
# full_dat <- bind_rows(dat_list)
# 
# table(full_dat$`LATITUDE (degrees_north)`)
# 
# full_dat$Transect <- as.character("")
# full_dat$Transect[full_dat$`LATITUDE (degrees_north)`  == "-31.8"] <- "DiamondHead"
# full_dat$Transect[full_dat$`LATITUDE (degrees_north)`  == "-30"] <- "NorthSolitary"
# full_dat$Transect[full_dat$`LATITUDE (degrees_north)`  == "-29"] <- "EvansHead"
# full_dat$Transect[full_dat$`LATITUDE (degrees_north)`  == "-28.6"] <- "CapeByron"
# 
# full_dat$Transect <- factor(full_dat$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))
# 
# table(full_dat$Transect)
# 
# write.csv(full_dat, "Data/Velocity Timeseries/All sites velocity timeseries.csv", row.names = F)

vel_dat <- read_csv("../Data/All sites velocity timeseries.csv")
vel_dat$Date <- vel_dat$`TIME (UTC)`


#### Rotate for coastline

vel_dat$U_shore = 0
vel_dat$V_shore = 0
for (i in 1:nrow(vel_dat)){
  if (vel_dat$Transect[i] == "CapeByron") {
    rot_deg_angle= -356
    vel_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
    vel_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
  }
  if (vel_dat$Transect[i] == "DiamondHead") {
    rot_deg_angle= -19
    vel_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
    vel_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
  }
  if (vel_dat$Transect[i] == "EvansHead") {
    rot_deg_angle= -13
    vel_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
    vel_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
  }
  if (vel_dat$Transect[i] == "NorthSolitary") {
    rot_deg_angle= -15
    vel_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
    vel_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*vel_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*vel_dat$`VCUR (m/s)`[i]
  }
}


str(vel_dat)
vel_dat$Month <- month(vel_dat$Date)


plot_dat <- vel_dat %>% group_by(Month, Transect) %>% 
  summarise(Along_Velocity = mean(V_shore)*-1, n = n(), SD = sd(V_shore), SE = SD/sqrt(n))
plot_dat

p_season <- ggplot(plot_dat, aes(x = Month, y = Along_Velocity, lty = Transect)) +  geom_point() +
  geom_line(size = 1.5) + theme_classic() + ylab("Alongshore Velocity (m/s ± SE)") +
  geom_errorbar(aes(ymin=Along_Velocity-SE, ymax=Along_Velocity+SE), width=.2, show.legend = FALSE)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        legend.key.width = unit(3, "line"),
        legend.position=c(.45, .9),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=10)) +
  scale_x_continuous(limits=c(0.8, 12.1), breaks = c(1:12)) +
  scale_linetype_discrete(name  ="Transect",
                          breaks=c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"),
                          labels=c("Cape Byron (28.6 °S)", "Evans Head (29 °S)", "North Solitary (30 °S)", "Diamond Head (31.8 °S)"))
p_season

ggsave("../plots/velocity seasonality.png", width = 21, height = 14.8, units = "cm", dpi = 600)
ggsave("../plots/velocity seasonality.pdf", width = 21, height = 14.8, units = "cm", dpi = 600)
