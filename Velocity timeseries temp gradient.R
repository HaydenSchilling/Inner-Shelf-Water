# Velocity timeseries and comparison to Temp gradient

library(tidyverse)

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

vel_dat <- read_csv("Data/Velocity Timeseries/All sites velocity timeseries.csv")
vel_dat$Date <- vel_dat$`TIME (UTC)`

temp_dat <- read_csv("Data/Temp gradient all sites.csv")
temp_dat <- filter(temp_dat, Offshore_Inshore < 8)

full_dat <- left_join(vel_dat, temp_dat, by =c("Date", "Transect"))
head(full_dat)

full_dat <- full_dat %>% drop_na(Offshore_Inshore)

full_dat$Transect <- factor(full_dat$Transect, levels = c("CapeByron", "EvansHead", "NorthSolitary", "DiamondHead"))

hist(full_dat$`VCUR (m/s)`)

str(full_dat)

#### Rotate for coastline

full_dat$U_shore = 0
full_dat$V_shore = 0
for (i in 1:nrow(full_dat)){
  if (full_dat$Transect[i] == "CapeByron") {
    rot_deg_angle= -356
    full_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
    full_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
  }
  if (full_dat$Transect[i] == "DiamondHead") {
    rot_deg_angle= -19
    full_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
    full_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
  }
  if (full_dat$Transect[i] == "EvansHead") {
    rot_deg_angle= -13
    full_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
    full_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
  }
  if (full_dat$Transect[i] == "NorthSolitary") {
    rot_deg_angle= -15
    full_dat$U_shore[i] = cos(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + sin(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
    full_dat$V_shore[i] = sin(rot_deg_angle*pi/180)*full_dat$`UCUR (m/s)`[i] + cos(rot_deg_angle*pi/180)*full_dat$`VCUR (m/s)`[i]
  }
}




p1 <- ggplot(full_dat, aes(x = -V_shore, y = Offshore_Inshore)) + geom_point(alpha = 0.4) + 
  facet_wrap(~Transect) + theme_bw() + geom_smooth(method = "lm") +
  xlab("Alongshore Current (m/s)") + ylab("Offshore - Inshore Temp (deg C)") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14))
p1

ggsave("plots/Alongshore Current Temp Gradient Relationship_15km.png", 
       height = 14.8, width = 21, dpi = 600, units = "cm")
ggsave("plots/Alongshore Current Temp Gradient Relationship_15km.pdf", 
       height = 14.8, width = 21, dpi = 600, units = "cm")

cor.test(full_dat$V_Current, full_dat$Offshore_Inshore)
fit1 <- lm(Offshore_Inshore ~ V_Current, data = full_dat)
summary(fit1)

head(full_dat)
