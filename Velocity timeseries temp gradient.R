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
full_dat$V_Current <- full_dat$`VCUR (m/s)`

p1 <- ggplot(full_dat, aes(x = -V_Current, y = Offshore_Inshore)) + geom_point(alpha = 0.4) + 
  facet_wrap(~Transect) + theme_bw() + geom_smooth(method = "lm") +
  xlab("Southward Current (m/s)") + ylab("Offshore - Inshore Temp (deg C)")
p1

ggsave("plots/Southward Current Temp Gradient Relationship.png", 
       height = 14.8, width = 21, dpi = 600, units = "cm")

cor.test(full_dat$V_Current, full_dat$Offshore_Inshore)
fit1 <- lm(Offshore_Inshore ~ V_Current, data = full_dat)
summary(fit1)

head(full_dat)
