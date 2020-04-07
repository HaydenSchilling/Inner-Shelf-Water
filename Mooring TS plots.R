# Mooring Data

library(tidyverse)
library(lubridate)

#mydata <- read_csv("Data/Mooring Data East Aus.csv")

#mydata2 <- mydata %>% select(TIME, LATITUDE, LONGITUDE, DEPTH, TEMP, PRES, PSAL, site_code)

#write.csv(mydata2, "Data/Mooring data small.csv", row.names = FALSE)

mydata2 <- read_csv("Data/Mooring data small.csv")

table(mydata2$site_code)

dat <- mydata2 %>% filter(site_code != "SEQ200" & site_code != "SEQ400" & DEPTH <= 100)

dat$Date <- date(dat$TIME)
dat$Month <- month(dat$Date)
dat$Depth_m <- round(dat$DEPTH)

dat2 <- dat %>% filter(Month ==9 & PSAL > 30)

dat3 <- dat2 %>% group_by(site_code, Date, Depth_m) %>% summarise(Temp = mean(TEMP), Salt = mean(PSAL))

pTS <- ggplot(dat3, aes(x = Salt, y = Temp)) + geom_point(alpha = 0.2) + theme_classic() +  facet_wrap(~site_code) 
pTS


# OR

pTS <- ggplot(dat2, aes(x = PSAL, y = TEMP)) + geom_point(alpha = 0.2) + theme_classic() + facet_wrap(~site_code) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pTS

ggsave("plots/Mooring TS Plots for September.png", width = 21, height = 14.8, units = "cm", dpi = 600)
