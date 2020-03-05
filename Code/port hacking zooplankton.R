# Port Hacking zooplankton

library(tidyverse)
library(lubridate)

mydata <- read_csv("Data/NRS Zooplankton PH.csv")

str(mydata)

p1 <- ggplot(mydata, aes(x = Month, y = log10(ZoopAbundance_m3))) + geom_point() +
  geom_smooth() + scale_x_continuous(breaks = seq(1,12)) + theme_classic()
p1

ggsave("plots/Port Hacking Seasonality.png", height = 14.8, width = 21, units = "cm", dpi = 600)
