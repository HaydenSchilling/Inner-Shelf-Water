# 1994 Data from Franklin via Iain

library(tidyverse)

mydata <- read_csv("1994 stuff/1994_data_plotting.csv")

p94 <- ggplot(mydata, aes(x = Distance_coast, y = -Depth, size = log10(Biomass_mg_m3))) + geom_point() + 
  facet_wrap(~Date, ncol=1) + theme_classic() +
  geom_ribbon(data = mydata, aes(x = Distance_coast, ymax = -Bathymetry, ymin=-200), inherit.aes = FALSE, fill = "grey60") +
  coord_cartesian(ylim = c(-200,0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
p94

ggsave("plots/zoop/1994 plots.png", width = 21, height = 29.7, units = "cm", dpi = 600)

## Biomass with Distance Plots
head(mydata)

pD <- ggplot(mydata, aes(x = Distance_coast, y = log10(Biomass_mg_m3))) + geom_point(alpha = 0.5) + facet_wrap(~Date) +
  theme_classic() + geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pD

# TS Plots
library(sommer) # for jet colour scheme

pTS <- ggplot(mydata, aes(x = Sal., y = Temp, col = log10(Biomass_mg_m3))) + geom_point() +# facet_wrap(~Date) +
  theme_classic() + scale_color_gradientn(colours = jet.colors(100)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 16), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 16))
pTS
