# Multi-study plots

library(tidyverse)

mydata <- read_csv("Data/Mulistudy plot data.csv")
head(mydata)
mydata$extra <- 1

# The palette:
cbPalette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')
cbPalette2 <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')


set.seed(1)

p1 <- ggplot(mydata, aes(y = Ratio, x = extra, col = Region)) + geom_jitter(size = 3, height = 0, width = 0.1) +
  facet_wrap(~Parameter, scales = "free_y", ncol = 1) + theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("Inshore:Offshore Ratio") + scale_x_continuous(limits = c(0.9,1.1))+
  scale_color_manual(values=cbPalette) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.y = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 11),
        legend.title = element_text(face = "bold", size = 11),
        panel.background = element_rect(fill = NA, color = "black"))
p1

ggsave("Plots/Multistudy plot.png", dpi = 600, height = 9, width = 12, units = "cm")
