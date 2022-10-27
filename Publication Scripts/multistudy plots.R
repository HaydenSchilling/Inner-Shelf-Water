# Multi-study plots

library(tidyverse)
library(ggrepel)

mydata <- read_csv("Data/Mulistudy plot data.csv")
head(mydata)
mydata$extra <- 1

# The palette:
cbPalette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')
cbPalette2 <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')


set.seed(1)

p1 <- ggplot(mydata, aes(y = Ratio, x = Bathy, label = `Reference Number`)) + geom_point(size = 2) +
  facet_wrap(~Parameter, scales = "free_y", ncol = 1) + theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("Inshore:Offshore Ratio") + geom_errorbarh(aes(xmin=Inhore_Bath, xmax=Offshore_Bathy), col = "grey60")+
  scale_x_log10()+ xlab("Bathymetry (m)") + geom_text_repel(col="blue")+ scale_y_log10()+
  #scale_color_manual(values=cbPalette) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(colour = "black", size = 10),
        strip.text = element_text(face = "bold", size = 11),
        legend.title = element_text(face = "bold", size = 11),
        panel.background = element_rect(fill = NA, color = "black"))
p1

#ggsave("../Plots/Multistudy plot.png", dpi = 600, height = 12, width = 12, units = "cm")


# World map

library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "small", returnclass = "sf")
class(world)

# 1 row per study
mydata2 <- mydata %>% distinct(`Reference Number`, .keep_all = TRUE)

P_map <- ggplot() +
  geom_sf(data = world, col="grey70", fill = "grey70") +
  geom_point(data = mydata2, aes(x = Longitude, y = Latitude), size = 1.5, 
             shape = 21, fill = "blue") +
  geom_text_repel(data = mydata2, aes(x = Longitude, y = Latitude, label = `Reference Number`),col="black", size =3.5, fontface = "bold",
                  max.overlaps = 30) +
  theme_bw() +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"),
        axis.ticks = element_line(colour="black"))
  

P_map

#ggsave("world map test.png", dpi = 600, units = "cm", height = 12.8, width = 21)


# Abundance only plot

mydata_a <- mydata %>% filter(Parameter == "Abundance")

pA <- ggplot(mydata_a, aes(y = Ratio, x = Bathy, label = `Reference Number`)) + geom_point(size = 2) +
  theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("Inshore:Offshore Ratio") + geom_errorbarh(aes(xmin=Inhore_Bath, xmax=Offshore_Bathy), col = "grey60",height=0)+
  scale_x_log10()+ xlab("Bathymetry (m)") + geom_text_repel(col="blue")+
  scale_y_log10(breaks = c(1,3,10,20, 25))+
  #scale_color_manual(values=cbPalette) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(colour = "black", size = 10)) +
  ggtitle('A) Abundance')
pA

# Biomass only plot

mydata_b <- mydata %>% filter(Parameter == "Biomass")

pB <- ggplot(mydata_b, aes(y = Ratio, x = Bathy, label = `Reference Number`)) + geom_point(size = 2) +
  theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("") + geom_errorbarh(aes(xmin=Inhore_Bath, xmax=Offshore_Bathy), col = "grey60",height=0)+
  scale_x_log10()+ xlab("Bathymetry (m)") + geom_text_repel(col="blue")+
  scale_y_log10()+
  #scale_color_manual(values=cbPalette) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(colour = "black", size = 10))+
  ggtitle('B) Biomass')
pB

# NBSS Slope only plot

mydata_s <- mydata %>% filter(Parameter == "NBSS Slope")

pC <- ggplot(mydata_s, aes(y = Ratio, x = Bathy, label = `Reference Number`)) + geom_point(size = 2) +
  theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("") +
  geom_errorbarh(aes(xmin=Inhore_Bath, xmax=Offshore_Bathy), col = "grey60",height=0)+
  scale_x_log10()+ xlab("Bathymetry (m)") + geom_text_repel(col="blue")+
  scale_y_log10(breaks=c(0.5,0.75,1,1.25,1.5,1.75))+
  #scale_color_manual(values=cbPalette) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(color = "black", size=10))+
  ggtitle('C) Size Spectra Slope')
pC


library(patchwork)
p_final <- P_map + pA + pB + pC + plot_layout(ncol = 1, guides = 'collect')
p_final

#ggsave("multiplot test.png", dpi = 600, units = "in", height = 10, width = 3.5)

p_final2 <-  pA + pB + pC + P_map + plot_layout(ncol = 2, widths = c(1, 1))
p_final2
#ggsave("multiplot test square.png", dpi = 600, units = "cm", height = 14.8, width = 21)


## Attempt to plot a table in ggplot
#library(gridExtra)

mydata3 <- mydata2 %>% select(`Reference Number`, Region, Study)
mydata3 <- mydata3 %>% distinct(`Reference Number`, .keep_all = TRUE)
mydata3 <- mydata3 %>% rename("Ref. #" = `Reference Number`)
mydata3

tt2 <- gridExtra::ttheme_minimal(base_size = 9, padding = unit(c(1.5,1.5),"mm"))


t_test <- (pA + pB + pC) / (P_map + gridExtra::tableGrob(mydata3, theme = tt2, rows = NULL)) + plot_layout(heights = c(0.69,1))
t_test
#ggsave("Other prepublication stuff/plots/multiplot final22.png", dpi = 600, units = "cm", height = 15.5, width = 21)
#ggsave("Other prepublication stuff/plots/multiplot final.pdf", dpi = 600, units = "cm", height = 14.8, width = 21)
### Note two author names edited later to get correct symbol.

#### Now to check m2 results ####
# Abundance only plot

mydata_a <- mydata %>% filter(Parameter == "Abundance")

m2A <- ggplot(mydata_a, aes(y = Ratio_m2, x = Bathy, label = `Reference Number`)) + geom_point(size = 2) +
  theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("Inshore:Offshore Ratio") + geom_errorbarh(aes(xmin=Inhore_Bath, xmax=Offshore_Bathy), col = "grey60",height=0)+
  scale_x_log10()+ xlab("Bathymetry (m)") + geom_text_repel(col="blue")+
  scale_y_log10(breaks = c(1,3,10,20, 25))+
  #scale_color_manual(values=cbPalette) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(colour = "black", size = 10)) +
  ggtitle('A) Abundance per m2')
m2A

# Biomass only plot

mydata_b <- mydata %>% filter(Parameter == "Biomass")

m2B <- ggplot(mydata_b, aes(y = Ratio_m2, x = Bathy, label = `Reference Number`)) + geom_point(size = 2) +
  theme_classic() + geom_hline(yintercept = 1, col= "red", lty=2) +
  ylab("") + geom_errorbarh(aes(xmin=Inhore_Bath, xmax=Offshore_Bathy), col = "grey60",height=0)+
  scale_x_log10()+ xlab("Bathymetry (m)") + geom_text_repel(col="blue")+
  scale_y_log10()+
  #scale_color_manual(values=cbPalette) +
  theme(axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(colour = "black", size = 10))+
  ggtitle('B) Biomass per m2')
m2B

m2A + m2B
#ggsave("Other prepublication stuff/plots/ratio m2 plot.png", dpi = 600, width=21, height=14.8, units="cm")
#ggsave("Other prepublication stuff/plots/ratio m2 plot.pdf", dpi = 600, width=21, height=14.8, units="cm")
