# Zooplankton composition off northern NSW Monthly
# Using Data from 14th May 2021

library(tidyverse)

#Load data
#mydata2 <- read_csv("Other prepublication stuff/Other Data/CPR/IMOS_-_AusCPR#_Zooplankton_Abundance-Higher_taxonomic_groups_-_sample,_update_on_request.csv")

mydata <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/CPR_Zoop_HTGMat.csv")

# make long data
mydata <- mydata %>% filter(Latitude < -28 & Latitude > -32 & Longitude >152 & Longitude < 155) %>%
  pivot_longer(names_to = "Taxa", values_to = "Abundance_m3", cols=Amphipod:Urochordata)

# get total abundance/m3
totals <- mydata %>% group_by(Latitude, Longitude, SampleDateUTC) %>% summarise(Total_abund = sum(Abundance_m3))
head(totals)

# join abundance and totals and calculate % composition.
mydata <- left_join(mydata, totals)
mydata <- mydata %>% mutate(Percent_composition = Abundance_m3/Total_abund*100)

# get the months labelled right
dat <- mydata %>% group_by(Taxa, Month) %>% summarise(Mean_composition = mean(Percent_composition, na.rm = T), sd_composition = sd(Percent_composition, na.rm = T), n= n(), SE_composition=(sd_composition/sqrt(n)), CV=sd_composition/Mean_composition) %>%
  mutate(Month = as.factor(month.name[Month]))
dat$Month <- forcats::fct_relevel(dat$Month, "January","February","March","April","May","June","July","August","September","October","November","December")

# plot
ggplot(dat, aes(Taxa, Mean_composition, fill= Taxa)) + facet_wrap(~Month) + geom_bar(stat="identity")
dat

# restrict to taxa only >3% of the composition.
dat_5 <- dat %>% filter(Mean_composition >3)
ggplot(dat_5, aes(Taxa, Mean_composition, fill= Taxa)) + facet_wrap(~Month) + geom_bar(stat="identity") +
  scale_y_continuous(breaks = seq(0,100,20))+
  geom_errorbar(aes(ymax=Mean_composition+SE_composition, ymin=Mean_composition-SE_composition)) +
  theme_classic() + ylab("Composition (% ± SE)") + theme(axis.text.x = element_blank(),
                                                         axis.text.y = element_text(colour="black", size=10),
                                                         axis.title = element_text(face="bold",size=12),
                                                         strip.text = element_text(colour="black", size=10, face="bold"),
                                                         legend.title = element_text(face="bold",size=12))+
  scale_fill_viridis_d(option="viridis")

ggsave("Other prepublication stuff/plots/CPR monthly composition 3percent.png", dpi=600, width=21, height=18, units="cm")
ggsave("Other prepublication stuff/plots/CPR monthly composition 3percent.pdf", dpi=600, width=21, height=18, units="cm")
range(mydata$Year)


