# Zooscan detritus plots

library(tidyverse)

mydat <- read_csv("Data/zooscan_samples.csv", col_types = "ccncnc") 

mydat_wide <- mydat %>% pivot_wider(names_from = dimension, values_from = measure_mm)

# make ESD using Schilling et al equation
mydat_wide <- mydat_wide %>% mutate(ESD = 2000*(((length*width*width)/8)^(1/3)), 
                                    ESD_round=round(ESD, -2),
                                    detritus_category = case_when(taxa == "Other" ~ "Possible detritus",
                                                                  T ~ "Zooplankton"),
                                    Sample = case_when(Sample == "333_0.5B" ~ "f) Diamond Head Offshore",
                                                       Sample == "338_0.5" ~ "e) Diamond Head Inshore",
                                                      Sample == "340_0.5" ~ "d) North Solitary Offshore",
                                                      Sample == "340_0.25" ~ "d) North Solitary Offshore",
                                                      Sample == "346_0.25" ~ "c) North Solitary Inshore",
                                                      Sample == "433_0.25" ~ "b) Evans Head Offshore",
                                                      Sample == "437_0.25" ~ "a) Evans Head Inshore"),
                                    taxa = case_when(taxa== "other" ~ "Other",
                                                     T ~ taxa))

table(mydat_wide$ESD_round)
table(mydat_wide$detritus_category)



dat_sum <- mydat_wide %>% group_by(ESD_round,detritus_category) %>% summarise(n=n()) %>%
  pivot_wider(names_from = detritus_category, values_from = n, values_fill = 0) %>% 
  mutate(Percent_detritus = (`Possible detritus`/(`Possible detritus`+`Zooplankton`))*100)
#dat_sum$Percent_detritus[1] <- 100


ggplot(mydat_wide, aes(ESD, fill=detritus_category)) + geom_histogram(binwidth=100, position="stack")
ggplot(mydat_wide, aes(ESD, fill=detritus_category)) + geom_histogram(binwidth=100, position="stack") + 
  facet_wrap(~Sample, scales="free_y", ncol = 2) + xlab("ESD (µm)") +
  #geom_vline(xintercept = 300) +
  theme_classic() + theme(legend.position = "bottom",
                          axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(face="bold", size=14),
                          panel.background = element_rect(fill=NA, colour="black"),
                          strip.text = element_text(size=12, face="bold"),
                          legend.title = element_text(face="bold", size=14),
                          legend.text = element_text(size=12))+
  scale_fill_discrete(name="Particle Type")+
  xlim(c(250,1550))
ggsave("detritus plots.png", dpi =600, units="cm", width=21, height=26)

ggplot(mydat_wide, aes(ESD, fill=taxa)) + geom_histogram(binwidth=100, position="stack") + 
  facet_wrap(~Sample, scales="free_y", ncol = 2) + xlab("ESD (µm)") +
  #geom_vline(xintercept = 300) +
  theme_classic() + theme(legend.position = "bottom",
                          axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(face="bold", size=14),
                          panel.background = element_rect(fill=NA, colour="black"),
                          strip.text = element_text(size=12, face="bold"),
                          legend.title = element_text(face="bold", size=14),
                          legend.text = element_text(size=12))+
  xlim(c(250,1550)) + scale_fill_viridis_d(option="magma", name="Taxa")
ggsave("detritus plots2.png", dpi =600, units="cm", width=21, height=26)


summary(mydat_wide$ESD)

ggplot(dat_sum, aes(ESD_round, Percent_detritus)) + geom_line()+ geom_vline(xintercept = 300, col="red")


dat_sum2 <- mydat_wide %>% group_by(Sample, ESD_round,detritus_category) %>% summarise(n=n()) %>%
  pivot_wider(names_from = detritus_category, values_from = n, values_fill = 0) %>% 
  mutate(Percent_detritus = (`possible detritus`/(`possible detritus`+`Not detritus`))*100)

ggplot(dat_sum2, aes(ESD_round, Percent_detritus, col=Sample)) + geom_line()+ geom_vline(xintercept = 300, col="red") +
  xlim(c(0,1000))


Overall_summary <- dat_sum2 %>% group_by(ESD_round) %>% summarise(mean_detritus = mean(Percent_detritus), 
                                                            SD_detritus= sd(Percent_detritus), 
                                                            n=n(),
                                                            SE_detritus =SD_detritus/sqrt(n))
ggplot(Overall_summary, aes(ESD_round, mean_detritus)) + geom_line()+ 
  geom_vline(xintercept = 300, col="red") + 
  geom_errorbar(aes(ymin=mean_detritus-SE_detritus, ymax =mean_detritus+SE_detritus ))+
  ylab("Possible Detritus (% + SE)") + xlab("ESD (um)")
