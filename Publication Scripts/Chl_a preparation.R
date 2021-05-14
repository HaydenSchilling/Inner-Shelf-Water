# chl_a data prep

library(tidyverse)

mydata <- read_csv("Other prepublication stuff/Other Data/MNF_SS200408_ctd_trawler/MNF_SS200408_ctd_trawler.csv")

Locations <- mydata %>% distinct(START_LAT, START_LON, .keep_all = TRUE)

ggplot(Locations, aes(x= START_LON, y= START_LAT)) + geom_point() +
  coord_quickmap()


library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(country = 'australia',scale = "large", returnclass = "sf")
class(world)


ggplot() + geom_point(data=Locations, aes(x= START_LON, y= START_LAT)) +
  geom_sf(data=world, col="grey70", fill = "grey80")+
  coord_sf(xlim = c(152, 154.5), ylim = c(-32.4, -28.7), expand = FALSE) 


Locations <- Locations %>% mutate(Site = case_when((START_LAT < -31) ~ "Diamond Head",
                                                   (START_LAT > -29.2) ~ "Evans Head",
                                                   (START_LAT > -30.5 & START_LAT < -29.9) ~ "North Solitary",
                                                   TRUE ~ "Other"))

ggplot() + geom_point(data=Locations, aes(x= START_LON, y= START_LAT, col=Site)) +
  geom_sf(data=world, col="grey70", fill = "grey80")+
  coord_sf(xlim = c(152, 154.5), ylim = c(-32.4, -28.7), expand = FALSE) 


mydata <- mydata %>% mutate(Site = case_when((START_LAT < -31) ~ "Diamond Head",
                                                   (START_LAT > -29.2) ~ "Evans Head",
                                                   (START_LAT > -30.5 & START_LAT < -29.9) ~ "North Solitary",
                                                   TRUE ~ "Other"))

mydata <- mydata %>% filter(Site != "Other")

mydata <- mydata %>% mutate(Chl_a = 0.0157*FLUORESCENCE + 0.4421)
hist(mydata$Chl_a)

write_csv(mydata, "Data/CHL_a_Cleaned.csv")
