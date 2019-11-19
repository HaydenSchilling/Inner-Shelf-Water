### ADCP plots

install.packages("akima")

library(akima)

mydata <- read.csv("Data/ADP_tows_final_300419.csv")
str(mydata)
head(mydata)

vars = c("U", "V")
sites = c("DH", "NS", "EH", "CB")

#get(vars)

for (i in vars){
  
  p1 <- ggplot(mydata, aes(x = Lon, y = -Depth, col = get(i))) + geom_point() +
    facet_wrap(~OPC_site, scales = "free_x") + theme_bw()
  print(p1)
}
mydata2 <- subset(mydata, OPC_site == "EH")
str(mydata2)


for (j in sites){
  mydata2 <- subset(mydata, OPC_site == j)
  for (i in vars){
    #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
    fit1 <- interp(x = mydata2$Lon, y = -mydata2$Depth, z = mydata2[[i]])
    pdf(paste0('plots/ADCP/ADCP_',j,"_",i,'.pdf'), width=10, height=5)
    print(filled.contour(fit1, color.palette = function(n) hcl.colors(n, "RdBu", rev = TRUE),
                         zlim = c(min(mydata[[i]]), max(mydata[[i]])), plot.title = title(main = c(j, i))))
    dev.off()
    png(paste0('plots/ADCP/ADCP_',j,"_",i,'.png'), width=6000, height=3000, res = 600)
    print(filled.contour(fit1, color.palette = function(n) hcl.colors(n, "RdBu", rev = TRUE), 
                         zlim = c(min(mydata[[i]]), max(mydata[[i]])), plot.title = title(main = c(j, i))))
    dev.off()
    pdf(paste0('plots/ADCP/ADCP_',j,"_",i,'_lines.pdf'), width=10, height=5)
    print(contour(fit1, plot.title = title(main = c(j, i))))
    dev.off()
    png(paste0('plots/ADCP/ADCP_',j,"_",i,'_lines.png'), width=6000, height=3000, res = 600)
    print(contour(fit1, plot.title = title(main = c(j, i))))
    dev.off()
  }
}

filled.contour(fit1, color.palette = function(n) hcl.colors(n, "RdBu", rev = TRUE))


#### Quiver plots
library(dplyr)
#install.packages("ggquiver")
library(ggquiver)

top_dat <- mydata %>% group_by(Lat, Lon) %>% filter(Depth == min(Depth))
head(top_dat)

library(ggplot2)

#Load map data
library(rgdal)
library(raster)
Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
#plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea" )

head(Aus_coast)

#plot(Aus_coast)

min_lon <- 150
max_lon <- 155
min_lat <- -32
max_lat <- -28

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)

Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run

shelf <- read.csv("Hayden_200m_contour.csv", header = T)
shelf <- subset(shelf, Var2 >= -32)
shelf <- subset(shelf, Var2 <= -28)
shelf <- subset(shelf, Var1 > 150)



pQ <- ggplot(top_dat, aes(x = Lon, y = Lat)) + geom_point() +
  coord_quickmap() + #coord_map()
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), 
               fill = "gray60", colour = "gray60")+ labs(x = "Longitude", y = "Latitude")+
  geom_quiver(aes(u = U, v = V), center = T, vecsize = 1000) + theme_classic()+
  geom_path(data=shelf, aes(x=Var1, y = Var2)) + 
  scale_x_continuous(expand = c(0,0.1))+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position=c(0.92,0.05), legend.direction = "horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())
pQ

ggsave("plots/ADCP/Surface Velocity Map.pdf", height = 15, width = 12, units = "cm")
ggsave("plots/ADCP/Surface Velocity Map.png", height = 15, width = 12, units = "cm", dpi = 600)
