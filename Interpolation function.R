# Interpolating plots

#install.packages("akima")

library(akima)
library(ggplot2)
library(reshape2)

mydata <- read.csv("Data/HYD cleaned 080419.csv")
str(mydata)
head(mydata)
# variables to loop through
vars = c("CTD.Sal", "CTD.Temp", "Oxygen", "Silicate","Nitrate")
#sites to loop through
sites = c("DH", "NS", "EH")

# # Testing
# for (i in vars){
# 
#   p1 <- ggplot(mydata, aes(x = long3, y = -Depth, col = get(i))) + geom_point() +
#     facet_wrap(~OPC_site, scales = "free_x") + theme_bw()
#   print(p1)
# }
# mydata2 <- subset(mydata, OPC_site == "EH")
# str(mydata2)


for (j in sites){
  mydata2 <- subset(mydata, OPC_site == j)
  for (i in vars){
    #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
    fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2[[i]])
    pdf(paste0('plots/CTD/',j,"_",i,'.pdf'), width=10, height=5)
    print(filled.contour(fit1, zlim = c(min(mydata[[i]]), max(mydata[[i]])),
                         plot.title = title(main = c(j, i))))
    dev.off()
    png(paste0('plots/CTD/',j,"_",i,'.png'), width=6000, height=3000, res = 600)
    print(filled.contour(fit1, zlim = c(min(mydata[[i]]), max(mydata[[i]])),
                         plot.title = title(main = c(j, i))))
    dev.off()
    pdf(paste0('plots/CTD/',j,"_",i,'_lines.pdf'), width=10, height=5)
    print(contour(fit1, plot.title = title(main = c(j, i))))
    dev.off()
    png(paste0('plots/CTD/',j,"_",i,'_lines.png'), width=6000, height=3000, res = 600)
    print(contour(fit1, plot.title = title(main = c(j, i))))
    dev.off()
  }
}
str(fit1)


print(filled.contour(fit1, plot.title = title(main = i)))


## Now for zooplankton

mydata <- read.csv("Data/OPC_CTD 120419.csv", header = T)

str(mydata)
table(mydata$site)
library(akima)
library(ggplot2)

vars = c("NBSSSlope", "NBSSIntercept", "NBSSRsq", "Biomass","GeoMn")
sites = c("DiamondHead", "CapeByron", "NorthSolitary", "EvansHead")

#get(vars)

for (i in vars){
  
  p1 <- ggplot(mydata, aes(x = Lon, y = -Depth, col = get(i))) + geom_point() +
    facet_wrap(~site, scales = "free_x") + theme_bw()
  print(p1)
}
mydata2 <- subset(mydata, OPC_site == "EH")
str(mydata2)

library(tidyr)

for (j in sites){
  mydata2 <- subset(mydata, site == j)
  for (i in vars){
    mydata2 <- mydata2 %>% drop_na(i) # removes NA in the variable we are interested in
    #fit1 <- interp(x = mydata2$long3, y = -mydata2$Depth, z = mydata2$CTD.Sal)
    fit1 <- interp(x = mydata2$Lon, y = -mydata2$Depth, z = mydata2[[i]])
    pdf(paste0('plots/zoop/',j,"_",i,'.pdf'), width=10, height=5)
    print(filled.contour(fit1, plot.title = title(main = c(j, i))))
    dev.off()
    pdf(paste0('plots/zoop/',j,"_",i,'_lines.pdf'), width=10, height=5)
    print(contour(fit1, plot.title = title(main = c(j, i))))
    dev.off()
  }
}

### Test a single OPC variable (biomass but logged)

fit1 <- interp(x = mydata2$Lon, y = -mydata2$Depth, z = log10(mydata2$Biomass))
pdf(paste0('plots/zoop/',j,"_",i,'.pdf'), width=10, height=5)
print(filled.contour(fit1, plot.title = title(main = c(j, i))))
dev.off()


# ### TO do in ggplot
# 
# 
# d2 <- melt(fit1$z, na.rm = TRUE, value.name = "Nitrate")
# names(d2) <- c("Longitude", "Depth", "Nitrate")
# 
# p2 <- ggplot(d2, aes(x = Longitude, y = Depth, z=Nitrate, fill = Nitrate)) + 
#   geom_tile() + stat_contour(colour = "white")
# p2

# 
# ## Maybe try:
install.packages("ContourFunctions")
# 
library(ContourFunctions)

cf_data(mydata2$Lon, -mydata2$Depth, mydata2$NBSSSlope)

