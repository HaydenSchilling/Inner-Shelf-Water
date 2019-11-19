# Nitrate plots

library(ggplot2)

mydata <- read.csv("Data/HYD cleaned 080419.csv", header = T)

head(mydata)

p1 <- ggplot(mydata, aes(x = long3, y = -Depth, col = Nitrate, size = Nitrate)) + geom_point() +
  facet_wrap(~OPC_site, scales = "free_x") + theme_bw()
p1

p2 <- ggplot(mydata, aes(x = long3, y = -Depth, col = Phosphate, size = Phosphate)) + geom_point() +
  facet_wrap(~OPC_site, scales = "free_x") + theme_bw()
p2

cor.test(mydata$Nitrate, mydata$Phosphate)

mydata2 <- read.csv("Data/CleanedData_220519.csv", header = T)

head(mydata2)
mydata2 <- subset(mydata2, site2 != "CB")

p3 <- ggplot(mydata2, aes(x = Lon, y = -Depth, col = chla)) + geom_point() +
  facet_wrap(~site2, scales = "free_x") + theme_bw() + scale_color_gradient(low = "white", high = "darkgreen")
p3
