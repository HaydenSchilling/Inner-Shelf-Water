## Try some section plots

dat <- read.csv("data/MNF_SS200408_ctd_trawler/MNF_SS200408_ctd_trawler.csv", header = T)

head(dat)
dat1 <- subset(dat, START_LAT > -29.3)

table(dat1$BOTTOM_LAT)

str(dat1)

plot(dat1$START_LON, -dat1$PRESSURE, col = dat1$TEMPERATURE)


library(oce)

# Make into a "section" for the oce package to work
sect1 <- as.section(salinity = dat1$SALINITY, temperature = dat1$TEMPERATURE, pressure = dat1$PRESSURE, 
                    longitude = dat1$START_LON, latitude = dat1$START_LAT, station = dat1$STATION, sectionId = "Test Section")

plot(sect1, ylim = c(0,200))

plot(sect1, which='temperature', xtype='distance',
     ztype='image')

plot(sect1, which='salinity', xtype='distance',
     ztype='image')

## Extract data as matrix and plot separately
s <- sectionGrid(sect1, p='levitus')

nstation <- length(s[['station']])
p <- unique(s[['pressure']])
np <- length(p)
T <- S <- array(NA, dim=c(nstation, np))
for (i in 1:nstation) {
  T[i, ] <- s[['station']][[i]][['temperature']]
  S[i, ] <- s[['station']][[i]][['salinity']]
}

distance <- unique(s[['distance']])
par(mfrow=c(2, 1))
imagep(distance, p, T, col=oceColorsTemperature, flipy=TRUE)
imagep(distance, p, S, col=oceColorsSalinity, flipy=TRUE)


par(mfrow=c(2, 1))
Tcm <- colormap(T, breaks=seq(9, 23, 0.1), col=oceColorsTemperature)
Scm <- colormap(S, breaks=seq(35, 36, 0.02), col=oceColorsSalinity)
imagep(distance, p, T, colormap=Tcm, flipy=TRUE,
       ylab='p [dbar]', filledContour=TRUE,
       zlab='temperature [degC]')
imagep(distance, p, S, colormap=Scm, flipy=TRUE,
       xlab='distance [km]', ylab='p [dbar]', filledContour=TRUE,
       zlab='salinity')


library(ggplot2)
p1 <- ggplot(dat1, aes(x = START_LON, y = -PRESSURE, col = TEMPERATURE, z = TEMPERATURE)) + geom_point() +
  geom_line(aes(y = -BOTTOM_DEPTH), col = "grey60")+
  theme_classic() + scale_color_continuous(low = "blue", high = "red")
p1

nuts <- read.csv("data/HYD cleaned 080419.csv")
head(nuts)
plot(nuts$lat3, nuts$long3)

nuts1 <- subset(nuts, lat3 > -29.5)
plot(nuts1$Depth, nuts1$long3)


p3 <- ggplot(nuts1, aes(x = long3, y = -Depth, col = (Nitrate), z = Nitrate, size = Nitrate)) + geom_point() +
  #geom_line(aes(y = -BOTTOM_DEPTH), col = "grey60")+
  theme_classic() + scale_color_continuous(low = "blue", high = "red")
p3


nuts2 <- subset(nuts, lat3 < -31)
plot(nuts1$Depth, nuts1$long3)


p2 <- ggplot(nuts2, aes(x = long3, y = -Depth, col = (Nitrate), z = Nitrate, size = Nitrate)) + geom_point() +
  #geom_line(aes(y = -BOTTOM_DEPTH), col = "grey60")+
  theme_classic() + scale_color_continuous(low = "blue", high = "red")
p2
