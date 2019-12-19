# getting Bathymetry into Zooplankton file

library(akima)
library(ggplot2)
library(reshape2)
library(tidyverse)
#install.packages("ncdf4")
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis

mydata <- read_csv("Data/SS2004_SeaSoarData.csv")
str(mydata)
head(mydata)

mydata <- mydata %>% 
  mutate(site = case_when(site = str_detect(File,"SS0408_023") ~ "CapeByron",
                          site = str_detect(File,"SS0408_021") ~ "EvansHead",
                          site = str_detect(File,"SS0408_010") ~ "NorthSolitary",
                          site = str_detect(File,"SS0408_008") ~ "DiamondHead"),
         site = as.factor(site))
### Get distance from shore


### Get distance from shore
library(geosphere)

mydata$Distance_Coast = 0
for (i in 1:nrow(mydata)){
  if (mydata$site[i] == "CapeByron") {
    mydata$Distance_Coast[i] = distm(c(153.58, -28.6), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "DiamondHead") {
    mydata$Distance_Coast[i] = distm(c(152.75, -31.8), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "EvansHead") {
    mydata$Distance_Coast[i] = distm(c(153.48, -29.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
  if (mydata$site[i] == "NorthSolitary") {
    mydata$Distance_Coast[i] = distm(c(153.23, -30.0), c(mydata$Lon[i], mydata$Lat[i]), fun = distHaversine)
  }
}

zoo_dat <- mydata

# Open and manipulate GEBCO Depth data



mydata <- nc_open("Data/GEBCO Data/GEBCO_2019_135.0_-20.0_160.0_-46.0.nc")

print(mydata)

# Get lat/Long - check dimensions
lon <- ncvar_get(mydata,"lon")
nlon <- dim(lon)
#head(lon)
lat <- ncvar_get(mydata,"lat")
nlat <- dim(lat)
#head(lat)
print(head(c(nlon,nlat)))


# Get variable
depth_array <- ncvar_get(mydata, "elevation")
dim(depth_array)


fillvalue <- ncatt_get(mydata, "elevation", "_FillValue")
fillvalue

nc_close(mydata) 

# Replace dodgy with NA
depth_array[depth_array == fillvalue$value] <- NA

dimnames(depth_array)[[1]] <- lon
dimnames(depth_array)[[2]] <- lat

depth_array[depth_array > 0] <- NA


# check plot

r <- raster(t(depth_array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, 2)

plot(r)


# Make raster Brick to test data extraction

toolik_lon <- 148#.49795
toolik_lat <- -44#2.262567



toolik_series <- extract(r, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple', na.rm=T)
toolik_series


#### Making a loop to do all values

zoo_dat$Bathy <- NA # initialise the column for Bathymetry
pb <- txtProgressBar(min = 0, max = length(zoo_dat$Lat), style = 3)

r <- raster(t(depth_array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r <- flip(r, 2)

for (i in 1:nrow(zoo_dat)){
  
  point_lon <- zoo_dat$Lon[i]
  point_lat <- zoo_dat$Lat[i]
  
  toolik_series <- extract(r, SpatialPoints(cbind(point_lon,point_lat)), method='simple', na.rm=T)
  
  zoo_dat$Bathy[i] <- toolik_series
  setTxtProgressBar(pb, i)
}

hist(zoo_dat$Bathy)


### Stop
write.csv(zoo_dat, file = "Data/SS2004_SeaSoarData_with_GEBCO.csv", row.names = FALSE)

