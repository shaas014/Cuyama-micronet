## Extract elevation for Cuyama sites
## Alex Filazzola May 27 2016

## load libraries
library(raster)
library(rgdal)

## extract elevation from worldclim data
r1 <- getData("worldclim", var="alt", res=0.5, lon=-119, lat=33)

## load gps coordinates for each site
site <- c(1,2,3,4,5,6)
lat.site<- c(34.84812596,34.85474397,34.93852797,34.94738999,35.04567903,35.04867003)
lon.site<-c(-119.483145,-119.485971,-119.481707,-119.48008,-119.889839,-119.896268)

## identify as spatial points
gps <- data.frame(x=lon.site,y=lat.site)
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(gps) <- ~x+y
proj4string(gps) <- crs.world

## extract elevation values
alt.site <- extract(r1, gps)
alt.site <-data.frame(site=site, elevation=alt.site)
