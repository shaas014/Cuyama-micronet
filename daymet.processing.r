## load daymet data
data <- read.table("daymet.data.csv", header=T, sep=",")

## load libraries
library(dplyr)

##load functions
error.bar <- function(x, y, upper, lower=upper, length=0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=0, ...)
}
## SE
se <- function(x) sd(x)/sqrt(length(x))

##gps points
site <- c(1,2,3,4,5,6)
lat.site<- c(34.84812596,34.85474397,34.93852797,34.94738999,35.04567903,35.04867003)
lon.site<-c(-119.483145,-119.485971,-119.481707,-119.48008,-119.889839,-119.896268)

## collapse by year for all variables except precipitation
climate.data <- aggregate(data, by=list(year=data$year, site=data$site), mean)
## sum total annual precipitaiton
annual.precip <- aggregate(data$prcp, by=list(year=data$year, site=data$site), sum)
climate.data["prcp"] <- annual.precip[,3] #replace precipitation with annual sum rather than daily mean
climate.data["aridity"] <- climate.data[,"prcp"]/(climate.data[,"tmax"]+10)

## data 1980-2015
## means, totals, and error
means <- aggregate(climate.data, by=list(site=climate.data$site), mean)
ses <- aggregate(climate.data, by=list(site=climate.data$site), se)
site <- seq(1,6,1)

## plot figures for precipitation and temperature max
par(mar=c(4.5,5,.5,4.5))
plot(site-0.1, means[,"tmax"],  ylim=c(20,24), pch=19, xlim=c(0.7,6.3), cex=2, ylab="temperature (C°)", cex.axis=1.5, cex.lab=1.8, xlab="site")
error.bar(site-0.1, means[,"tmax"],ses[,"tmax"])
par(new = T)
plot(site+0.1, means[,"prcp"], xaxt="n",yaxt="n", ylim=c(300,440),xlim=c(0.7,6.3),  pch=21, bg="White", cex=1.5, xlab="", ylab="")
error.bar(site+0.1,means[,"prcp"],ses[,"prcp"])
points(site+0.1,means[,"prcp"], pch=21, bg="White", cex=2)
axis(4, cex.axis=1.5)
mtext("annual precipitation (mm)", side = 4, line = 3, cex = 1.8)

## plot aridity
par(mar=c(4.5,5,.5,.5))
plot(site-0.1, means[,"aridity"],  ylim=c(8,15), pch=19, xlim=c(0.7,6.3), cex=1.5, ylab="De Martonne aridity", cex.axis=1.5, cex.lab=1.8, xlab="site")
error.bar(site-0.1, means[,"aridity"],ses[,"aridity"])

## data 2005-2010
## means, totals, and error
climate.data2 <- subset(climate.data, year>2004)
means <- aggregate(climate.data2, by=list(site=climate.data2$site), mean)
ses <- aggregate(climate.data2, by=list(site=climate.data2$site), se)
site <- seq(1,6,1)


## plot figures
par(mar=c(4.5,5,.5,4.5))
plot(site-0.1, means[,"tmax"],  ylim=c(20,24), pch=19, xlim=c(0.7,6.3), cex=1.5, ylab="temperature (C°)", cex.axis=1.5, cex.lab=1.8, xlab="site")
error.bar(site-0.1, means[,"tmax"],ses[,"tmax"])
par(new = T)
plot(site+0.1, means[,"prcp"], xaxt="n",yaxt="n", ylim=c(250,450),  pch=21, bg="White", cex=1.5, xlab="", ylab="")
error.bar(site+0.1,means[,"prcp"],ses[,"prcp"])
points(site+0.1,means[,"prcp"], pch=21, bg="White", cex=1.5)
axis(4, cex.axis=1.5)
mtext("annual precipitation (mm)", side = 4, line = 3, cex = 1.8)

## compensate for elevation
library(raster)
library(rgdal)

# worldclim data
r1 <- raster("C:\\Users\\Alessandro\\Dropbox\\Ecography niche paper\\Altitude.tif", package="raster") #alt

gps <- data.frame(x=lon.site,y=lat.site)
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(gps) <- ~x+y
proj4string(gps) <- crs.world

## extract elevation values
alt.site <- extract(r1, gps)
cor(alt.site, means[,7:12]) ## strong correlation between alt and temperature. Medium precip and VP

m1 <- lm(means[,"tmax"]~alt.site)
plot(alt.site, means[,"tmax"])
