## load packages for spatial manipulation
library(raster)
library(rgdal)
library(dismo)
library(rJava)
library(maptools)
library(rgeos)

##load functions
error.bar <- function(x, y, upper, lower=upper, length=0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=0, ...)
}
## SE
se <- function(x) sd(x)/sqrt(length(x))

##input lat longs for each site. 
site <- c(1,2,3,4,5,6)
lat.site<- c(34.84812596,34.85474397,34.93852797,34.94738999,35.04567903,35.04867003)
lon.site<-c(-119.483145,-119.485971,-119.481707,-119.48008,-119.889839,-119.896268)
 
## extract relevant bioclim data
bioclim.data <- getData('worldclim', var='bio', res=0.5, lat=35, lon=-119)

## assign gps points as spatial points
gps <- data.frame(x=lon.site,y=lat.site)
crs.world <-CRS("+proj=longlat +datum=WGS84") #same as worldclim
coordinates(gps) <- ~x+y
proj4string(gps) <- crs.world

## extract worldclim values
bioclim.data <- extract(bioclim.data, gps)

##rename columns
colnames(bioclim.data) <- c("annual.temp","monthly.temp.range","isothermality","temp.seasonality","max.month.temp","min.month.temp","annual.temp.range","temp.wet.quart","temp.dry.quart","temp.warm.quart","temp.cold.quart","annual.precip","max.month.precip","min.month.precip","precip.seasonality","precip.wet.quart","precip.dry.quart","precip.warm.quart","precip.cold.quart")

## calculate standard error
temp.se <- (bioclim.data[,"temp.seasonality"]/100)/sqrt(50)
precip.se <- ((bioclim.data[,"precip.seasonality"])*(bioclim.data[,"annual.precip"])/100)/sqrt(50)


par(mar=c(4.5,5,.5,4.5))
plot(site-0.1,bioclim.data[,"annual.temp"]/10, ylim=c(0,25), pch=19, xlim=c(0.7,6.3), cex=1.5, ylab="temperature (C°)", cex.axis=1.5, cex.lab=1.8, xlab="site")
error.bar(site-0.1,(bioclim.data[,"annual.temp"]/10),temp.se)
par(new = T)
plot(site+0.1,bioclim.data[,"annual.precip"],  xaxt="n", yaxt="n", xlim=c(0.7,6.3), ylim=c(400,650), pch=21, bg="White", cex=1.5, xlab="", ylab="")
error.bar(site+0.1,bioclim.data[,"annual.precip"],precip.se)
points(site+0.1,bioclim.data[,"annual.precip"], pch=21, bg="White", cex=1.5)
axis(4, cex.axis=1.5)
mtext("annual precipitation (mm)", side = 4, line = 3, cex = 1.8)
