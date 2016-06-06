## load packages for spatial manipulation
library(raster)
library(rgdal)
library(dismo)
library(rJava)
library(maptools)
library(rgeos)
library(ncdf4)


##load functions
error.bar <- function(x, y, upper, lower=upper, length=0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=0, ...)
}
se <- function(x) sd(x)/sqrt(length(x)) ## standard error

get_cru <- function(lat, lon, variable, folder){ ##extract CRU data
  CRU <- file.path(folder, sprintf('cru_ts3.21.1901.2012.%s.dat.nc', variable))
  nc  <-  nc_open(CRU)
  lat.vals  <-    ncvar_get(nc,varid="lat")
  lon.vals    <-    ncvar_get(nc,varid="lon")
  days.since <- ncvar_get(nc,varid="time")
  start.time = 1
  
  lat.i  <-  which.min(abs(lat.vals-lat))[1]
  lon.i  <-  which.min(abs(lon.vals-lon))[1]
  
  vals = ncvar_get(nc=nc, varid=variable,start=c(lon.i,lat.i,start.time),count=c(1,1,length(days.since)))
  nc_close(nc)
  
  days.since = as.Date('1900-01-01')+days.since
  df <- data.frame("DateTime"=days.since,"vals"=vals)
  names(df)[2] <- variable
  return(df)
}

##input lat longs for each site. 
site <- c(1,2,3,4,5,6)
lat.site<- c(34.84812596,34.85474397,34.93852797,34.94738999,35.04567903,35.04867003)
lon.site<-c(-119.483145,-119.485971,-119.481707,-119.48008,-119.889839,-119.896268)
gps.site <- data.frame(site=site,lat=lat.site,lon=lon.site)

##create empty data framec
climate.data <- data.frame(site=character(),DateTime=as.Date(character()),pre=character(),tmp=character(),pet=character())

##loop extraction of climate data (3 var) for each of the 6 sites. 
## precipitation, temperature, potential evapotranspiration
for(i in 1:6){
precip.data <- get_cru(gps.site[i,2],gps.site[i,3], "pre", "C:\\Users\\Fitz\\Documents\\CRU data")
temp.data <- get_cru(gps.site[i,2],gps.site[i,3], "tmp", "C:\\Users\\Fitz\\Documents\\CRU data")
pet.data <- get_cru(gps.site[i,2],gps.site[i,3], "pet", "C:\\Users\\Fitz\\Documents\\CRU data")
site.name <- rep(paste("site",i),length(precip.data))
precip.data <- cbind(site.name,precip.data,temp=temp.data[,2],pet=pet.data[,2])
climate.data <- rbind(climate.data,precip.data)
}

## expand date into columns
datetxt <- climate.data[,"DateTime"]
date.columns <- data.frame(year = as.numeric(format(datetxt, format = "%Y")),month = as.numeric(format(datetxt, format = "%m")))

## add back to climate data
climate.data <- cbind(climate.data,date.columns)

means <- aggregate(climate.data, by=list(climate.data$site.name), mean)
ses <- aggregate(climate.data, by=list(climate.data$site.name), se)