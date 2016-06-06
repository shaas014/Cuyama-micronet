##Alex Filazzola
## May 20th 2016
## Micronet and Cuyama HOBO data processing and analysis

##load functions
error.bar <- function(x, y, upper, lower=upper, length=0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=0, ...)
}
## SE
se <- function(x) sd(x)/sqrt(length(x))


##load data
hobo.data <- read.table("data/May2016_microdata.csv", header=T, sep=",")
hobo.data <- na.omit(hobo.data)


## summarize means for each month
month.means <- aggregate(hobo.data[,c("temp","smc")], by=list(hobo.data$month,hobo.data$microsite,hobo.data$site), mean)
colnames(month.means) <- c("month","microsite","site","temp","smc")

## summarize se for each month
month.se <- aggregate(hobo.data[,c("temp","smc")], by=list(hobo.data$month,hobo.data$microsite,hobo.data$site), se)
colnames(month.se) <- c("month","microsite","site","temp","smc")

## summarize SD for each month
month.SD <- aggregate(hobo.data[,c("temp","smc")], by=list(hobo.data$month,hobo.data$microsite,hobo.data$site), sd)
colnames(month.SD) <- c("month","microsite","site","temp","smc")

##average daily highs for each month
max.days <- aggregate(hobo.data[,c("temp","smc")], by=list(hobo.data$day,hobo.data$month,hobo.data$microsite,hobo.data$site), max)
colnames(max.days) <- c("day","month","microsite","site","temp","smc")
mean.max <- aggregate(max.days[,c("temp","smc")], by=list(max.days$month,max.days$microsite,max.days$site), mean)
colnames(mean.max) <- c("month","microsite","site","temp","smc")

##average daily lows for each month
min.days <- aggregate(hobo.data[,c("temp","smc")], by=list(hobo.data$day,hobo.data$month,hobo.data$microsite,hobo.data$site), min)
colnames(min.days) <- c("day","month","microsite","site","temp","smc")
mean.min <- aggregate(min.days[,c("temp","smc")], by=list(max.days$month,max.days$microsite,max.days$site), mean)
colnames(mean.min) <- c("month","microsite","site","temp","smc")

## combine statistics

month.stats <- rbind(month.means,month.se,mean.max,mean.min,month.SD)
stats <- c(rep("mean",length(month.means[,1])),rep("SE",length(month.se[,1])),rep("max",length(mean.max[,1])),rep("min",length(mean.min[,1])),rep("SD",length(month.SD[,1])))
month.stats["statistic"] <- stats

## output results
write.csv(month.stats, "HOBO.data.monthly.stats.csv")

##subset microsites and separate out months for plotting

## February, shrub and open for means
Feb.shrub<- month.stats %>% filter(microsite =="shrub" & month == 2 & statistic=="mean")
Feb.open<- month.stats %>% filter(microsite =="open" & month == 2 & statistic=="mean")

## February, shrub and open for SE
Feb.shrub.se<- month.stats %>% filter(microsite =="shrub" & month == 2 & statistic=="SE")
Feb.open.se<- month.stats %>% filter(microsite =="open" & month == 2 & statistic=="SE")

## Apr, shrub and open for means
Apr.shrub<- month.stats %>% filter(microsite =="shrub" & month == 4 & statistic=="mean")
Apr.open<- month.stats %>% filter(microsite =="open" & month == 4 & statistic=="mean")

## Apr, shrub and open for SE
Apr.shrub.se<- month.stats %>% filter(microsite =="shrub" & month == 4 & statistic=="SE")
Apr.open.se<- month.stats %>% filter(microsite =="open" & month == 4 & statistic=="SE")

# ## plot april microsite differences
# 
# par(mar=c(4.5,4.5,.5,.5))
# plot(Apr.shrub$site-0.1, Apr.shrub$temp, pch=19, cex=1.5, xlim=c(0.7,6.3), ylim=c(12,22), ylab="temperature (C°)", xlab="site", cex.axis=1.5, cex.lab=1.8)
# error.bar(Apr.shrub$site-0.1, Apr.shrub$temp, Apr.shrub.se$temp, lwd=2)
# error.bar(Apr.open$site+0.1, Apr.open$temp, Apr.open.se$temp, lwd=2)
# points(Apr.open$site+0.1, Apr.open$temp, pch=21, cex=1.5, bg="white")
# 
# par(mar=c(4.5,5,.5,.5))
# plot(Apr.shrub$site-0.1, Apr.shrub$smc, pch=19, cex=1.5, xlim=c(0.7,6.3), ylim=c(0,0.3), ylab=expression("soil moisture (m"^"3"*"/m"^"3"*")"), xlab="site", cex.axis=1.5, cex.lab=1.8)
# error.bar(Apr.shrub$site-0.1, Apr.shrub$smc, Apr.shrub.se$smc, lwd=2)
# error.bar(Apr.open$site+0.1, Apr.open$smc, Apr.open.se$smc, lwd=2)
# points(Apr.open$site+0.1, Apr.open$smc, pch=21, cex=1.5, bg="white")
# legend(4.5,0.29, c("shrub","open"), pch=c(19,1), cex=1.4)


