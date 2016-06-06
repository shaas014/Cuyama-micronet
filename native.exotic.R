#author: cjlortie
#version: 1.1
#year: 2016
#title: native.exotic vegetation filter
#purpose: sort basic vegetation data from quadrat censuses into native/exotic species
#dependencies: dplyr

#preferred column headers for censuses: site,microsite,census,date,treatment (if applied), cover, species, total.density, richness

library(dplyr)
select <- dplyr::select 

#read census data ####
veg<-read.csv("data/plant.censuses.csv")
str(veg)
#head(veg)

#read species origins ####
origins <-read.csv("data/species.origins.csv")
str(origins)
#head(origins)

#mutate a new column, match, merge, join, loop

#parse ####
#natives <- subset(veg(if(colnames == origins$origin == 'native')))

#natives <- veg %>% 
  group_by(site, microsite) %>%
  select(origins$origin == 'native)))

#natives <- veg %>%
  group_by(site, microsite) %>%
         select(showy.madia, Owls.clover)

veg.dim<-dim(veg) #dim
columns<-veg.dim[2] #get number of columns to compare to native-exotic df

natives <- veg %>%
  group_by(site, microsite) %>%
         select(if c(... match)

veg[veg %in% origins$orgins == "native]

#natives <- veg %>%
  group_by(site, microsite) %>%
         select( use %in%)



