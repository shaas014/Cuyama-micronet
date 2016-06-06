#author: cjlortie
#version: 1.2
#year: 2016
#title: Basic vegetation analyses
#purpose: basic read of field vegetation census data for density and richness
#dependencies: dplyr

#preferred column headers: site,microsite,census,date,treatment (if applied), cover, species, total.density, richness


#read census data ####
veg<-read.csv("data/plant.censuses.csv")
#str(veg)

#richness ####
#parse data into sites/microsites and calculate se
n <-length(veg$richness)
veg.richness <- veg %>%
  group_by(site,microsite) %>%
  summarize(species = mean(richness), se = (sd(richness))/sqrt(n)) 
head(veg.richness)

#plot
interaction.plot(veg.richness$site, veg.richness$microsite, veg.richness$species, xlab="sites", ylab="species richness", ylim=c(0,2.5), trace.label="")
points(veg.richness$site, veg.richness$species)
error.bar(veg.richness$site,veg.richness$species,veg.richness$se)

#density ####
#parse data into sites/microsites and calculate se
n <-length(veg$total.density)
veg.density <- veg %>%
  group_by(site, microsite) %>%
  summarize(density = mean(total.density), error = (sd(total.density))/sqrt(n)) 

#plot
interaction.plot(veg.density$site, veg.density$microsite, veg.density$density, xlab="sites", ylab="plant density", trace.label="")
points(veg.density$site, veg.density$density)
error.bar(veg.density$site,veg.density$density,veg.density$error)

#density-richness exploration ####
plot(veg.density$density,veg.richness$species, xlab="plant density", ylab="species richness")

cor.test(veg.density$density,veg.richness$species) #correlation coefficient

#ordination-community analysis ####
