#author: cjlortie
#version: 1.1
#year: 2016
#title: tidy up a*b/site*microsite data & generate interaction plots
#purpose: standard cleanup to tidy and present shrub-open by site for whatever responses recorded (inckuding burrows, other plants, rii, or any interactions)
#dependencies: dplyr

#preferred column headers: site,microsite,treatment (if applied), response variables

#read data ####
#response: burrow frequency recorded under shrub & open for instance
burrows<-read.table("data/burrows.csv", sep=",", header=T)

#parse data into sites/microsites and calculate se ####
#get n for response to calculate se
n<-length(burrows$burrows)
#generate summary data frame for plotting
burrows.summary<- burrows %>%
  group_by(site, microsite) %>%
  summarize(frequency = mean(burrows), se = (sd(burrows))/sqrt(n))

#plot
interaction.plot(burrows.summary$site,burrows.summary$microsite,
                 burrows.summary$frequency, xlab="Cuyama sites", ylab="burrows", ylim=c(0,38),trace.label="")

error.bar(burrows.summary$site, burrows.summary$frequency,burrows.summary$se)
points(burrows.summary$site, burrows.summary$frequency)