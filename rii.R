#author: alex filazzola & suggested be cjlortie :)
#version: 1.1
#year: 2016
#purpose: to calculate Rii
#Armas, C., Ordiales, R., & Pugnaire, F. I. (2004). Measuring plant interactions: a new comparative indedata. Ecology, 85(10), 2682-2686.

#preferred column headers: microsite with Shrub, Open
# data = dataframe
# x = factors to keep
# y = response variables for rii function to operate on


rii <- function(x, j, var)
{
  #parse out shrub and open
  s1 <- subset(x, microsite == "Shrub", select=var)
  o1 <- subset(x, microsite == "Open", select=var)
  return1 <- (s1 - o1) / (s1+o1)  # Rii formula
  # attach factors
  x1 <- x[seq(1, nrow(x), by = 2),]
  return2 <- cbind(x1[j], return1)
  return2[is.na(return2)] <- 0
  print(return2)
}

#ensure that you then assign to a matrix/object
#rii.dat <- rii(quadrat.data, c(1,5), 16:17)


