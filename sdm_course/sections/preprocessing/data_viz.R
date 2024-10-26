#####################################################################
############ More data visualizations

library(spocc)

## get data for multiple species from multiple DBs
spp <- c("Rhinoplax vigil", "Buceros rhinoceros", "Anthracoceros malayanus")

dat <- occ(query = spp, from ='gbif', gbifopts = list(hasCoordinate=TRUE))
dat

library(leaflet)

leaflet(dat) #leaflet based interactive map - need to come back not working

library(ggplot2)
library(ggmap)

map_ggmap(dat) # also not working!! great :)))