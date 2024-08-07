# Extracting species Geolocation data from other sources in R
# 7/8/2024

#LIBRARY----
#install.packages(spocc)
library(spocc)

# get data from GBIF
results = occ(query = 'Rhinoplax vigil', from = 'gbif')
results
head(results$gbif)

# get data from GBIF and eBird
results = occ(query = 'Rhinoplax vigil', from = c('ebird','gbif','ecoengine'))
summary(results)
   
# get data for multiple species from multiple data bases
spp = c("Rhinoplax vigil", "Buceros rhinoceros", "Antracoceros malayanus")
res_set = occ(spp, from = c('gbif', 'ecoengine'))
head(res_set)

# clean for georef records only
dat = occ(query = spp, from = 'gbif', gbifopts = list(hasCoordinate = TRUE))
data = occ2df(dat) # converting to data frame
head(data)

names(data)
