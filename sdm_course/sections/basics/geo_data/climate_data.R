# Accessing climate and other data in R

# Normally you download the specific raster data you need but useful to 
# know how to do it

# LIBRARY ----
library(raster)

# DATA ----

# Country outlines ----
# divaGIS is good resource for country outlines outside of R
# can get same data from r

# my0 = getData('GADM', country = 'MYS', level = 0)

# MINERVAS WAY WAS OUTDATED - AHHH
# MYS = country code for malaysia, level 0 = country outline

# using geodata instead

my0 <- gadm(country = 'MYS', level = 0, path = tempdir("sdm_course/sections/basics/geo_data"))

my1 <- gadm(country = 'MYS', level = 1, path = tempdir("sdm_course/sections/basics/geo_data"))
# level 1 = states included

par(mfrow = c(1,2))

plot(my0, main = "Adm. Boundaries Malaysia Level 0")
plot(my1, main = "Adm. Boundaries Malaysia Level 1")

#YAYYYYYYYY

## WORLD CLIMATE ----
# climate =  getData('worldclim', var = 'bio', res = 2.5)

climate <- worldclim_global(var = 'bio', res = 2.5, path = "sdm_course/sections/basics/geo_data")

# need to use geodata package instead


### PLOTTING BIOCLIMATIC VARIABLES ----
plot(climate$wc2.1_2.5m_bio_1, main = "Mean Annual Temperature")
plot(climate$wc2.1_2.5m_bio_5, main = "Maximum Temperature")
