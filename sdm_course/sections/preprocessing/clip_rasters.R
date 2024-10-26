####################################################################
######### Clip raster data to a given extent

library(raster)
library(terra)

tmean = raster("sdm_course/sections/data/ready_rasters/tempAvg.tif")
tmean

tmin=raster("sdm_course/sections/data/ready_rasters/tempMin.tif")
tmin

precip=raster("sdm_course/sections/data/ready_rasters/preciptn.tif")
precip

alt=raster("sdm_course/sections/data/alt/MYS_alt.tif")
alt

plot(alt)

## I couldnt find the data this code matches 
# this goes over how to clip rasters which i will need to know how to do

#read in a shapefile

setwd("sdm_course/sections/data/MYS_adm/MYS_adm0")

library(sf)

pm = read_sf(".", "MYS_adm0") # read_sf = sf replacement for readOGR

plot(pm, add=T)

altc = crop(alt, pm)
plot(altc)

## ok she didn't give us the updated shapefile for peninsular malaysia but has
## provided the clipped rasters required for the rest of the course
# we know how to clip a raster to a shape file now - we can use this