####################################################################
######### CRS of the raster data

<<<<<<< HEAD
=======
setwd("sdm_course/sections")
>>>>>>> 8ed2c7c729c36732c98c6336b86efaa1d829af75
library(raster)
library(terra)


<<<<<<< HEAD
j <- raster("sdm_course/sections/basics/join_59_60.tif")
=======
j <- raster("preprocessing/join_59_60.tif")
>>>>>>> 8ed2c7c729c36732c98c6336b86efaa1d829af75

plot(j)

# converting lat long to utm ----

j # this is in lat long crs - what we need

#utm projection for north borneo 
ref = "+proj=utm +zone=50 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# project utm, zone 50, 

projectedU = projectRaster(j, crs = ref) # this took years, but worked using terra
projectedU


<<<<<<< HEAD
# converting from utm to lat-long ----

=======
>>>>>>> 8ed2c7c729c36732c98c6336b86efaa1d829af75
s=raster("slp2.tif")

plot(s)

s

## utm to lat-long

ref= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

projectedL = projectRaster(s, crs = ref)

projectedL
