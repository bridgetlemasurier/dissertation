####################################################################
######### CRS of the raster data

library(raster)
library(terra)

j <- raster("sdm_course/sections/basics/join_59_60.tif")

plot(j)

# converting lat long to utm ----

j # this is in lat long crs - what we need

#utm projection for north borneo 
ref = "+proj=utm +zone=50 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# project utm, zone 50, 

projectedU = projectRaster(j, crs = ref) # this took years, but worked using terra
projectedU


# converting from utm to lat-long ----
s=raster("slp2.tif")

plot(s)

s

## utm to lat-long

ref= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

projectedL = projectRaster(s, crs = ref)

projectedL
