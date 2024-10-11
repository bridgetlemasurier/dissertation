# Working with elevation data in R

# LIBRARIES
library(raster)
library(terra)

#rgdal is no longer available so using terra instead

#Elevation raster plots----

el1 = raster("sdm_course/sections/basics/geo_data/srtm_59_12/srtm_59_12.tif")
plot(el1)


el2 = raster("sdm_course/sections/basics/geo_data/strm_60_11/srtm_60_11.tif")
plot(el2)


# Mosaics of 2 plots ----
par(mfrow = c(1,1))
mosee = mosaic(el1, el2, fun = mean)
plot(mosee)

writeRaster(mosee, "join_59_60.tif")  # save to hard drive

# TOPOGRAPHIC PRODUCTS----
# no joy at all - fix later if need be xx

# slope
mosee

# want to convert lat-long rasters to planat units such as UTM
# in meters

#utm projection for north borneo
#utm projection for north borneo 
crs(mosee) <-  CRS("+proj=utm +zone=50 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 

projected_raster = projectRaster(mosee, crs = ref)

projected_raster


slp=terrain(projected_raster, opt='slope', unit='radians', neighbors=8, filename='slp2.tif')

plot(slp)