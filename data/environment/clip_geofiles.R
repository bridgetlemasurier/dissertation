# clipping rasters
# 13/1/2024


# aim
# clip raster files to bounding box = xlim = c(-60, 45), ylim = c(41, 83)
# found in data/sponge/tidysponges.R 
# subject to change but this extent encompasses all presence data


## library ----
library(terra)


# set wd ----
setwd("D:/dissertation_BL/data/environment")


# Define extent ----
# Extent: The minimum bounding rectangle (xmin, ymin and xmax, ymax)
NAtl_extent <- ext(-60, 41, 45, 83) # North Atlantic (ish) extent


# aspect ----
aspect <- rast("aspect.tif")  # import raster

NAtl_aspect <- crop(aspect, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_aspect) # yay!

writeRaster(NAtl_aspect, "NAtl_rasters/NAtl_aspect.tif") # save cropped raster


# bathymetry ----
bathymetry <- rast("bathymetry.tif")  # import raster

NAtl_bathymetry <- crop(bathymetry, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_bathymetry) # yay!

writeRaster(NAtl_bathymetry, "NAtl_rasters/NAtl_bathymetry.tif")


# currentdirection ----
currentdirection <- rast("currentdirection.tif")  # import raster

NAtl_currentdirection <- crop(currentdirection, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_currentdirection) # yay!

writeRaster(NAtl_currentdirection, "NAtl_rasters/NAtl_currentdirection.tif")


# currentvelocity ----
currentvelocity <- rast("currentvelocity.tif")  # import raster

NAtl_currentvelocity <- crop(currentvelocity, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_currentvelocity) # yay!

writeRaster(NAtl_currentvelocity, "NAtl_rasters/NAtl_currentvelocity.tif")


# dissolvedO2 ----
dissolvedO2 <- rast("dissolvedO2.tif")  # import raster

NAtl_dissolvedO2 <- crop(dissolvedO2, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_dissolvedO2) # yay!

writeRaster(NAtl_dissolvedO2, "NAtl_rasters/NAtl_dissolvedO2.tif")


# iron ----
iron <- rast("iron.tif")  # import raster

NAtl_iron <- crop(iron, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_iron) # yay!

writeRaster(NAtl_iron, "NAtl_rasters/NAtl_iron.tif")


# nitrate ----
nitrate <- rast("nitrate.tif")  # import raster

NAtl_nitrate <- crop(nitrate, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_nitrate) # yay!

writeRaster(NAtl_nitrate, "NAtl_rasters/NAtl_nitrate.tif")


# pH ----
pH <- rast("pH.tif")  # import raster

NAtl_pH <- crop(pH, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_pH) # yay!

writeRaster(NAtl_pH, "NAtl_rasters/NAtl_pH.tif")


# phosphate ----
phosphate <- rast("phosphate.tif")  # import raster

NAtl_phosphate <- crop(phosphate, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_phosphate) # yay!

writeRaster(NAtl_phosphate, "NAtl_rasters/NAtl_phosphate.tif")


# primaryprod ----
primaryprod <- rast("primaryprod.tif") # import raster

NAtl_primaryprod <- crop(primaryprod, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_primaryprod) # yay!

writeRaster(NAtl_primaryprod, "NAtl_rasters/NAtl_primaryprod.tif")


# salinity ----
salinity <- rast("salinity.tif") # import raster

NAtl_salinity <- crop(salinity, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_salinity) # yay!

writeRaster(NAtl_salinity, "NAtl_rasters/NAtl_salinity.tif")


# silicate ----
silicate <- rast("silicate.tif") # import raster

NAtl_silicate <- crop(silicate, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_silicate) # yay!

writeRaster(NAtl_silicate, "NAtl_rasters/NAtl_silicate.tif")


# slope ----
slope <- rast("slope.tif") # import raster

NAtl_slope <- crop(slope, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_slope) # yay!

writeRaster(NAtl_slope, "NAtl_rasters/NAtl_slope.tif")


# temp ----
temp <- rast("temp.tif") # import raster

NAtl_temp <- crop(temp, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_temp) # yay!

writeRaster(NAtl_temp, "NAtl_rasters/NAtl_temp.tif")


# TPI ----
TPI <- rast("TPI.tif") # import raster

NAtl_TPI <- crop(TPI, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_TPI) # yay!

writeRaster(NAtl_TPI, "NAtl_rasters/NAtl_TPI.tif")


# TRI ----
TRI <- rast("TRI.tif") # import raster

NAtl_TRI <- crop(TRI, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_TRI) # yay!

writeRaster(NAtl_TRI, "NAtl_rasters/NAtl_TRI.tif")

