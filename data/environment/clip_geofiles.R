# clipping rasters
# 13/1/2024


# aim
# clip raster files to bounding box = xlim = c(-55, 36), ylim = c(45, 83)
# found in data/sponge/tidysponges.R 
# subject to change but this extent encompasses all presence data


## library ----
library(terra)
library(raster)

# loading sponge records for reference ----
sponges <- read.csv("data/sponge/tidyishsponge.csv")
sponge_occs <- sponges%>%
  mutate(lat = MiddleLatitude, long = MiddleLongitude)%>%
  dplyr::select(long, lat)  # needs to be long then lat


# set wd ----
setwd("D:/dissertation_BL/data/environment")


# Define extent ----
# Extent: The minimum bounding rectangle (xmin, xmax, ymin, ymax)
# NAtl_extent <- ext(-60, 45, 41, 83) # North Atlantic (ish) extent

NAtl_extent <- ext(-60, 45, 41, 83)


# 1. aspect ----
aspect <- rast("aspect.tif")  # import raster

NAtl_aspect <- crop(aspect, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_aspect) # yay!
points(sponge_occs, col = "red")

writeRaster(NAtl_aspect, "NAtl_rasters/NAtl_aspect.tif", overwrite = TRUE) # save cropped raster



# 2. bathymetry ----
bathymetry <- rast("bathymetry.tif")  # import raster

NAtl_bathymetry <- crop(bathymetry, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_bathymetry) # yay!

writeRaster(NAtl_bathymetry, "NAtl_rasters/NAtl_bathymetry.tif", overwrite = TRUE)


# 3. currentdirection ----
currentdirection <- rast("currentdirection.tif")  # import raster

NAtl_currentdirection <- crop(currentdirection, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_currentdirection) # yay!

writeRaster(NAtl_currentdirection, "NAtl_rasters/NAtl_currentdirection.tif", overwrite = TRUE)


# 4. currentvelocity ----
currentvelocity <- rast("currentvelocity.tif")  # import raster

NAtl_currentvelocity <- crop(currentvelocity, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_currentvelocity) # yay!

writeRaster(NAtl_currentvelocity, "NAtl_rasters/NAtl_currentvelocity.tif", overwrite = TRUE)


# 5. dissolvedO2 ----
dissolvedO2 <- rast("dissolvedO2.tif")  # import raster

NAtl_dissolvedO2 <- crop(dissolvedO2, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_dissolvedO2) # yay!

writeRaster(NAtl_dissolvedO2, "NAtl_rasters/NAtl_dissolvedO2.tif", overwrite = TRUE)


# 6. iron ----
iron <- rast("iron.tif")  # import raster

NAtl_iron <- crop(iron, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_iron) # yay!

writeRaster(NAtl_iron, "NAtl_rasters/NAtl_iron.tif", overwrite = TRUE)


# 7. nitrate ----
nitrate <- rast("nitrate.tif")  # import raster

NAtl_nitrate <- crop(nitrate, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_nitrate) # yay!

writeRaster(NAtl_nitrate, "NAtl_rasters/NAtl_nitrate.tif", overwrite = TRUE)


# 8. pH ----
pH <- rast("pH.tif")  # import raster

NAtl_pH <- crop(pH, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_pH) # yay!

writeRaster(NAtl_pH, "NAtl_rasters/NAtl_pH.tif", overwrite = TRUE)


# 9. phosphate ----
phosphate <- rast("phosphate.tif")  # import raster

NAtl_phosphate <- crop(phosphate, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_phosphate) # yay!

writeRaster(NAtl_phosphate, "NAtl_rasters/NAtl_phosphate.tif", overwrite = TRUE)


# 10. primaryprod ----
primaryprod <- rast("primaryprod.tif") # import raster

NAtl_primaryprod <- crop(primaryprod, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_primaryprod) # yay!

writeRaster(NAtl_primaryprod, "NAtl_rasters/NAtl_primaryprod.tif", overwrite = TRUE)


# 11. salinity ----
salinity <- rast("salinity.tif") # import raster

NAtl_salinity <- crop(salinity, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_salinity) # yay!

writeRaster(NAtl_salinity, "NAtl_rasters/NAtl_salinity.tif", overwrite = TRUE)


# 12. silicate ----
silicate <- rast("silicate.tif") # import raster

NAtl_silicate <- crop(silicate, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_silicate) # yay!

writeRaster(NAtl_silicate, "NAtl_rasters/NAtl_silicate.tif", overwrite = TRUE)


# 13. slope ----
slope <- rast("slope.tif") # import raster

NAtl_slope <- crop(slope, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_slope) # yay!

writeRaster(NAtl_slope, "NAtl_rasters/NAtl_slope.tif", overwrite = TRUE)


# 14. temp ----
temp <- rast("temp.tif") # import raster

NAtl_temp <- crop(temp, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_temp) # yay!

writeRaster(NAtl_temp, "NAtl_rasters/NAtl_temp.tif", overwrite = TRUE)


# 15. TPI ----
TPI <- rast("TPI.tif") # import raster

NAtl_TPI <- crop(TPI, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_TPI) # yay!

writeRaster(NAtl_TPI, "NAtl_rasters/NAtl_TPI.tif", overwrite = TRUE)


# 16. TRI ----
TRI <- rast("TRI.tif") # import raster

NAtl_TRI <- crop(TRI, NAtl_extent) # crop raster to North Atlantic

plot(NAtl_TRI) # yay!

writeRaster(NAtl_TRI, "NAtl_rasters/NAtl_TRI.tif", overwrite = TRUE)

# bioracle stack (test) ----

# set new wd
setwd("D:/dissertation_BL/data/environment/NAtl_rasters")

# list of new predictors
bioracle_layers = Sys.glob("*.tif")
bioracle_layers

bioracle_stack <- stack() #empty raster stack for storing raster layers

for(i in 1:NROW(bioracle_layers)){
  tempraster = raster(bioracle_layers[i])  # temporary raster
  bioracle_stack = stack(bioracle_stack,tempraster)
}

bioracle_stack

# all same dims, res, ext and crs - dont need to change :)
# class      : RasterStack 
# dimensions : 760, 2020, 1535200, 16  (nrow, ncol, ncell, nlayers)
# resolution : 0.05, 0.05  (x, y)
# extent     : -60, 41, 45, 83  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 

plot(bioracle_stack) # yay, need to change graph titles if presenting

