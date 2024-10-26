######## resize/resample data- change spatial resolution

# need to run clip_rasters.R first

library(raster)

land = raster("sdm_course/sections/data/ready_rasters/landuse1.tif")
land

plot(land) # resolution : 0.002777778, 0.002777778  (x, y)

alt # coarser resolution: 0.008333333, 0.008333333  (x, y)

landC = resample(land, alt, method="bilinear")

## first arguement is the raster whose spatial resolution we want to change
## second raster is the one whose resolution we want to apply on 1
## can specify any interpolation technique - here bilinear

landC

plot(landC)
