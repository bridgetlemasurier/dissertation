# substrate raster
# 28/1/2025

# aim: to convert substrate shapefile into raster layer to put into bio-oracle stack
# need to:
# 1. reproject shapefile onto stack CRS (+proj=longlat +datum=WGS84 +no_defs)
# 2. rasterise data
# 3. assign NAs
# 4. encorporate into stack

# library
library(terra)
library(raster)
library(tidyverse)

# bioracle stack ----

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

# reset to project directory

# load in raster ----
folk5_1M <- raster("data/environment/substrate/qgis_rasters/5folk_1M.tif")
plot(folk5_1M)


# check sponges match up
sponges <- read.csv("data/sponge/tidyishsponge.csv")
sponge_occs <- sponges%>%
  mutate(lat = MiddleLatitude, long = MiddleLongitude)%>%
  dplyr::select(long, lat)  # needs to be long then lat

plot(folk5_1M)
points(sponge_occs, col = "red")

# put substrate into stack ----
predictor_stack <- addLayer(bioracle_stack, folk5_1M)

summary(predictor_stack)
plot(predictor_stack,17) # yay!!!


#check NAs have been handled in sponge data ----
sponge_info <- raster::extract(predictor_stack, sponge_occs)
sponge_infodf = data.frame(cbind(sponges, sponge_info))
