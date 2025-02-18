# predictor stack
# 17/02/2025

# aim: put all the predictor rasters in a stack for ease

# library
library(terra)
library(raster)
library(tidyverse)

# bio-oracle layers----
setwd("D:/dissertation_BL/data/environment/NAtl_rasters")

bioracle_layers = Sys.glob("*.tif")
bioracle_layers

bioracle_stack <- stack() #empty raster stack for storing raster layers

for(i in 1:NROW(bioracle_layers)){
  tempraster = raster(bioracle_layers[i])  # temporary raster
  bioracle_stack = stack(bioracle_stack,tempraster)
}

bioracle_stack

# add in substrate -----
setwd("D:/dissertation_BL")
folk5_1M <- rast("data/environment/substrate/qgis_rasters/5folk_1M.tif")
folk5_1M

# Ensure the raster is treated as categorical
substrate <- as.factor(folk5_1M)

levels(substrate)
cats(substrate)

substrate_levels <- data.frame(id = 0:6, 
                               cover = c(NA,
                                         "Mud to muddy Sand",
                                         "Sand",
                                         "Coarse-grained sediment",
                                         "Mixed sediment",
                                         "Rock & boulders",
                                         NA))
levels(substrate) <- substrate_levels

levels(substrate)
terra::plot(substrate) # YAYYYYYYYYYYY

substrate_raster <- raster(substrate)


# creating predictor stack

bioracle_terra <- rast(bioracle_stack)

predictors <- c(bioracle_terra, substrate)
predictors

plot(predictors)

# select for layers im keeping ----
names(predictors)

env_layers <- c("terrain_ruggedness_index", 
                "sws_mean", 
                "o2_mean", 
                "si_mean",
                "thetao_mean",
                "cover")

env_vars <- predictors[[env_layers]]
summary(env_vars)
plot(env_vars)

writeRaster(env_vars, "present_env_vars.tif", overwrite = TRUE)

