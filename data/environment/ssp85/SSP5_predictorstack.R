# SSP2 projection stack

library(terra)

SSP5_change <- rast("data/environment/ssp85/NAtl_ssp5_stack.tif")

plot(SSP5_change)

TRI <- rast("data/environment/NAtl_rasters/NAtl_TRI.tif")

plot(TRI)

SSP5_predictors <- c(TRI, SSP5_change)

SSP5_predictors

# select for layers im keeping ----
names(SSP5_predictors)

env_layers <- c("terrain_ruggedness_index", 
                "sws_mean", 
                "o2_mean", 
                "si_mean",
                "thetao_mean")

SSP5_predictors <- SSP5_predictors[[env_layers]]
summary(SSP5_predictors)

plot(SSP5_predictors)

writeRaster(SSP5_predictors, "data/environment/ssp85/SSP5_predictors4050.tif", overwrite = TRUE)
