# SSP2 projection stack

library(terra)

SSP2_change <- rast("data/environment/ssp45/NAtl_ssp2_stack.tif")

plot(SSP2_change)

TRI <- rast("data/environment/NAtl_rasters/NAtl_TRI.tif")

SSP2_predictors <- c(TRI, SSP2_change)

SSP2_predictors

# select for layers im keeping ----
names(SSP2_predictors)

env_layers <- c("terrain_ruggedness_index", 
                "sws_mean", 
                "o2_mean", 
                "si_mean",
                "thetao_mean")

SSP2_predictors <- SSP2_predictors[[env_layers]]
summary(SSP2_predictors)

plot(SSP2_predictors)

writeRaster(SSP2_predictors, "data/environment/ssp45/SSP2_predictors4050.tif", overwrite = TRUE)
