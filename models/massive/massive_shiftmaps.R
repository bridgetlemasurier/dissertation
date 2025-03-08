# binary shift maps - massive

library(terra)
library(raster)
library(tidyverse)

#load rasters
present <- raster("models/massive/present_binary.tif")
SSP2 <- raster("models/massive/SSP2_binary.tif")
SSP5 <- raster("models/massive/SSP5_binary.tif")

# SSP2 ----
#plot current and future distribution
plot(present, main = "Present Day Pres/abs")
plot(SSP2, main = "SSP2 2040-2050 Pres/abs")

# change values of absence for SSP2 
SSP2[SSP2 == 0] <- -1

# create change map
SSP2change <- present + SSP2

SSP2change_map <- rast(SSP2change)

SSP2_change_factor <- terra::as.factor(SSP2change_map)

levels(SSP2_change_factor) <- data.frame(
  value = c(-1, 0, 1, 2),
  response = c("Absent", "Loss", "Gain", "Present"))

plot(SSP2_change_factor, col = c("lightblue", "red", "darkgreen", "gold"))


# SSP5 ----
#plot current and future distribution
plot(present, main = "Present Day Pres/abs")
plot(SSP5, main = "SSP5 2040-2050 Pres/abs")

# change values of absence for SSP5
SSP5[SSP5 == 0] <- -1

# create change map
SSP5change<- present + SSP5

SSP5_change_map <- rast(SSP5change)

SSP5_change_factor <- terra::as.factor(SSP5_change_map)

levels(SSP5_change_factor) <- data.frame(
  value = c(-1, 0, 1, 2),
  response = c("Absent", "Loss", "Gain", "Present"))
   
terra::plot(SSP5_change_factor, col = c("lightblue", "red", "darkgreen", "gold"))



