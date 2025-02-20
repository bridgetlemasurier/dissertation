# SSP5 (8.5) scenario file clipping

# 1. Import .nc files
# 2. Stack rasters
# 3. Crop stack to North Atlantic extent
# 4. Save stack as .tif

# Libraries ----
library(raster)
library(terra)

# Set working directory for SSP5-8.5
setwd("data/environment/ssp85")

# Import rasters
currentdirection85 <- rast("currentdirection85.nc")
currentvelocity85 <- rast("currentvelocity85.nc")
dissolvedO285 <- rast("dissolvedO285.nc")
iron85 <- rast("iron85.nc")
nitrate85 <- rast("nitrate85.nc")
pH85 <- rast("pH85.nc")
phosphate85 <- rast("phosphate85.nc")
primaryprod85 <- rast("primaryprod85.nc")
salinity85 <- rast("salinity85.nc")
silicate85 <- rast("silicate85.nc")
temp85 <- rast("temp85.nc")

# Stack rasters
ssp5_stack <- c(currentdirection85, currentvelocity85, dissolvedO285, 
                iron85, nitrate85, pH85, phosphate85, primaryprod85, 
                salinity85, silicate85, temp85)

plot(ssp5_stack)

# Crop to North Atlantic extent
NAtl_extent <- ext(-60, 45, 41, 83)
NAtl_ssp5_stack <- crop(ssp5_stack, NAtl_extent)

plot(NAtl_ssp5_stack)

summary(NAtl_ssp5_stack)

# Save as GeoTIFF
writeRaster(NAtl_ssp5_stack, "NAtl_ssp5_stack.tif", overwrite = TRUE, filetype = "GTiff")


# compare ssp2 and ssp5
ssp2 <- rast("data/environment/ssp45/NAtl_ssp2_stack.tif")
ssp5 <- rast("data/environment/ssp85/NAtl_ssp5_stack.tif")

ssp2
ssp5

plot(ssp2)
plot(ssp5)

summary(ssp2)
summary(ssp5)
