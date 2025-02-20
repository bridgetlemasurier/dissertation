# ssp2 4.5 scenario file clipping

# 1. import nc file
# 2. stack rasters
# 3. chop stack
# 4. save stack as .tif

# library ----
library(raster)
library(terra)


# SSP2 (4.5)
setwd("data/environment/ssp45")

# rasters
currentdirection45 <- rast("currentdirection45.nc")
currentvelocity45 <- rast("currentvelocity45.nc")
dissolvedO245 <- rast("dissolvedO245.nc")
iron45 <- rast("iron45.nc")
nitrate45 <- rast("nitrate45.nc")
pH45 <- rast("pH45.nc")
phosphate45 <- rast("phosphate45.nc")
primaryprod45 <- rast("primaryprod45.nc")
salinity45 <- rast("salinity45.nc")
silicate45 <- rast("silicate45.nc")
temp45 <- rast("temp45.nc")

# stack
ssp2_stack <- c(currentdirection45, currentvelocity45, dissolvedO245, 
                iron45, nitrate45, pH45, phosphate45, primaryprod45, 
                salinity45, silicate45, temp45)

plot(ssp2_stack)

# chop 

NAtl_extent <- ext(-60, 45, 41, 83)

NAtl_ssp2_stack <- crop(ssp2_stack, NAtl_extent)

plot(NAtl_ssp2_stack)

# save
writeRaster(NAtl_ssp2_stack, "NAtl_ssp2_stack.tif", overwrite = TRUE, filetype = "GTiff")
