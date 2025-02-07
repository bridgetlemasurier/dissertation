# bio oracle GeoTIFF files

#library ----
library(terra)

#set wd ----
setwd("D:/dissertation_BL/data/environment")

# temperature ----
# path
temp_nc_path <- "tempmean.nc"
#read layer
temp_nc <- rast(temp_nc_path)

temp_nc

plot(temp_nc)

# Write as GeoTIFF
writeRaster(temp_nc, "temp.tif")

# salinity ----

# path
salinity_nc_path <- "salinitymean.nc"
#read layer
salinity <- rast(salinity_nc_path)

plot(salinity)

# Write as GeoTIFF
writeRaster(salinity, "salinity.tif")

# current velocity ----

# path
cv_nc_path <- "currentvelocity.nc"

#read layer
currentvelocity <- rast(cv_nc_path)

plot(currentvelocity)

# Write as GeoTIFF
writeRaster(currentvelocity, "currentvelocity.tif")

# current direction ----

# path
cd_nc_path <- "currentdirection.nc"

#read layer
currentdirection <- rast(cd_nc_path)

currentdirection

plot(currentdirection)

# Write as GeoTIFF
writeRaster(currentdirection, "currentdirection.tif")

# Nitrate ----

# path
nitrate_path <- "nitrate.nc"

#read layer
nitrate <- rast(nitrate_path)

nitrate

plot(nitrate)

# Write as GeoTIFF
writeRaster(nitrate, "nitrate.tif")

# phosphate ----

# path
phosphate_path <- "phosphate.nc"

#read layer
phosphate <- rast(phosphate_path)

phosphate

plot(phosphate)

# Write as GeoTIFF
writeRaster(phosphate, "phosphate.tif")

# silicate ----

# path
silicate_path <- "silicate.nc"

#read layer
silicate <- rast(silicate_path)

silicate

plot(silicate)

# Write as GeoTIFF
writeRaster(silicate, "silicate.tif")

# dissolvedO2 ----

# path
dissolvedO2_path <- "dissolvedO2.nc"

#read layer
dissolvedO2 <- rast(dissolvedO2_path)

dissolvedO2

plot(dissolvedO2)

# Write as GeoTIFF
writeRaster(dissolvedO2, "dissolvedO2.tif")

# iron ----

# path
iron_path <- "iron.nc"

#read layer
iron <- rast(iron_path)

iron

plot(iron)

# Write as GeoTIFF
writeRaster(iron, "iron.tif")

# primaryprod ----

# path
primaryprod_path <- "primaryprod.nc"

#read layer
primaryprod <- rast(primaryprod_path)

primaryprod

plot(primaryprod)

# Write as GeoTIFF
writeRaster(primaryprod, "primaryprod.tif")

# pH ----

# path
pH_path <- "pH.nc"

#read layer
pH <- rast(pH_path)

pH

plot(pH)

# Write as GeoTIFF
writeRaster(pH, "pH.tif")

# bathymetry ----

# path
bathymetry_path <- "bathymetry.nc"

#read layer
bathymetry <- rast(bathymetry_path)

bathymetry

plot(bathymetry)

# Write as GeoTIFF
writeRaster(bathymetry, "bathymetry.tif")

# slope ----

# path
slope_path <- "slope.nc"

#read layer
slope <- rast(slope_path)

slope

plot(slope)

# Write as GeoTIFF
writeRaster(slope, "slope.tif")

# aspect ----

# path
aspect_path <- "aspect.nc"

#read layer
aspect <- rast(aspect_path)

aspect

plot(aspect)

# Write as GeoTIFF
writeRaster(aspect, "aspect.tif")

# Topographic position index (TPI) ----

# path
TPI_path <- "TPI.nc"

#read layer
TPI <- rast(TPI_path)

TPI

plot(TPI)

# Write as GeoTIFF
writeRaster(TPI, "TPI.tif")

# Topographic ruggedness index (TRI) ----

# path
TRI_path <- "TRI.nc"

#read layer
TRI <- rast(TRI_path)

TRI

plot(TRI)

# Write as GeoTIFF
writeRaster(TRI, "TRI.tif")
