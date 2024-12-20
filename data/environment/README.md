# Environmental Data

## Access
Downloaded from Bio-Oracle on 17/12/2024

Coordinate Reference System uses coordinates in longitude and latitude and is based on the WGS84 datum (+proj=longlat +datum=WGS84 +no_defs) over entire globe (extent = -180, 180, -90, 90) with 0.05 resolution.

Parameters are from the mean depth layer - using the average depth of each 0.05 grid square

Data in .nc files is the average value of:
- Temperature
- Salinity
- Current Velocity
- Current Direction
- Nitrate
- Silicate
- Dissolved O2
- Iron
- Primary Productivity
- pH
- Bathymetry
- Slope
- Aspect
- Topographic Position Index (TPI)
- Topographic Ruggedness index (TRI)

[tif_files.R] converts .nc files to GeoTIFF files - used in sdm course so familiar with this format

## Still to do
- [ ] need to decide on extent based on sponges
- [ ] clip rasters to this extent

  Then I think (??) we're good to go - but definitely double check this...
