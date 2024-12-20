# Environmental Data

## Access
Downloaded from Bio-Oracle on 17/12/2024. Bio-ORACLE is released under the GNU General Public License.


Coordinate Reference System uses coordinates in longitude and latitude and is based on the WGS84 datum (+proj=longlat +datum=WGS84 +no_defs) over entire globe (extent = -180, 180, -90, 90) with 0.05 degree resolution.

Parameters are from the mean depth layer - using the average depth of each 0.05 grid square for the period of 2010-2020.

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

## Data citations
v.3.0 Assis, J., Fernández Bejarano, S.J., Salazar, V.W., Schepers, L., Gouvêa, L., Fragkopoulou, E., Leclercq, F., Vanhoorne, B., Tyberghein, L., Serrão, E.A., Verbruggen, H., De Clerck, O. (2024) Bio-ORACLE v3.0. Pushing marine data layers to the CMIP6 Earth system models of climate change research. Global Ecology and Biogeography. DOI: 10.1111/geb.13813

v.1.0 Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F, De Clerck O (2012) Bio-ORACLE: A global environmental dataset for marine species distribution modelling. Global Ecology and Biogeography, 21, 272–281. DOI: 10.1111/j.1466-8238.2011.00656.x
