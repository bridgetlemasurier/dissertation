###################################################################
############# Bioclim- Climate envelope model
##### uses presence only data 

## Climate change is creating new challenges for biodiversity conservation.
##Climate envelope models describe the climate where a species 
#currently lives (its climate "envelope")
## map the geographic shift of that envelope under climate change

library(dismo)
library(terra) #rgeos package retired
library(sf)
library(raster)
library(geodata)

horn <- read.csv("sdm_course/sections/data/ready_rasters/hornbill_my1.csv")
head(horn)
horn1 <- horn[,-1] #first column not needed

ext = extent(99, 105, 1.2, 6.7) #geographic extent of Peninsular M'Sia
# longitude 1, longitude 2, latitude 1, latitude 2

all.worldclim <- worldclim_global(var = 'bio', res = 10, path = "sdm_course/sections/basics/classics")
msia.worldclim = crop(all.worldclim, extent(99, 105, 1.2, 6.7)) # spatraster

## set up the bounding box of your map
#h.extent <- extent(min(horn1$long -1),
#                  max(horn1$long + 1),
 #                 min(horn1$lat - 1),
  #                max(horn1$lat + 1)) # extent

# Determine geographic extent of our data
max_lat <- ceiling(max(horn1$lat+1))
min_lat <- floor(min(horn1$lat-1))
max_lon <- ceiling(max(horn1$long+1))
min_lon <- floor(min(horn1$long-1))
# Store boundaries in a single extent object
h.extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

# Download data with geodata's world function to use for our base map
world_map <- world(resolution = 3,
                   path = "sdm_course/sections/basics/classics")
# Crop the map to our area of interest
my_map <- crop(x = world_map, y = h.extent)
# Plot the base map
plot(my_map,
     axes = TRUE,
     col = "grey95")
# Add the points for individual observations
points(x = horn1$long,
       y = horn1$lat,
       col = "olivedrab",
       pch = 20,
       cex = 0.75)

# Plot the first of the bioclim variables to check on cropping
plot(msia.worldclim[[1]])

## Use the bioclim function, which takes your climate layers and the long and lat columns (in that order)
# Convert SpatRaster to RasterLayer if needed
msia.worldclim_stack <- raster::stack(msia.worldclim)  # Retains all layers  # Keep as SpatRaster if you need to use it with terra

h.bc <- bioclim(msia.worldclim_stack, horn1[,c('long','lat')])

# Adjust margins (if needed)
par(mar = c(5, 4, 2, 2))  # Set margins (bottom, left, top, right)

# Plot the response
response(h.bc)

horn.d <- bioclim(msia.worldclim_stack, horn1[,c('long','lat')])
response(horn.d)

horn.d.pred <- predict(object = horn.d, msia.worldclim_stack)
plot(horn.d.pred, main = 'sdm predictions using climate layers')
# higher = greater chance of finding species based on climate

#### evaluate model performance----

#### background data (pseudo-absences) needed for this
      # we don't know exactly where absences are
      # so we must create matrix of pseudo absences

#### evaluation determines if the model can differentiate bw the habitat & the background

#species presence

head(horn1)

plot(msia.worldclim) #predictors

# extract random points from predictor rastors

backg <- randomPoints(msia.worldclim_stack, n=1000,ext=ext, extf = 1.25)
#background/pseudo-absence data - random 
# can generate ones based on ecology - later
# only generated 681 points - why?

e = evaluate(horn1, backg, horn.d, msia.worldclim)
#presence, background, model, predictors 
# 4 arguements which evaluate function needs

e
  #class          : ModelEvaluation 
  #n presences    : 944 
  #n absences     : 681 
  #AUC            : 0.8510921  <- most important metric for eval
  #cor            : 0.5007327 
  #max TPR+TNR at : 0.06875593 

plot(e, 'ROC')

# AUC < 0.5 = shite - model is no better than guess
# AUC = 0.85 means bioclim model does a good job of identifying the
# climate envelope of the species