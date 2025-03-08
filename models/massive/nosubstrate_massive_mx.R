# massive maxent - no substrate
# massive models - maxent
# 19/2/2025

# 1. Model with background points so we have something
# 2. Try implement ecoPAs

# library ----
library(terra)
library(raster)
library(dismo)
library(rJava)
library(tidyverse)

# data ----
env_vars <- rast("data/environment/NAtl_rasters/present_env_vars.tif")  # predictors
sponge_info <- read.csv("data/environment/NAtl_rasters/red_spongeinfo.csv")  # sponges

sponge_info$morphotype <- as.factor(sponge_info$morphotype)


massive <- sponge_info%>%
  filter(morphotype == "massive")%>%  # massive sponges
  dplyr::select("MiddleLatitude",
                "MiddleLongitude",
                "morphotype",
                "terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

###########################################################################
## 1. BACKGROUND POINT MODELLING ---------------------------------------------

massive_occs <- massive%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # massive presence data

plot(env_vars,1)
points(massive_occs, col = "red")

# split presence data ----
group = kfold(massive_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
massive_train = massive_occs[group != 1, ] # training 4/5
massive_test = massive_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster
ns_env_vars <- dropLayer(env_vars_raster, 6)
ns_env_vars

# presence model----
ns_massive_MX = maxent(ns_env_vars, massive_train) #implement maxent on the presence-only data

plot(ns_massive_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
ns_massive_background = randomPoints(ns_env_vars, n=10000,ext=NAtl_extent, extf = 1)
colnames(ns_massive_background) = c('lon', 'lat')
group = kfold(ns_massive_background, 5)


#pseudo-absences for training model performances
ns_backg_train = ns_massive_background[group != 1, ]
ns_backg_test = ns_massive_background[group == 1, ]

# evaluate model
ns_massive_eval = evaluate(massive_test, ns_backg_test, ns_massive_MX, ns_env_vars)
ns_massive_eval

#ns_massive_eval
#class          : ModelEvaluation 
#n presences    : 470 
#n absences     : 2000 
#AUC            : 0.9399207 
#cor            : 0.7141856 
#max TPR+TNR at : 0.3949634 

plot(ns_massive_eval, 'ROC')

# heat map ----
ns_massive_prediction = predict(ns_env_vars, ns_massive_MX, ext=NAtl_extent, progress='')
# use model xm to predict species presence with stck over given ext
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

par(mfrow=c(1,1))

plot(ns_massive_prediction, main='Present day predicted suitability') # yay!


#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_massive_prediction = predict(SSP2_stack, ns_massive_MX, ext=NAtl_extent, progress='')

plot(SSP2_massive_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_massive_prediction = predict(SSP5_stack, ns_massive_MX, ext=NAtl_extent, progress='')

plot(SSP5_massive_prediction, main = "2040-2050 SSP5 predicted suitability")


###################################################################################
### ECOPAs ---------------------------------------------------------------------

# quick env data tidy
env_vars_raster <- raster::stack(env_vars)  # needs to be raster stack for maxent
env_vars_raster <- dropLayer(env_vars_raster, 6)  # currently not using substrate
env_vars_raster

#massive presences
massive_occs <- massive%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # massive presence data


# split presence data ----
group = kfold(massive_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
massive_train = massive_occs[group != 1, ] # training 4/5
massive_test = massive_occs[group == 1, ] # testing 1/5

# presence model----
massive_MX = maxent(env_vars_raster, massive_train)  # model on presence only
plot(massive_MX) ## variable importance

# pseudoabsences----
massive_pas <- read.csv("data/pseudoabsences/ecoPA_massivepas.csv")
massive_pas <- massive_pas%>%
  select(x,y)

plot(env_vars,1)
points(background_points, col = "blue")
points(massive_pas, col = "red")
points(massive_occs, col = "yellow")


#split for training model performances
pa_train = massive_pas[group != 1, ]
pa_test = massive_pas[group == 1, ]


#evaluate ----
ecoPA_massive_eval = evaluate(massive_test, pa_test, massive_MX, env_vars_raster)
ecoPA_massive_eval

# all PAs outside niche (defined by max and min values for massive)
# ecoPA_massive_eval
# class          : ModelEvaluation 
# n presences    : 470 
# n absences     : 1993 
# AUC            : 0.9034552 
# cor            : 0.6287589 
# max TPR+TNR at : 0.4922526 

response(massive_MX)

plot(ecoPA_massive_eval, 'ROC')


# binary map background model ----
#heat maps
# present
massive_prediction = predict(env_vars_raster, massive_MX, ext=NAtl_extent, progress='')
plot(massive_prediction, main='Present day predicted suitability') 

#SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_massive_prediction = predict(SSP2_stack, massive_MX, ext=NAtl_extent, progress='')
SSP2_massive_prediction

plot(SSP2_massive_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_massive_prediction = predict(SSP5_stack, massive_MX, ext=NAtl_extent, progress='')

plot(SSP5_massive_prediction, main = "2040-2050 SSP5 predicted suitability")

#binary maps based on TPR+TNR threashold
threshold <- 0.4922526

binary_map <- massive_prediction >= threshold
plot(binary_map)

SSP2_binary_map <- SSP2_massive_prediction >= threshold
plot(SSP2_binary_map)

SSP5_binary_map <- SSP5_massive_prediction >= threshold
plot(SSP5_binary_map)  

# write rasters

writeRaster(binary_map, "models/massive/present_binary.tif", format="GTiff", overwrite=TRUE)
writeRaster(SSP2_binary_map, "models/massive/SSP2_binary.tif", format="GTiff", overwrite=TRUE)
writeRaster(SSP5_binary_map, "models/massive/SSP5_binary.tif", format="GTiff", overwrite=TRUE)

##############################################################
# Models with ecoPAs------
ecoPA_model <- maxent(env_vars_raster, massive_train, pa_train)
plot(ecoPA_model)
response(ecoPA_model)

ecoPA_massive_eval = evaluate(massive_test, pa_test, ecoPA_model, env_vars_raster)
ecoPA_massive_eval

#class          : ModelEvaluation 
#n presences    : 470 
#n absences     : 2011 
#AUC            : 0.9645741 
#cor            : 0.8589589 
#max TPR+TNR at : 0.1978522 

plot(ecoPA_massive_eval, 'ROC')

eco_PAmassive_prediction = predict(env_vars_raster, ecoPA_model, ext=NAtl_extent, progress='')
plot(eco_PAmassive_prediction, main='Present day predicted suitability') 

ecoPA_SSP2_massive_prediction = predict(SSP2_stack, ecoPA_model, ext=NAtl_extent, progress='')
plot(ecoPA_SSP2_massive_prediction, main = "ssp2")

ecoPA_SSP5_massive_prediction = predict(SSP5_stack, ecoPA_model, ext=NAtl_extent, progress='')
plot(ecoPA_SSP5_massive_prediction, main = "ssp5")

#binary maps based on TPR+TNR threashold
threshold <- 0.5

ecoPA_binary_map <- massive_prediction >= threshold
plot(binary_map)

SSP2_binary_map <- SSP2_massive_prediction >= threshold
plot(SSP2_binary_map)

SSP5_binary_map <- SSP5_massive_prediction >= threshold
plot(SSP5_binary_map)  
