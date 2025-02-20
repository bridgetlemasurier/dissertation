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

response(ns_massive_MX)

# heat map ----
ns_massive_prediction = predict(ns_env_vars, ns_massive_MX, ext=NAtl_extent, progress='')
# use model xm to predict species presence with stck over given ext
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

par(mfrow=c(1,1))

plot(ns_massive_prediction, main='Present day predicted suitability') # yay!

plot(ns_massive_eval, 'ROC')

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


plot(env_vars,1)
points(massive_occs, col = "red")

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

#split for training model performances
pa_train = massive_pas[group != 1, ]
pa_test = massive_pas[group == 1, ]

#evaluate ----
ecoPA_massive_eval = evaluate(massive_test, pa_test, massive_MX, env_vars_raster)
ecoPA_massive_eval

#class          : ModelEvaluation 
#n presences    : 470 
#n absences     : 1998 
#AUC            : 0.9324963 
#cor            : 0.7390623 
#max TPR+TNR at : 0.4459068 

response(massive_MX)

plot(ecoPA_massive_eval, 'ROC')

#heat map ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

massive_prediction = predict(env_vars_raster, massive_MX, ext=NAtl_extent, progress='')
# use model xm to predict species presence with stck over given ext
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(massive_prediction, main='Present day (EcoPA)') # yay!

#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

ep_SSP2_massive_prediction = predict(SSP2_stack, massive_MX, ext=NAtl_extent, progress='')

plot(ep_SSP2_massive_prediction, main = "2040-2050 SSP2 (EcoPA)")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

ep_SSP5_massive_prediction = predict(SSP5_stack, massive_MX, ext=NAtl_extent, progress='')

plot(ep_SSP5_massive_prediction, main = "2040-2050 SSP5 (EcoPA)")



