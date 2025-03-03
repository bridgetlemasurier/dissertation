# papillate models - maxent
# 20/2/2025

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


papillate <- sponge_info%>%
  filter(morphotype == "papillate")%>%  # papillate sponges
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

papillate_occs <- papillate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # papillate presence data

plot(env_vars,1)
points(papillate_occs, col = "red")

# split presence data ----
group = kfold(papillate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
presence_train = papillate_occs[group != 1, ] # training 4/5
presence_test = papillate_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster <- dropLayer(env_vars_raster, 6)
env_vars_raster

# presence model----
papillate_MX = maxent(env_vars_raster, presence_train) #implement maxent on the presence-only data

plot(papillate_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
papillate_background = randomPoints(env_vars_raster, n=10000,ext=NAtl_extent, extf = 1)
colnames(papillate_background) = c('lon', 'lat')
group = kfold(papillate_background, 5)

#pseudo-absences for training model performances
background_train = papillate_background[group != 1, ]
background_test = papillate_background[group == 1, ]

# evaluate model
papillate_eval = evaluate(presence_test, background_test, papillate_MX, env_vars_raster)
papillate_eval

#papillate_eval
#class          : ModelEvaluation 
#n presences    : 246 
#n absences     : 2000 
#AUC            : 0.9585732 
#cor            : 0.6992586 
#max TPR+TNR at : 0.2470638 

response(papillate_MX)
plot(papillate_eval, 'ROC')

# heat map ----
papillate_prediction = predict(env_vars_raster, papillate_MX, ext=NAtl_extent, progress='')
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(papillate_prediction, main='Present day predicted suitability') # yay!


#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_papillate_prediction = predict(SSP2_stack, papillate_MX, ext=NAtl_extent, progress='')

plot(SSP2_papillate_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_papillate_prediction = predict(SSP5_stack, papillate_MX, ext=NAtl_extent, progress='')

plot(SSP5_papillate_prediction, main = "2040-2050 SSP5 predicted suitability")

###################################################################################
### ECOPAs ---------------------------------------------------------------------

# pseudoabsences----
papillate_pas <- read.csv("data/pseudoabsences/ecoPA_papillate_pas.csv")
papillate_pas <- papillate_pas%>%
  dplyr::select(x,y)

#split for training model performances
ecopa_train = papillate_pas[group != 1, ]
ecopa_test = papillate_pas[group == 1, ]

#evaluate ----
ecoPA_papillate_eval = evaluate(presence_test, ecopa_test, papillate_MX, env_vars_raster)
ecoPA_papillate_eval

#class          : ModelEvaluation 
#n presences    : 246 
#n absences     : 2000 
#AUC            : 0.9530041 
#cor            : 0.640612 
#max TPR+TNR at : 0.4698666 

#correct defined niche
#ecoPA_papillate_eval
#class          : ModelEvaluation 
#n presences    : 246 
#n absences     : 2000 
#AUC            : 0.9541463 
#cor            : 0.6527866 
#max TPR+TNR at : 0.4388237 

response(papillate_MX)

plot(ecoPA_papillate_eval, 'ROC')

#heat map ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

papillate_prediction = predict(env_vars_raster, papillate_MX, ext=NAtl_extent, progress='')
# use model xm to predict species presence with stck over given ext
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(papillate_prediction, main='Present day (EcoPA)') # yay!

#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

ep_SSP2_papillate_prediction = predict(SSP2_stack, papillate_MX, ext=NAtl_extent, progress='')

plot(ep_SSP2_papillate_prediction, main = "2040-2050 SSP2 (EcoPA)")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

ep_SSP5_papillate_prediction = predict(SSP5_stack, papillate_MX, ext=NAtl_extent, progress='')

plot(ep_SSP5_papillate_prediction, main = "2040-2050 SSP5 (EcoPA)")



