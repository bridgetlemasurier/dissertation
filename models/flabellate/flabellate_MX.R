# flabellate models - maxent
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


flabellate<- sponge_info%>%
  filter(morphotype == "flabellate")%>%  # arborescent sponges
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

flabellate_occs <- flabellate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # papillate presence data

plot(env_vars,1)
points(flabellate_occs, col = "red")

# split presence data ----
group = kfold(flabellate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
presence_train = flabellate_occs[group != 1, ] # training 4/5
presence_test = flabellate_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster <- dropLayer(env_vars_raster, 6)
env_vars_raster

# presence model----
flabellate_MX = maxent(env_vars_raster, presence_train) #implement maxent on the presence-only data

plot(flabellate_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
flabellate_background = randomPoints(env_vars_raster, n=10000,ext=NAtl_extent, extf = 1)
colnames(flabellate_background) = c('lon', 'lat')
group = kfold(flabellate_background, 5)

#pseudo-absences for training model performances
background_train = flabellate_background[group != 1, ]
background_test = flabellate_background[group == 1, ]

# evaluate model
flabellate_eval = evaluate(presence_test, background_test, flabellate_MX, env_vars_raster)
flabellate_eval

#flabellate_eval
#class          : ModelEvaluation 
#n presences    : 87 
#n absences     : 2000 
#AUC            : 0.9658103 
#cor            : 0.6021012 
#max TPR+TNR at : 0.096533 

response(flabellate_MX)
plot(flabellate_eval, 'ROC')

# heat map ----
flabellate_prediction = predict(env_vars_raster, flabellate_MX, ext=NAtl_extent, progress='')
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(flabellate_prediction, main='Present day predicted suitability') # yay!


#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_flabellate_prediction = predict(SSP2_stack, flabellate_MX, ext=NAtl_extent, progress='')

plot(SSP2_flabellate_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_flabellate_prediction = predict(SSP5_stack, flabellate_MX, ext=NAtl_extent, progress='')

plot(SSP5_flabellate_prediction, main = "2040-2050 SSP5 predicted suitability")

###################################################################################
### ECOPAs ---------------------------------------------------------------------

# pseudoabsences----
flabellate_pas <- read.csv("data/pseudoabsences/ecoPA_flabellate_pas.csv")
flabellate_pas <- flabellate_pas%>%
  dplyr::select(x,y)

#split for training model performances
ecopa_train = flabellate_pas[group != 1, ]
ecopa_test = flabellate_pas[group == 1, ]

#evaluate ----
ecoPA_flabellate_eval = evaluate(presence_test, ecopa_test, flabellate_MX, env_vars_raster)
ecoPA_flabellate_eval

#ecoPA_flabellate_eval with correct niche
#class          : ModelEvaluation 
#n presences    : 87 
#n absences     : 2000 
#AUC            : 0.8496552 
#cor            : 0.2794112 
#max TPR+TNR at : 0.3095905


plot(ecoPA_flabellate_eval, 'ROC') # not great but potentially more realistic
