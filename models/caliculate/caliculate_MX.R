# caliculate models - maxent
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


caliculate<- sponge_info%>%
  filter(morphotype == "caliculate")%>%  # caliculate sponges
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

caliculate_occs <- caliculate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # caliculate presence data

plot(env_vars,1)
points(caliculate_occs, col = "red")

# split presence data ----
group = kfold(caliculate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
presence_train = caliculate_occs[group != 1, ] # training 4/5
presence_test = caliculate_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster <- dropLayer(env_vars_raster, 6)
env_vars_raster

# presence model----
caliculate_MX = maxent(env_vars_raster, presence_train) #implement maxent on the presence-only data

plot(caliculate_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
caliculate_background = randomPoints(env_vars_raster, n=10000,ext=NAtl_extent, extf = 1)
colnames(caliculate_background) = c('lon', 'lat')
group = kfold(caliculate_background, 5)

#pseudo-absences for training model performances
background_train = caliculate_background[group != 1, ]
background_test = caliculate_background[group == 1, ]

# evaluate model
caliculate_eval = evaluate(presence_test, background_test, caliculate_MX, env_vars_raster)
caliculate_eval

#caliculate_eval
#class          : ModelEvaluation 
#n presences    : 16 
#n absences     : 2000 
#AUC            : 0.9738438 
#cor            : 0.7340145 
#max TPR+TNR at : 0.00460325  

response(caliculate_MX)
plot(caliculate_eval, 'ROC')

# heat map ----
caliculate_prediction = predict(env_vars_raster, caliculate_MX, ext=NAtl_extent, progress='')
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(caliculate_prediction, main='Present day predicted suitability') # yay!


#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_caliculate_prediction = predict(SSP2_stack, caliculate_MX, ext=NAtl_extent, progress='')

plot(SSP2_caliculate_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_caliculate_prediction = predict(SSP5_stack, caliculate_MX, ext=NAtl_extent, progress='')

plot(SSP5_caliculate_prediction, main = "2040-2050 SSP5 predicted suitability")

###################################################################################
### ECOPAs ---------------------------------------------------------------------

# pseudoabsences----
caliculate_pas <- read.csv("data/pseudoabsences/ecoPA_caliculate_pas.csv")
caliculate_pas <- caliculate_pas%>%
  dplyr::select(x,y)

#split for training model performances
ecopa_train = caliculate_pas[group != 1, ]
ecopa_test = caliculate_pas[group == 1, ]

#evaluate ----
ecoPA_caliculate_eval = evaluate(presence_test, ecopa_test, caliculate_MX, env_vars_raster)
ecoPA_caliculate_eval

#ecoPA_caliculate_eval
#class          : ModelEvaluation 
#n presences    : 16 
#n absences     : 2000 
#AUC            : 0.9402812 
#cor            : 0.8583675 
#max TPR+TNR at : 0.2816299  


plot(ecoPA_caliculate_eval, 'ROC') # not great but potentially more realistic
