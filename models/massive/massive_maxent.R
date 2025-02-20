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
                "thetao_mean",
                "cover")

###########################################################################
## BACKGROUND POINT MODELLING ---------------------------------------------

massive_occs <- massive%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # massive presence data

plot(env_vars,1)
points(massive_occs, col = "red")

# split presence data ----
group = kfold(massive_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits
massive_train = massive_occs[group != 1, ] # training 4/5
massive_test = massive_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster

# presence model
massive_MX = maxent(env_vars_raster, massive_train) #implement maxent on the presence-only data

plot(massive_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
massive_background = randomPoints(env_vars_raster, n=10000,ext=NAtl_extent, extf = 1)
colnames(massive_background) = c('lon', 'lat')
group = kfold(massive_background, 5)

#pseudo-absences for training model performances
backg_train = massive_background[group != 1, ]
backg_test = massive_background[group == 1, ]

# evaluate model
massive_eval = evaluate(massive_test, backg_test, massive_MX, env_vars_raster)
massive_eval

#class          : ModelEvaluation 
#n presences    : 386 
#n absences     : 689 
#AUC            : 0.880374 
#cor            : 0.6528521 
#max TPR+TNR at : 0.3604935

# heat map ----
massive_prediction = predict(env_vars_raster, massive_MX, ext=NAtl_extent, progress='')
# use model xm to predict species presence with stck over given ext
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

par(mfrow=c(1,1))

plot(massive_prediction, main='Maxent, raw values') 

# only showing patches where there is data for substrate - not ideal
