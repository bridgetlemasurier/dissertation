# arborescent models - maxent
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


arborescent<- sponge_info%>%
  filter(morphotype == "arborescent")%>%  # arborescent sponges
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

arborescent_occs <- arborescent%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # papillate presence data

plot(env_vars,1)
points(arborescent_occs, col = "red")

# split presence data ----
group = kfold(arborescent_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
presence_train = arborescent_occs[group != 1, ] # training 4/5
presence_test = arborescent_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster <- dropLayer(env_vars_raster, 6)
env_vars_raster

# presence model----
arborescent_MX = maxent(env_vars_raster, presence_train) #implement maxent on the presence-only data

plot(arborescent_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
arborescent_background = randomPoints(env_vars_raster, n=10000,ext=NAtl_extent, extf = 1)
colnames(arborescent_background) = c('lon', 'lat')
group = kfold(arborescent_background, 5)

#pseudo-absences for training model performances
background_train = arborescent_background[group != 1, ]
background_test = arborescent_background[group == 1, ]

# evaluate model
arborescent_eval = evaluate(presence_test, background_test, arborescent_MX, env_vars_raster)
arborescent_eval

#arborescent_eval
#class          : ModelEvaluation 
#n presences    : 22 
#n absences     : 2000 
#AUC            : 0.9832045 
#cor            : 0.4983924 
#max TPR+TNR at : 0.250361  

response(arborescent_MX)
plot(arborescent_eval, 'ROC')

# heat map ----
arborescent_prediction = predict(env_vars_raster, arborescent_MX, ext=NAtl_extent, progress='')
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(arborescent_prediction, main='Present day predicted suitability') # yay!


#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_arborescent_prediction = predict(SSP2_stack, arborescent_MX, ext=NAtl_extent, progress='')

plot(SSP2_arborescent_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_arborescent_prediction = predict(SSP5_stack, arborescent_MX, ext=NAtl_extent, progress='')

plot(SSP5_arborescent_prediction, main = "2040-2050 SSP5 predicted suitability")

###################################################################################
### ECOPAs ---------------------------------------------------------------------

# pseudoabsences----
arborescent_pas <- read.csv("data/pseudoabsences/ecoPA_arborescent_pas.csv")
arborescent_pas <- arborescent_pas%>%
  dplyr::select(x,y)

#split for training model performances
ecopa_train = arborescent_pas[group != 1, ]
ecopa_test = arborescent_pas[group == 1, ]

#evaluate ----
ecoPA_arborescent_eval = evaluate(presence_test, ecopa_test, arborescent_MX, env_vars_raster)
ecoPA_arborescent_eval

#ecoPA_arborescent_eval with correct niche
# class          : ModelEvaluation 
# n presences    : 22 
# n absences     : 2000 
# AUC            : 0.4693182 
# cor            : -0.004034687 
# max TPR+TNR at : 0.2225156 


plot(ecoPA_arborescent_eval, 'ROC') # not great but potentially more realistic
