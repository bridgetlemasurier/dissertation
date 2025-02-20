# stipulate models - maxent
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


stipulate<- sponge_info%>%
  filter(morphotype == "stipulate")%>%  # stipulate sponges
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

stipulate_occs <- stipulate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # papillate presence data

plot(env_vars,1)
points(stipulate_occs, col = "red")

# split presence data ----
group = kfold(stipulate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
presence_train = stipulate_occs[group != 1, ] # training 4/5
presence_test = stipulate_occs[group == 1, ] # testing 1/5

env_vars_raster <- raster::stack(env_vars)
env_vars_raster <- dropLayer(env_vars_raster, 6)
env_vars_raster

# presence model----
stipulate_MX = maxent(env_vars_raster, presence_train) #implement maxent on the presence-only data

plot(stipulate_MX) ## variable importance

# model vs random ----
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# 10000 background data points
stipulate_background = randomPoints(env_vars_raster, n=10000,ext=NAtl_extent, extf = 1)
colnames(stipulate_background) = c('lon', 'lat')
group = kfold(stipulate_background, 5)

#pseudo-absences for training model performances
background_train = stipulate_background[group != 1, ]
background_test = stipulate_background[group == 1, ]

# evaluate model
stipulate_eval = evaluate(presence_test, background_test, stipulate_MX, env_vars_raster)
stipulate_eval

#stipulate_eval
#class          : ModelEvaluation 
#n presences    : 26 
#n absences     : 2000 
#AUC            : 0.9517308 
#cor            : 0.2788861 
#max TPR+TNR at : 0.4293788  

response(stipulate_MX)
plot(stipulate_eval, 'ROC')

# heat map ----
stipulate_prediction = predict(env_vars_raster, stipulate_MX, ext=NAtl_extent, progress='')
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

plot(stipulate_prediction, main='Present day predicted suitability') # yay!


#projecting -----
# SSP2
SSP2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
SSP2_stack <- raster::stack(SSP2)

SSP2_stipulate_prediction = predict(SSP2_stack, stipulate_MX, ext=NAtl_extent, progress='')

plot(SSP2_stipulate_prediction, main = "2040-2050 SSP2 predicted suitability")

#SSP5
SSP5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
SSP5_stack <- raster::stack(SSP5)

SSP5_stipulate_prediction = predict(SSP5_stack, stipulate_MX, ext=NAtl_extent, progress='')

plot(SSP5_stipulate_prediction, main = "2040-2050 SSP5 predicted suitability")

###################################################################################
### ECOPAs ---------------------------------------------------------------------

# pseudoabsences----
stipulate_pas <- read.csv("data/pseudoabsences/ecoPA_stipulate_pas.csv")
stipulate_pas <- stipulate_pas%>%
  dplyr::select(x,y)

#split for training model performances
ecopa_train = stipulate_pas[group != 1, ]
ecopa_test = stipulate_pas[group == 1, ]

#evaluate ----
ecoPA_stipulate_eval = evaluate(presence_test, ecopa_test, stipulate_MX, env_vars_raster)
ecoPA_stipulate_eval

#ecoPA_stipulate_eval
#class          : ModelEvaluation 
#n presences    : 26 
#n absences     : 2000 
#AUC            : 0.7552692 
#cor            : 0.1065237 
#max TPR+TNR at : 0.4885508 


plot(ecoPA_stipulate_eval, 'ROC') # not great but potentially more realistic