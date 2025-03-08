# arborescent eco_PA models
# 8/3/2025

# Aims
# 1. Encorporate EcoPAs into model
# 2. Evaluate Model
# 3. Project Model if AUC>0.85
# 4. Create Binary maps

################################################################################

# LIBRARY ----
library(tidyverse)
library(raster)
library(terra)
library(dismo)
library(rJava)

# DATA ----
# Oceanic conditions
env_vars <- rast("data/environment/NAtl_rasters/present_env_vars.tif")
env_vars <- raster::stack(env_vars) # predictors
env_vars <- dropLayer(env_vars, 6)  # currently not using substrate
env_vars  # slay

# Sponges 
sponge_info <- read.csv("data/environment/NAtl_rasters/red_spongeinfo.csv")  # sponges

sponge_info$morphotype <- as.factor(sponge_info$morphotype)

arborescent <- sponge_info%>%
  filter(morphotype == "arborescent")%>%  # flabellate sponges
  dplyr::select("MiddleLatitude",
                "MiddleLongitude",
                "morphotype",
                "terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

arborescent_occs <- arborescent%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # arborescent presence data

# Pseudoabsences
arborescent_pas <- read.csv("data/pseudoabsences/ecoPA_arborescent_pas.csv")
arborescent_pas <- arborescent_pas%>%
  dplyr::select(x,y)

# SSP2
ssp2 <- rast("data/environment/ssp45/SSP2_predictors4050.tif")
ssp2 <- raster::stack(ssp2)

# SSP5
ssp5 <- rast("data/environment/ssp85/SSP5_predictors4050.tif")
ssp5 <- raster::stack(ssp5)

################################################################################
# MODEL ----
# split presence data 
group = kfold(arborescent_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
arborescent_train = arborescent_occs[group != 1, ] # training 4/5
arborescent_test = arborescent_occs[group == 1, ] # testing 1/5

# split pa data
#split for training model performances
group = kfold(arborescent_pas, 5)
pa_train = arborescent_pas[group != 1, ]
pa_test = arborescent_pas[group == 1, ]

# maxent model
ecoPA_arborescent_MX = maxent(env_vars, arborescent_train, pa_train)
ecoPA_arborescent_MX

#variable importance
plot(ecoPA_arborescent_MX) ## variable importance

arborescent_variable_importance <- tibble(
  variable = c("si_mean", "terrain_ruggedness_index", "thetao_mean", "o2_mean", "sws_mean"),
  percent_contribution = c(34.2, 32.3, 16.4, 15.7, 1.4),
  permutation_importance = c(14, 24.1, 27.6, 29.5, 4.9))


### EVALUATE ----
#evaluate ----
ecoPA_arborescent_eval = evaluate(arborescent_test, pa_test, ecoPA_arborescent_MX, env_vars)
ecoPA_arborescent_eval

#class          : ModelEvaluation 
#n presences    : 22 
#n absences     : 2000 
#AUC            : 0.6079091  # AUC <0.85 no further !
#cor            : 0.03782109 
#max TPR+TNR at : 0.161122   

# need to get table here for sens, spec, AUC and MSS

plot(ecoPA_arborescent_eval, 'ROC') # a bit all over the place
##########################################################################################