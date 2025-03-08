# stipitate eco_PA models
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

stipitate <- sponge_info%>%
  filter(morphotype == "stipulate")%>%  # stipitate sponges (i spelt it wrong)
  dplyr::select("MiddleLatitude",
                "MiddleLongitude",
                "morphotype",
                "terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

stipitate_occs <- stipitate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # stipitate presence data

# Pseudoabsences
stipitate_pas <- read.csv("data/pseudoabsences/ecoPA_stipulate_pas.csv")
stipitate_pas <- stipitate_pas%>%
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
group = kfold(stipitate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
stipitate_train = stipitate_occs[group != 1, ] # training 4/5
stipitate_test = stipitate_occs[group == 1, ] # testing 1/5

# split pa data
#split for training model performances
group = kfold(stipitate_pas, 5)
pa_train = stipitate_pas[group != 1, ]
pa_test = stipitate_pas[group == 1, ]

# maxent model
ecoPA_stipitate_MX = maxent(env_vars, stipitate_train, pa_train)
ecoPA_stipitate_MX

#variable importance
plot(ecoPA_stipitate_MX) ## variable importance

stipitate_variable_importance <- tibble(
  variable = c("terrain_ruggedness_index", "si_mean", "thetao_mean", "o2_mean", "sws_mean"),
  percent_contribution = c(57.3, 19.6, 13.2, 6.3, 3.5),
  permutation_importance = c(46.3, 25.2, 12.6, 14.6, 1.3))
  


### EVALUATE ----
#evaluate ----
ecoPA_stipitate_eval = evaluate(stipitate_test, pa_test, ecoPA_stipitate_MX, env_vars)
ecoPA_stipitate_eval

#ecoPA_stipitate_eval
#class          : ModelEvaluation 
#n presences    : 26 
#n absences     : 2000 
#AUC            : 0.5282404  no further!
#cor            : 0.1292741 
#max TPR+TNR at : 0.7696065 

# need to get table here for sens, spec, AUC and MSS

plot(ecoPA_stipitate_eval, 'ROC') # not hot cute or pretty


######################################################################################################
