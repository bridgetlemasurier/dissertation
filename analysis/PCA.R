# PCA of sponge data
# 20/01/2024

# Aim: To see if we have morph clusters and what the main drivers are

# library ----
library(tidyverse)
library(raster)
library(terra)
library(factoextra)
library(ade4)

# bio oracle stack ----
setwd("data/environment/NAtl_rasters")

# list of new predictors
bioracle_layers = Sys.glob("*.tif")
bioracle_layers

bioracle_stack <- stack() #empty raster stack for storing raster layers

for(i in 1:NROW(bioracle_layers)){
  tempraster = raster(bioracle_layers[i])  # temporary raster
  bioracle_stack = stack(bioracle_stack,tempraster)
}

bioracle_stack
plot(bioracle_stack)

# reset wd here to project directory

# sponges ----
# for now lets just do presences for the sake of maxentness
tidyishsponge <- read.csv("data/sponge/tidyishsponge.csv")

# we just want presences for now 
sponge_pres <- tidyishsponge%>%
  filter(presence == "present")

# we also dont need extra info - just morph, lat and long
sponge_occs <- sponge_pres%>%
  mutate(lat = MiddleLatitude, long = MiddleLongitude)%>%
  dplyr::select(long, lat)  # needs to be long then lat

NAtl_extent <- ext(-60, 41, 45, 83) # North Atlantic (ish) extent

plot(bioracle_stack,16)
points(sponge_occs, col = "blue")

# extract data at each point of presenence ----
sponge_info <- extract(bioracle_stack, sponge_occs)  # extract info for
                                                     # each point 

# put in data frame of all info for each record
sponge_infodf = data.frame(cbind(sponge_pres, sponge_info))

sponge_infodf <-sponge_infodf%>%
  mutate(status = as.factor(status),
         HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution),
         Species = as.factor(Species),
         Ship = as.factor(Ship),
         SurveyMethod = as.factor(SurveyMethod),
         morphotype = as.factor(morphotype),
         presence = as.factor(presence))

summary(sponge_infodf)
# renaming vars to easier names
sponge_infodf <- sponge_infodf%>%
  rename(latitude = "MiddleLatitude",
    longitude = "MiddleLongitude",
    current_direction = "swd_mean",
    current_velocity = "sws_mean",
    dissolvedO2 = "o2_mean",
    iron = "dfe_mean",
    nitrate = "no3_mean",
    pH = "ph_mean",
    phosphate = "po4_mean",
    primaryprod = "phyc_mean",
    salinity = "so_mean",
    silicate = "si_mean",
    temperature = "thetao_mean")

summary(sponge_infodf)

# PCA time ----
spongePCA_NA <- sponge_infodf%>%
  na.omit(TRUE)
sponge_PCA <- dudi.pca(spongePCA_NA[,c(13:28)],
                       center = TRUE,
                       scale = TRUE,
                       scannf = FALSE,
                       nf = 2)
fviz_pca(sponge_PCA, habillage = spongePCA_NA$morphotype, col.var = "black", label = "var")+
  labs(title = "") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# ok slay that will do 
# seems to be some clustering but not very distinct but will try again with a
# more refined set of variables

