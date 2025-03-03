# arborescent pseudo absences using ecoPA

#library
library(tidyverse)
library(terra)
library(EcoPA)
library(FNN) 

# install ecoPA
#install.packages("devtools")
#devtools::install_github("JosephineBroussin/EcoPA")

# sponge info data frame ----
sponge_info <- read.csv("data/environment/NAtl_rasters/red_spongeinfo.csv")

sponge_info$morphotype <- as.factor(sponge_info$morphotype)

# predictor spat rast ----
env_vars <- rast("data/environment/NAtl_rasters/present_env_vars.tif")

# ARBORESCENT SPONGES
arborescent <- sponge_info%>%
  filter(morphotype == "arborescent")%>%
  dplyr::select("terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

summary(arborescent)
# define niche space

arborescent_niche = SpeciesNiche(data = arborescent,
                               bins_sizes = c(10, # TRI dimension will be represented with bins of size 10
                                              0.01, # current v dimension will be represented with bins of size 0.1
                                              10, # O2 dimension will be represented with bins of size 1
                                              1, # Si dimension will be represented with bins of size 1
                                              1), # temp dimension will be represented with bins of size 1
                               niche_border = c(2, 350, # TRI dimension goes from 0 to 1265
                                                0, 0.4, # Current V dimension goes from 1.049610e-06 to 1.290776 m/s
                                                180, 270, # O2 dimension goes from 0.229 to 409 
                                                5, 14, # Si dimension goes from 1.37, 300
                                                4, 11))  # temp dimension goes from -2 to 20 coverage

# generate PAs in niche space

arborescentPAs <- PAGeneration(data = arborescent_niche,
                             nb_pa = 10000,
                             ratio_pa_in = 0)

## assigning location

# Convert raster to a dataframe with coordinates
env_values <- as.data.frame(env_vars, xy = TRUE, na.rm = TRUE)
env_values <- env_values%>%
  dplyr::select(!cover)

# Fast nearest neighbor matching

# Select only environmental columns (ignore lat/lon)
env_only <- env_values[, -c(1,2)]

# Select the pseudo-absence environmental values
pseudo_abs_env <- as.data.frame(arborescentPAs[[1]])  # all inside niche space

pseudo_abs_env <- pseudo_abs_env%>%
  dplyr::select(terrain_ruggedness_index,
                sws_mean,
                o2_mean,
                si_mean,
                thetao_mean)


# Find nearest neighbors (matching env conditions to locations)
nn <- get.knnx(env_only, pseudo_abs_env, k = 1)

# Get matching coordinates
matched_coords <- env_values[nn$nn.index, c("x", "y")]

# Combine matched coordinates with pseudo-absence data
pseudo_abs_points <- cbind(matched_coords, presence = 0)

# Check results
head(pseudo_abs_points)

arborescent_pa_xy <- matched_coords

arborescent_occs <- sponge_info%>%
  filter(morphotype == "arborescent")%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)

plot(env_vars,1)
points(arborescent_pa_xy, col = "red")
points(arborescent_occs, col = "yellow")

write.csv(arborescent_pa_xy, "data/pseudoabsences/ecoPA_arborescent_pas.csv")


