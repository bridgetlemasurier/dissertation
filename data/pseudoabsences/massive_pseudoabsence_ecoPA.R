# massive pseudo absences using ecoPA

#library
library(tidyverse)
library(terra)
library(EcoPA)

# install ecoPA
#install.packages("devtools")
#devtools::install_github("JosephineBroussin/EcoPA")

# sponge info data frame ----
sponge_info <- read.csv("data/environment/NAtl_rasters/red_spongeinfo.csv")

sponge_info$morphotype <- as.factor(sponge_info$morphotype)

# predictor spat rast ----
env_vars <- rast("data/environment/NAtl_rasters/present_env_vars.tif")

# MASSIVE SPONGES
massive <- sponge_info%>%
  filter(morphotype == "massive")%>%
  dplyr::select("terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

massive_niche_borders <- massive%>%
  summary()


# define niche space

massive_niche = SpeciesNiche(data = massive,
                     bins_sizes = c(10, # TRI dimension will be represented with bins of size 1
                                    0.1, # current v dimension will be represented with bins of size 0.01
                                    10, # O2 dimension will be represented with bins of size 1
                                    1, # Si dimension will be represented with bins of size 1
                                    1), # temp dimension will be represented with bins of size 1
                     niche_border = c(0, 590, # TRI dimension goes from 0 to 590
                                      0, 0.4, # Current V dimension goes from 0 to 0.4 m/s
                                      180, 340, # O2 dimension goes from 180 to 340
                                      3, 18, # Si dimension goes from 1.37, 300
                                      -1, 15))  # temp dimension goes from -1 to 15

# generate PAs in niche space

massivePAs <- PAGeneration(data = massive_niche,
             nb_pa = 10000,
             ratio_pa_in = 0)

## assigning location

# Convert raster to a dataframe with coordinates
env_values <- as.data.frame(env_vars, xy = TRUE, na.rm = TRUE)
env_values <- env_values%>%
  dplyr::select(!cover)

library(FNN)  # Fast nearest neighbor matching

# Select only environmental columns (ignore lat/lon)
env_only <- env_values[, -c(1,2)]

# Select the pseudo-absence environmental values
pseudo_abs_env <- as.data.frame(massivePAs[[1]])  # all outside niche space

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

massive_pa_xy <- matched_coords

massive_pa_xy <- massive_pa_xy%>%
  rename(long = "x",
         lat = "y")

massive_occs <- sponge_info%>%
  filter(morphotype == "massive")%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)

plot(env_vars,1)
points(massive_pa_xy, col = "red")
points(massive_occs, col = "yellow")

write.csv(massive_pa_xy, "data/pseudoabsences/ecoPA_massivepas.csv")



