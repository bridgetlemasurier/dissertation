# caliculate pseudo absences using ecoPA

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

# CALICULATE SPONGES
caliculate <- sponge_info%>%
  filter(morphotype == "caliculate")%>%
  dplyr::select("terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")
caliculate_niche_borders <- summary(caliculate)

# define niche space

caliculate_niche = SpeciesNiche(data = caliculate,
                                bins_sizes = c(10, # TRI dimension will be represented with bins of size 10
                                               0.1, # current v dimension will be represented with bins of size 0.1
                                               10, # O2 dimension will be represented with bins of size 1
                                               1, # Si dimension will be represented with bins of size 1
                                               1), # temp dimension will be represented with bins of size 1
                                niche_border = c(2, 160, # TRI dimension goes from 2 to 160
                                                 0, 0.2, # Current V dimension goes from 0 to 0.2 m/s
                                                 200, 310, # O2 dimension goes from  200 to 310 
                                                 4, 9, # Si dimension goes from 4, 9
                                                 0, 11))  # temp dimension goes from 0 to 11 c

# generate PAs in niche space

caliculatePAs <- PAGeneration(data = caliculate_niche,
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
pseudo_abs_env <- as.data.frame(caliculatePAs[[1]])  # all inside niche space

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

caliculate_pa_xy <- matched_coords

caliculate_occs <- sponge_info%>%
  filter(morphotype == "caliculate")%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)

plot(env_vars,1)
points(caliculate_pa_xy, col = "red")
points(caliculate_occs, col = "yellow")

write.csv(caliculate_pa_xy, "data/pseudoabsences/ecoPA_caliculate_pas.csv")

########## 3) Saving outputs ##########
### Rounded presences

#massive_rounded_pres = massive_niche[[3]]
#colnames(massive_rounded_pres) = massive_niche[[2]]

#write.csv(massive_rounded_pres,
#"Rounded_presences_massive.csv",
#row.names = F)

### Pseudo-absences

#for (i in 1:length(massivePAs)){

#  pseudo_abs_step = massivePAs[[i]]

#  write.csv(pseudo_abs_step,
#            paste0("PA_", names(massivePAs)[i], ".csv"),
#            row.names = F)
#}