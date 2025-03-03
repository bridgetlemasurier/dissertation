# flabellate pseudo absences using ecoPA

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

# FLABELLATE SPONGES
flabellate <- sponge_info%>%
  filter(morphotype == "flabellate")%>%
  dplyr::select("terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

summary(flabellate)


# define niche space

flabellate_niche = SpeciesNiche(data = flabellate,
                                 bins_sizes = c(10, # TRI dimension will be represented with bins of size 10
                                                0.01, # current v dimension will be represented with bins of size 0.1
                                                10, # O2 dimension will be represented with bins of size 1
                                                1, # Si dimension will be represented with bins of size 1
                                                1), # temp dimension will be represented with bins of size 1
                                 niche_border = c(0, 610, # TRI dimension goes from 0 to 610
                                                  0, 0.2, # Current V dimension goes from 0 to 0.2 m/s
                                                180, 310, # O2 dimension goes from 184 to 306 
                                                  4, 12, # Si dimension goes from 4 to 12
                                                  1, 14))  # temp dimension goes from 1 to 14 c

# generate PAs in niche space

flabellatePAs <- PAGeneration(data = flabellate_niche,
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
pseudo_abs_env <- as.data.frame(flabellatePAs[[1]])  # all inside niche space

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

flabellate_pa_xy <- matched_coords

flabellate_occs <- sponge_info%>%
  filter(morphotype == "flabellate")%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)

plot(env_vars,1)
points(flabellate_pa_xy, col = "red")
points(flabellate_occs, col = "yellow")

write.csv(flabellate_pa_xy, "data/pseudoabsences/ecoPA_flabellate_pas.csv")

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

