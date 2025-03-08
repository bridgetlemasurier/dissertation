# flabellate eco_PA models
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

flabellate <- sponge_info%>%
  filter(morphotype == "flabellate")%>%  # flabellate sponges
  dplyr::select("MiddleLatitude",
                "MiddleLongitude",
                "morphotype",
                "terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

flabellate_occs <- flabellate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # flabellate presence data

# Pseudoabsences
flabellate_pas <- read.csv("data/pseudoabsences/ecoPA_flabellate_pas.csv")
flabellate_pas <- flabellate_pas%>%
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
group = kfold(flabellate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
flabellate_train = flabellate_occs[group != 1, ] # training 4/5
flabellate_test = flabellate_occs[group == 1, ] # testing 1/5

# split pa data
#split for training model performances
group = kfold(flabellate_pas, 5)
pa_train = flabellate_pas[group != 1, ]
pa_test = flabellate_pas[group == 1, ]

# maxent model
ecoPA_flabellate_MX = maxent(env_vars, flabellate_train, pa_train)
ecoPA_flabellate_MX

#variable importance
plot(ecoPA_flabellate_MX) ## variable importance

flabellate_variable_importance <- tibble(
  variable = c("terrain_ruggedness_index", "si_mean", "o2_mean", "thetao_mean", "sws_mean"),
  percent_contribution = c(59.5, 29.6, 5.8, 4.8, 0.3),
  permutation_importance = c(44.7, 44.9, 7.7, 2.6, 0))


### EVALUATE ----
#evaluate ----
ecoPA_flabellate_eval = evaluate(flabellate_test, pa_test, ecoPA_flabellate_MX, env_vars)
ecoPA_flabellate_eval

#class          : ModelEvaluation 
#n presences    : 87 
#n absences     : 2000 
#AUC            : 0.9745546   AUC > 0.85 happy to proceed
#cor            : 0.6411615 
#max TPR+TNR at : 0.1947504   

# need to get table here for sens, spec, AUC and MSS

plot(ecoPA_flabellate_eval, 'ROC') # stunning gorgeous everything in between

## HEAT MAPS ----
# set extent
NAtl_extent <- raster::extent(-60, 45, 41, 83)

# Present Day
current_habitats <- predict(env_vars, ecoPA_flabellate_MX, ext=NAtl_extent, progress='')
plot(current_habitats, main = "Present Day Habitat Suitability")

# 2040-2050

# ssp2
ssp2_habitats <- predict(ssp2, ecoPA_flabellate_MX, ext=NAtl_extent, progress='')
plot(ssp2_habitats, main='ssp2 Habitat Suitability')

# ssp2
ssp5_habitats <- predict(ssp5, ecoPA_flabellate_MX, ext=NAtl_extent, progress='')
plot(ssp5_habitats, main='ssp5 Habitat Suitability')

#binary maps
threashold <- 0.5

present_map <- current_habitats >= threashold
plot(present_map, main = "2010-2020")

ssp2_map <- ssp2_habitats >= threashold
plot(ssp2_map, main = "2040-2050: ssp2")

ssp5_map <- ssp5_habitats >= threashold
plot(ssp5_map, main = "2040-2050: ssp5")  

###############################################################################
# Shift maps ----
present_map <- rast(present_map)
ssp2_map <- rast(ssp2_map)
ssp5_map <- rast(ssp5_map)

# ssp2
# change values of absence for SSP2 
ssp2_map[ssp2_map == 0] <- -1

# create change map
ssp2change <- present_map + ssp2_map
ssp2change <- terra::as.factor(ssp2change)

levels(ssp2change) <- data.frame(
  value = c(-1, 0, 1, 2),
  response = c("Absent", "Loss", "Gain", "Present"))

plot(ssp2change, col = c("lightblue", "red", "darkgreen", "gold"))

# ssp5
# change values of absence for SSP5
ssp5_map[ssp5_map == 0] <- -1

# create change map
ssp5change <- present_map + ssp5_map
ssp5change <- terra::as.factor(ssp5change)

levels(ssp5change) <- data.frame(
  value = c(-1, 0, 1, 2),
  response = c("Absent", "Loss", "Gain", "Present"))

plot(ssp5change, col = c("lightblue", "red", "darkgreen", "gold"))

# Pixels loss gain stable ----

# ssp2
ssp2change_df <- terra::as.data.frame(ssp2change, xy = TRUE, cells = TRUE)


# % of loss, gain and stability
ssp2_percentchange <- ssp2change_df%>%
  summarise(loss = sum(response == "Loss"),
            gain = sum(response == "Gain"),
            stable = sum(response == "Present"),
            potential_pres = loss + gain + stable,
            percent_loss = loss/potential_pres * 100,
            percent_gain = gain/potential_pres * 100,
            percent_stable = stable/potential_pres *100)%>%
  pivot_longer(cols = starts_with("percent"), 
               names_to = "category", 
               values_to = "percent")%>%
  mutate(category = recode(category, 
                           percent_loss = "Loss", 
                           percent_gain = "Gain", 
                           percent_stable = "Stable"),
         scenario = "ssp2",
         morphotype = "flabellate")%>%
  dplyr::select(category, percent, scenario, morphotype)

write.csv(ssp2_percentchange, "models/flabellate/flabellatessp2_percentchange.csv")

ggplot(ssp2_percentchange, aes(x = category, 
                               y = percent, 
                               fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Change from Present Day",
       y = "Percentage of Potential Presence (%)")+
  scale_fill_manual(values = c("Stable" = "gold", 
                               "Loss" = "red", 
                               "Gain" = "darkgreen")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")


# ssp5
ssp5change_df <- terra::as.data.frame(ssp5change, xy = TRUE, cells = TRUE)

# % of loss, gain and stability
ssp5_percentchange <- ssp5change_df%>%
  summarise(loss = sum(response == "Loss"),
            gain = sum(response == "Gain"),
            stable = sum(response == "Present"),
            potential_pres = loss + gain + stable,
            percent_loss = loss/potential_pres * 100,
            percent_gain = gain/potential_pres * 100,
            percent_stable = stable/potential_pres *100)%>%
  pivot_longer(cols = starts_with("percent"), 
               names_to = "category", 
               values_to = "percent")%>%
  mutate(category = recode(category, 
                           percent_loss = "Loss", 
                           percent_gain = "Gain", 
                           percent_stable = "Stable"),
         scenario = "ssp5",
         morphotype = "flabellate")%>%
  dplyr::select(category, percent, scenario, morphotype)

write.csv(ssp5_percentchange, "models/flabellate/flabellatessp5_percentchange.csv")

ggplot(ssp5_percentchange, aes(x = category, 
                               y = percent, 
                               fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Change from Present Day",
       y = "Percentage of Potential Presence (%)")+
  scale_fill_manual(values = c("Stable" = "gold", 
                               "Loss" = "red", 
                               "Gain" = "darkgreen")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")


# Gain, loss and stability per latitude line ----
# ssp2
ssp2lat_summary <- ssp2change_df %>%
  group_by(y) %>%
  summarise(
    loss = sum(response == "Loss"),
    gain = sum(response == "Gain"),
    stable = sum(response == "Present"),
    absent = sum(response == "Absent"),
    total_cells = n(),
    total_present = loss + gain + stable) %>%
  mutate(
    percent_loss = (loss / total_cells) * 100,
    percent_gain = (gain / total_cells) * 100,
    percent_stable = (stable / total_cells) * 100,
    percent_absent = (absent / total_cells) * 100,
    ssp = "ssp2"
  )
# ssp5
ssp5lat_summary <- ssp5change_df %>%
  group_by(y) %>%
  summarise(
    loss = sum(response == "Loss"),
    gain = sum(response == "Gain"),
    stable = sum(response == "Present"),
    absent = sum(response == "Absent"),
    total_cells = n(),
    total_present = loss + gain + stable) %>%
  mutate(
    percent_loss = (loss / total_cells) * 100,
    percent_gain = (gain / total_cells) * 100,
    percent_stable = (stable / total_cells) * 100,
    percent_absent = (absent / total_cells) * 100,
    ssp = "ssp5"
  )

lat_summary <- bind_rows(ssp2lat_summary, ssp5lat_summary)

latitude_shift_plot <- lat_summary%>%
  group_by(ssp)%>%
  ggplot(aes(x = y, y = percent_gain, colour = ssp))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Latitude",
       y = "Increase in Habitat Suitability (%)")

ggsave("models/flabellate/latitudeshift_flabellate.png", latitude_shift_plot,
       width = 8, height = 6, dpi = 300)
