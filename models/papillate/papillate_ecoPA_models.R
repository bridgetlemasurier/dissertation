# papillate eco_PA models
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
library(viridis)

# DATA ----
# Oceanic conditions
env_vars <- rast("data/environment/NAtl_rasters/present_env_vars.tif")
env_vars <- raster::stack(env_vars) # predictors
env_vars <- dropLayer(env_vars, 6)  # currently not using substrate
env_vars  # slay

# Sponges 
sponge_info <- read.csv("data/environment/NAtl_rasters/red_spongeinfo.csv")  # sponges

sponge_info$morphotype <- as.factor(sponge_info$morphotype)

papillate <- sponge_info%>%
  filter(morphotype == "papillate")%>%  # papillate sponges
  dplyr::select("MiddleLatitude",
                "MiddleLongitude",
                "morphotype",
                "terrain_ruggedness_index",
                "sws_mean",
                "o2_mean",
                "si_mean",                   
                "thetao_mean")

papillate_occs <- papillate%>%
  dplyr::select(MiddleLongitude, MiddleLatitude)  # papillate presence data

# Pseudoabsences
papillate_pas <- read.csv("data/pseudoabsences/ecoPA_papillate_pas.csv")
papillate_pas <- papillate_pas%>%
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
group = kfold(papillate_occs, 5) #split the data into 5 portions

#build and test models on all 5 data splits ----
papillate_train = papillate_occs[group != 1, ] # training 4/5
papillate_test = papillate_occs[group == 1, ] # testing 1/5

# split pa data
#split for training model performances
group = kfold(papillate_pas, 5)
pa_train = papillate_pas[group != 1, ]
pa_test = papillate_pas[group == 1, ]

# maxent model
ecoPA_papillate_MX = maxent(env_vars, papillate_train, pa_train)
ecoPA_papillate_MX

#variable importance
plot(ecoPA_papillate_MX) ## variable importance

papillate_variable_importance <- tibble(
  variable = c("terrain_ruggedness_index", "o2_mean", "si_mean", "thetao_mean", "sws_mean"),
  percent_contribution = c(65.3, 24.6, 5.7, 4.2, 0.2),
  permutation_importance = c(38.6, 12.2, 2.1, 46.3, 0.8))


### EVALUATE ----
#evaluate ----
ecoPA_papillate_eval = evaluate(papillate_test, pa_test, ecoPA_papillate_MX, env_vars)
ecoPA_papillate_eval

#class          : ModelEvaluation 
#n presences    : 246 
#n absences     : 2000 
#AUC            : 0.9875793   AUC > 0.85
#cor            : 0.9037509 
#max TPR+TNR at : 0.1716984  

# need to get table here for sens, spec, AUC and MSS

plot(ecoPA_papillate_eval, 'ROC') # stunning gorgeous everything in between

## HEAT MAPS ----
# set extent
NAtl_extent <- raster::extent(-60, 45, 41, 83)
x_limits <- c(xmin(NAtl_extent), xmax(NAtl_extent))
y_limits <- c(ymin(NAtl_extent), ymax(NAtl_extent))

# Present Day
current_habitats <- predict(env_vars, ecoPA_papillate_MX, ext=NAtl_extent, progress='')
current_habitats_terra <- rast(current_habitats)
plot(current_habitats_terra,col = viridis(100), legend = FALSE,
     xlim = x_limits, ylim = y_limits)

# 2040-2050

# ssp2
ssp2_habitats <- predict(ssp2, ecoPA_papillate_MX, ext=NAtl_extent, progress='')
ssp2_habitats_terra <- rast(ssp2_habitats)
plot(ssp2_habitats_terra, col = viridis(100), legend = FALSE,
     xlim = x_limits, ylim = y_limits)


# ssp5
ssp5_habitats <- predict(ssp5, ecoPA_papillate_MX, ext=NAtl_extent, progress='')
ssp5_habitats_terra <- rast(ssp5_habitats)
plot(ssp5_habitats_terra, col = viridis(100), legend = TRUE,
     xlim = x_limits, ylim = y_limits)


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

plot(ssp2change, col = c("#0A00F92F", "#9A0000FF","#0AB4A9F9", "#FABA39FF"))

# ssp5
# change values of absence for SSP5
ssp5_map[ssp5_map == 0] <- -1

# create change map
ssp5change <- present_map + ssp5_map
ssp5change <- terra::as.factor(ssp5change)

levels(ssp5change) <- data.frame(
  value = c(-1, 0, 1, 2),
  response = c("Absent", "Loss", "Gain", "Present"))

plot(ssp5change, col = c("#0A00F92F", "#9A0000FF","#0AB4A9F9", "#FABA39FF"))

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
         morphotype = "papillate")%>%
  dplyr::select(category, percent, scenario, morphotype)

write.csv(ssp2_percentchange, "models/papillate/papillatessp2_percentchange.csv")

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
         morphotype = "papillate")%>%
  dplyr::select(category, percent, scenario, morphotype)

write.csv(ssp5_percentchange, "models/papillate/papillatessp5_percentchange.csv")

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
  mutate(ssp = as.factor(ssp),
         ssp = toupper(ssp))%>%
  group_by(ssp)%>%
  ggplot(aes(x = y, y = percent_gain, colour = ssp))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm")+
  labs(x = "Latitude (°)",
       y = "Increase in Habitat Suitability (%)",
       colour = "Climate Change
     Scenario")+
  scale_colour_manual(values = c("SSP2" = "#00BFC4" ,
                                 "SSP5" = "#F8766D")) +
  theme_bw() +
  theme(panel.grid = element_blank())


ggsave("models/papillate/latitudeshift_papillate.png", latitude_shift_plot,
       width = 8, height = 6, dpi = 300)
