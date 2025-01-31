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

# add substrate to stack ----
# load in raster 
folk5_1M <- raster("data/environment/substrate/qgis_rasters/5folk_1M.tif")
plot(folk5_1M)

predictor_stack <- addLayer(bioracle_stack, folk5_1M)

summary(predictor_stack)
plot(predictor_stack,17)

# sponges ----
# import tidyish sponges
tidyishsponge <- read.csv("data/sponge/tidyishsponge.csv")

#dont need extra info - just lat and long for occurences
sponge_occs <- tidyishsponge%>%
  mutate(lat = MiddleLatitude, long = MiddleLongitude)%>%
  dplyr::select(long, lat)  # needs to be long then lat

NAtl_extent <- ext(-60, 41, 45, 83) # North Atlantic (ish) extent

plot(bioracle_stack,16)
points(sponge_occs, col = "blue")

# extract data at each point of presenence ----
sponge_info <- extract(predictor_stack, sponge_occs)  # extract info for
                                                     # each point 

# put in data frame of all info for each record
sponge_infodf = data.frame(cbind(tidyishsponge, sponge_info))


sponge_infodf <-sponge_infodf%>%
  mutate(status = as.factor(status),
         HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution),
         Species = as.factor(Species),
         Ship = as.factor(Ship),
         SurveyMethod = as.factor(SurveyMethod),
         morphotype = as.factor(morphotype),
         X5folk_1M = as.factor(X5folk_1M))

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
    temperature = "thetao_mean",
    substrate = "X5folk_1M")

summary(sponge_infodf)

summary(sponge_infodf$morphotype)

# PCA time ----
# need to filter NAs for PCA
spongePCA_NA <- sponge_infodf%>%
  na.omit(TRUE)

sponge_PCA <- dudi.pca(spongePCA_NA[,c(12:27)],  # cant do factors so no substrate
                       center = TRUE,
                       scale = TRUE,
                       scannf = FALSE,
                       nf = 2)
sponge_biplot <- fviz_pca(sponge_PCA, habillage = spongePCA_NA$morphotype, col.var = "black", label = "var")+
  scale_color_manual(values = c("arborescent" = "#f9776e", 
                                "caliculate" = "#b8a001", 
                                "flabellate" = "#01c0c5", 
                                "massive" = "#01bb39",
                                "papillate" = "#f66ae4",
                                "stipulate" = "#66a0ff")) + # Custom colors for morphotypes
  scale_shape_manual(values = c("arborescent" = 16, 
                                "caliculate" = 16, 
                                "flabellate" = 16, 
                                "massive" = 16,
                                "papillate" = 16,
                                "stipulate" = 16)) +
  labs(title = "") +
  theme_minimal() +
  theme(panel.grid = element_blank())

sponge_biplot

ggsave("analysis/spongePCA2.png",
       plot = sponge_biplot, width = 10, height = 8, dpi = 300)
# ok slay that will do 
# seems to be some clustering but not very distinct but will try again with a
# more refined set of variables


