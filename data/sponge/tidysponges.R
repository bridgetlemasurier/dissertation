# Sponge tidying

# LIBRARY ----
library(tidyverse)

#sponge data ----
VMEsponge <- read.csv("data/sponge/VMEsponge.csv") # raw data from ICES

# Sponge "species"
tidysponges <- VMEsponge %>%
  filter(Species != "")%>% # filter missing species obs out
  mutate(Species = as.factor(Species),
       HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution))  # as factor
