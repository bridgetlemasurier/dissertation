## Sponge Data Exploration

#Library ----
library(tidyverse)

#sponge data ----
VMEsponge <- read.csv("data/sponge/VMEsponge.csv") # raw data from ICES

# What year range are we working with?
tidysponges <- VMEsponge%>%
  mutate(ObsYear = as.numeric(str_extract(ObsDate, "\\d{4}")))

tidysponges%>%
  summarise(earliest = min(ObsYear),
            latest = max(ObsYear))

#working from 1894 - 2023

# How many observations has taxonomic resolution for morphotype

tidysponges <- tidysponges %>%
  filter(Species != "") %>%  # filter missing species obs out
  mutate(Species = as.factor(Species),
         HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution))  # as factor

spongespecies <- tidysponges%>%
  group_by(Species)%>%
  summarise(count = n())

# Save spongespecies as a CSV file
write.csv(spongespecies, "spongespecies.csv", row.names = FALSE)

