## Sponge Data Exploration

#Library ----
library(tidyverse)
library(maps)
library(ggthemes)
library(rnaturalearth)

#sponge data ----
VMEsponge <- read.csv("data/sponge/VMEsponge.csv") # raw data from ICES

# What year range are we working with?----
tidysponges <- VMEsponge%>%
  mutate(ObsYear = as.numeric(str_extract(ObsDate, "\\d{4}")))

tidysponges%>%
  summarise(earliest = min(ObsYear),
            latest = max(ObsYear))

#working from 1894 - 2023
hist(tidysponges$ObsYear)  # most are for 2000-2023

tidysponges$decade <- (floor(tidysponges$ObsYear/ 10) * 10)

decade_counts <- tidysponges %>%
  group_by(decade) %>%
  summarise(count = n())
print(decade_counts)

# How much data do I have? ----
count(tidysponges)

# How many observations has taxonomic resolution for morphotype ----

tidysponges <- tidysponges %>%
  filter(Species != "") %>%  # filter missing species obs out
  mutate(Species = as.factor(Species),
         HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution))  # as factor

spongespecies <- tidysponges%>%
  group_by(Species)%>%
  summarise(count = n())

# Save spongespecies as a CSV file
# write.csv(spongespecies, "spongespecies.csv", row.names = FALSE)

# Map existing records? ----

# Load the world map from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the map
spongemap <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = tidysponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude),
             color = "red", alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-75, 50), ylim = c(30, 85), expand = FALSE) +
  theme_map() 
spongemap

# Save the map as a PNG file
ggsave("sponge_map.png", plot = map, width = 10, height = 8, dpi = 300)


