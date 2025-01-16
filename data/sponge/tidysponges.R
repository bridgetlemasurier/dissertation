# Sponge tidying

# LIBRARY ----
library(tidyverse)
library(maps)
library(ggthemes)
library(rnaturalearth)

#sponge data ----
VMEsponge <- read.csv("data/sponge/VMEsponge.csv") # raw data from ICES

# Sponge "species" ----
tidysponges <- VMEsponge %>%
  filter(Species != "")%>% # filter missing species obs out
  mutate(Species = as.factor(Species),
       HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution))  # as factor

# morphotype variable ----
morphsponges <- tidysponges%>%
  group_by(Species)%>%
  mutate(morphotype = case_when(Species %in% c("Antho (Antho) dichotoma",
                                               "Antho dichotoma",
                                               "Aphrocallistes",
                                               "Aphrocallistes beatrix", 
                                               "Aphrocallistes beatrix beatrix",
                                               "Axinella polypoides",
                                               "Axinella rugosa") ~ "arborescent",
                                Species == "Axinella infundibuliformis" ~ "caliculate",
                                Species %in% c("Phakellia", 
                                               "Phakellia bowerbanki",
                                               "Phakellia robusta",
                                               "Phakellia ventilabrum") ~ "flabellate",
                                Species %in% c("Craniella", 
                                               "Craniella cranium", 
                                               "Craniella longipilis", 
                                               "Craniella polyura", 
                                               "Craniella zetlandica", 
                                               "Geodia", 
                                               "Geodia atlantica", 
                                               "Geodia barretti", 
                                               "Geodia hentscheli", 
                                               "Geodia macandrewii", 
                                               "Geodia megastrella", 
                                               "Geodia parva", 
                                               "Geodia phlegraei",
                                               "Mycale (mycale) lingua",
                                               "Mycale (Mycale) lingua",
                                               "Mycale lingua",
                                               "Stelletta", 
                                               "Stelletta normani", 
                                               "Stelletta rhaphidiophora", 
                                               "Stelletta tuberosa",
                                               "Stryphnus fortis", 
                                               "Stryphnus ponderosus", 
                                               "Suberites", 
                                               "Suberites ficus", 
                                               "Suberites pagurorum", 
                                               "Tethya norvegica", 
                                               "Thenea",
                                               "Thenea levis", 
                                               "Thenea muricata", 
                                               "Thenea valdiviae") ~ "massive",
                                Species %in% c("Polymastia",
                                               "Polymastia andrica",
                                               "Polymastia arctica",
                                               "Polymastia grimaldii",
                                               "Polymastia hemisphaerica",
                                               "Polymastia nivea",
                                               "Polymastia penicillus",
                                               "Polymastia thielei",
                                               "Polymastia uberrima",
                                               "Radiella hemisphaerica",
                                               "Tentorium semisuberites") ~ "papillate",
                                Species %in% c("Pheronema carpenteri",
                                               "Quasillina brevis",
                                               "Stylocordyla borealis") ~ "stipulate"))%>%
  mutate(morphotype = as.factor(morphotype))

morphspongesNA <- morphsponges%>%
  filter(!is.na(morphotype))

# map morphotypes ----
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = morphspongesNA, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-75, 50), ylim = c(40, 85), expand = FALSE) +
  theme_map()

# remove years pre 2000 ----
morphspongesNA <- morphspongesNA%>%
  mutate(ObsYear = as.numeric(str_extract(ObsDate, "\\d{4}")),
         Decade = as.numeric(floor(ObsYear/ 10) * 10))%>%
  filter(ObsYear >=  2000)


# presence and absences ----
morphsponge_absences <- morphspongesNA%>%
  filter(Number == 0)

morphsponge_presences <- morphspongesNA%>%
  filter(Number > 0)

# map morphotype presences and trim extent
morphmap2 <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = morphsponge_presences, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-60, 45), ylim = c(41, 83), expand = FALSE) +
  theme_map()+
  theme(legend.position = "bottom")

morphmap2

#ggsave("data/sponge/morphmap2.png", plot = morphmap2, width = 10, height = 8, dpi = 300)

# presence/absence maps ----

# roughly uk samples
sponges <- morphspongesNA%>%
  mutate(presence = if_else(Number == 0, "absent","present", missing = NA))

ukpresence_map <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = presence),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-20, 10), ylim = c(50, 63), expand = FALSE) +
  theme_map()

# 'GB': ('United Kingdom', (-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085))

ukpresence_map

# arctic circle samples
articpresence_map <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = presence),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-5, 50), ylim = c(65, 85), expand = FALSE) +
  theme_map()

articpresence_map

# check chosen extent contains absences aswell

extpresence_map <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = presence),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-60, 45), ylim = c(41, 83), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom")

extpresence_map

# ggsave("data/sponge/presencemap.png",
#       plot = extpresence_map, width = 10, height = 8, dpi = 300)


# extent to clip rasters to is xlim = c(-60, 45), ylim = c(41, 83)
#  at least for now, may change to look at specific area


# playing with extents

ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = morphsponge_presences, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-20, 10), ylim = c(50, 65), expand = FALSE) +
  theme_map()+
  theme(legend.position = "bottom")

ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = presence),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-20, 10), ylim = c(50, 65), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom")

# massive sponges
massivesponges <- sponges%>%
  filter(morphotype == "massive")

par(mfrow = c(1, 2))

ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = massivesponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-75, 50), ylim = c(40, 85), expand = FALSE) +
  theme_map()+
  theme(legend.position = "bottom")

ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = massivesponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = presence),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-75, 50), ylim = c(40, 85), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom")


## split by morph
morph_facet <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype,
                 shape = presence),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-60, 45), ylim = c(41, 83), expand = FALSE) +
  theme_map()+
  facet_wrap(vars(morphotype)) +
  theme(legend.position = "bottom")

ggsave("data/sponge/morph_facet.png",
       plot = morph_facet, width = 10, height = 8, dpi = 300)
