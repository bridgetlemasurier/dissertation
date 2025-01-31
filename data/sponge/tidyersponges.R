# Sponge Morphotypes

# aims:
# 1. tidy sponge data
#     - classify morphotypes and filter out NAs
#     - filter years pre 2000
#     - write new csv for analyses

# 2. map sponge morphs
#     - map the different morphotypes
#     - determine extent to clip rasters to


# LIBRARY ----
library(tidyverse)
library(maps)
library(ggthemes)
library(rnaturalearth)

#sponge data ----
VMEsponge <- read.csv("data/sponge/VMEsponge.csv") # raw data from ICES

# Sponge "species" ----
sponges <- VMEsponge %>%
  filter(Species != "")%>% # filter missing species obs out
  mutate(Species = as.factor(Species),
         HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution))  # as factor

# Morphotype variable ----
sponges <- sponges%>%
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
  mutate(morphotype = as.factor(morphotype))  # make factor

sponges <- sponges%>%
  filter(!is.na(morphotype))  # remove NA morphs 

# remove years pre 2000 ----
sponges <- sponges %>%
  mutate(ObsYear = as.numeric(str_extract(ObsDate, "\\d{4}")),  # year of record
         Decade = as.numeric(floor(ObsYear/ 10) * 10))%>%  # decade of record
  filter(ObsYear >=  2000)  # filter those taken before 2000

# write new csv ----
tidyishsponges <- sponges%>%
  dplyr::select(status, HighestTaxonomicResolution, Species, Ship, SurveyMethod,
                MiddleLatitude, MiddleLongitude, morphotype, ObsYear, Decade)
# selected variables needed for analyses

write.csv(tidyishsponges, "data/sponge/tidyishsponge.csv")

# map morphotypes ----
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-75, 50), ylim = c(40, 85), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom")

# find extent covering all records ----
morphmap2 <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-60, 45), ylim = c(41, 83), expand = FALSE) +
  theme_map() +
  theme(legend.position = "bottom")

morphmap2

ggsave("data/sponge/morphmap2.png", plot = morphmap2, width = 10, height = 8, dpi = 300)

# extent xmin = -60
#        xmax = 45
#        ymin = 41
#        ymax = 83
# extent to clip rasters to is xlim = c(-60, 45), ylim = c(41, 83)
#  at least for now, may change to look at specific area

## split by morph ----
morph_labels <- c(
  arborescent = "Arborescent",  #f9776e
  caliculate = "Caliculate",  #b8a001
  massive = "Massive",  #01bb39
  flabellate = "Flabellate",  #01c0c5
  stipulate = "Stipulate",  #66a0ff
  papillate = "Papillate")  #f66ae4


morph_facet <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = sponges, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-60, 45), ylim = c(41, 83), expand = FALSE) +
  theme_map()+
  facet_wrap(vars(morphotype),labeller = labeller(morphotype = morph_labels)) +
  theme(legend.position = "none")

morph_facet

ggsave("data/sponge/morph_facet.png",
       plot = morph_facet, width = 10, height = 8, dpi = 300)


