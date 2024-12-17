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


# presence and absences ----
morphsponge_absences <- morphspongesNA%>%
  filter(Number == 0)

morphsponge_presences <- morphspongesNA%>%
  filter(Number > 0)

morphmap2 <- ggplot(data = world) +
  geom_sf(fill = "lightgrey", color = "gray40", size = 0.3) +
  geom_point(data = morphsponge_presences, 
             aes(x = MiddleLongitude, y = MiddleLatitude, colour = morphotype),
             alpha = 0.4, size = 2) +
  coord_sf(xlim = c(-75, 50), ylim = c(40, 85), expand = FALSE) +
  theme_map()

ggsave("data/sponge/morphmap2.png", plot = morphmap2, width = 10, height = 8, dpi = 300)

# remove years pre 2000 ----
