# pseudoabsence creation

# aim identify presence and absence on each cruise and use as absences

# steps 
# 1. group cruises
# 2. make pres/absence table of morphs on each cruise
# 3. 

# library ----
library(tidyverse)

# sponge data  ----
tidyishsponge <- read.csv("data/sponge/tidyishsponge.csv")

# create factors
tidyishsponge <- tidyishsponge%>%
  mutate(status = as.factor(status),
         HighestTaxonomicResolution = as.factor(HighestTaxonomicResolution),
         Species = as.factor(Species),
         Ship = as.factor(Ship),
         CruiseID = as.factor(CruiseID),
         SurveyMethod = as.factor(SurveyMethod),
         morphotype = as.factor(morphotype))

summary(tidyishsponge$CruiseID)

#create NAs in blanks
tidyishsponge <- tidyishsponge%>%
  mutate(CruiseID = na_if(CruiseID, ""))

# group by cruise ----
cruise_pres_abs <- tidyishsponge %>%
  distinct(CruiseID, morphotype) %>%  # Keep unique cruise-morphotype pairs
  mutate(Presence = 1) %>%  # Assign 1 for presence
  pivot_wider(names_from = morphotype, values_from = Presence, 
              values_fill = list(Presence = 0))%>%  # Convert to wide format
  na.omit(TRUE)

write.csv(cruise_pres_abs, "data/sponge/cruise_pres_abs.csv")

cruise_pres_abs%>%
  mutate(massive = as.factor(massive),
         arborescent = as.factor(arborescent),
         flabellate = as.factor(flabellate),
         caliculate = as.factor(caliculate),
         stipulate = as.factor(stipulate),
         papillate = as.factor(papillate))%>%
  summary()

# massive arborescent flabellate caliculate stipulate papillate
# 0: 7    0:66        0:39       0:65       0:48      0:27     
# 1:65    1: 6        1:33       1: 7       1:24      1:45 

# 0 is absent
# 1 is present


