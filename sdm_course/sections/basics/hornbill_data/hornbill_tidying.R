# Tidying hornbill data

#LIBRARY
library(readxl)
library(tidyverse)

#DATA
gbif_hornbills2 <- read_excel("sdm_course/sections/basics/gbif_hornbills2.xlsx")

#SELECT COLUMNS
hornbills <- gbif_hornbills2%>%
  select(species, countryCode, decimalLatitude, decimalLongitude)

# SAVE DATA
write.csv(hornbills, "sdm_course/sections/basics/hornbills.csv")