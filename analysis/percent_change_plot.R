# bar plot of shift responses
# 9/3/2024

#Aim
# To combine percentage responses from each modelled morph into 1 bar chart

# LIBRARY
library(tidyverse)
library(viridis)
library(kableExtra)

# For colorblind-friendly palettes


# DATA ----
# ssp2
massivessp2 <- read.csv("models/massive/massivessp2_percentchange.csv")
caliculatessp2<- read.csv("models/caliculate/caliculatessp2_percentchange.csv")
flabellatessp2 <- read.csv("models/flabellate/flabellatessp2_percentchange.csv")
papillatessp2 <- read.csv("models/papillate/papillatessp2_percentchange.csv")

# ssp5
massivessp5 <- read.csv("models/massive/massivessp5_percentchange.csv")
caliculatessp5 <- read.csv("models/caliculate/caliculatessp5_percentchange.csv")
flabellatessp5 <- read.csv("models/flabellate/flabellatessp5_percentchange.csv")
papillatessp5  <- read.csv("models/papillate/papillatessp5_percentchange.csv")

# Bind data into 1 ----
changes <- bind_rows(massivessp2, massivessp5,
                     caliculatessp2, caliculatessp5,
                     flabellatessp2, flabellatessp5,
                     papillatessp2, papillatessp5)

changes <- changes%>%
  mutate(category = as.factor(category),
         scenario = as.factor(scenario),
         morphotype = as.factor(morphotype))
# plots
changes%>%
  filter(scenario == "ssp2")%>%
  group_by(morphotype)%>%
  ggplot(aes(x = morphotype, y = percent, fill = category)) +
  geom_bar(stat = "identity", position = "dodge")
  
changes%>%
  filter(scenario == "ssp5")%>%
  group_by(morphotype)%>%
  ggplot(aes(x = morphotype, y = percent, fill = category)) +
  geom_bar(stat = "identity", position = "dodge")

percent_change_plot <- changes%>%
  mutate(morphotype = factor(str_to_title(morphotype), 
                            levels = c("Papillate", "Massive", "Flabellate", "Caliculate")),
         scenario = toupper(scenario),
         category = factor(category, levels = c("Stable", "Loss", "Gain"))) %>%
  group_by(scenario, morphotype)%>%
  ggplot(aes(x = morphotype, y = percent, fill = category)) +
  geom_bar(stat = "identity", position = "stack",color = "black") +
  facet_grid(scenario ~ .) +  # This arranges scenarios in rows
  labs(x = "Morphotype",
       y = "Percentage of Potential Presence (%)",
       fill = 
       "Change from 
Present Day")+
  scale_fill_manual(values = c("Gain" = "#0AB4A9F9", 
                               "Loss" = "#9A0000FF", 
                               "Stable" = "#FABA39FF")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("analysis/percent_change_plot_stacked.png", width = 6, height = 6, dpi = 300)

## table of percentages
wide_changes <- changes %>%
  select(-X) %>%
  pivot_wider(names_from = category, values_from = percent, values_fill = 0)
