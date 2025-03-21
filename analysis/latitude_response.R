# latidude response
# 11/3/2024

#Aim
# To combine latitudinal responses in singular plot

# LIBRARY
library(tidyverse)
library(viridis)

# DATA
caliculate <- read.csv("analysis/caliculate_latsummary.csv")
flabellate <- read.csv("analysis/flabellate_latsummary.csv")
massive <- read.csv("analysis/massive_latsummary.csv")
papillate <- read.csv("analysis/papillate_latsummary.csv")


# bind 
latitude <- bind_rows(caliculate, flabellate, massive, papillate)
latitude <- latitude%>%
  mutate(ssp = as.factor(ssp),
         morphotype = as.factor(morphotype),
         latitude = y)

# plot
latitude%>%
  filter(ssp == "ssp2")%>%
  group_by(morphotype)%>%
  ggplot(aes(x = y, y = percent_gain, colour = morphotype))+
  geom_point()+
  geom_smooth(method = "lm")

latitude%>%
  filter(ssp == "ssp5")%>%
  group_by(morphotype)%>%
  ggplot(aes(x = y, y = percent_gain, colour = morphotype))+
  geom_point()+
  geom_smooth(method = "lm")

latitude%>%
  mutate(morphotype = str_to_title(morphotype),
         ssp = toupper(ssp)) %>%
  group_by(ssp, morphotype)%>%
  ggplot(aes(x = y, y = percent_gain, colour = morphotype))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")+
  facet_grid(ssp ~ .) +  # This arranges scenarios in rows
  labs(x = "Latitude (°)",
       y = "Percentage Gain (%)",
       colour = "Morphotype") +
  scale_colour_viridis_d()+
  theme_bw() +
  theme(panel.grid = element_blank())


latitude_shift <- latitude%>%
  mutate(morphotype = factor(str_to_title(morphotype), 
                                          levels = c("Papillate", "Massive", "Flabellate", "Caliculate")),
         ssp = toupper(ssp)) %>%
  group_by(morphotype, ssp)%>%
  ggplot(aes(x = y, y = percent_gain, colour = ssp))+
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")+
  facet_wrap(~morphotype) +  # This arranges scenarios in rows
  scale_colour_manual(values = c("SSP2" = "#00BFD5" ,
                                 "SSP5" = "#F9766D")) +
  labs(x = "Latitude (°)",
       y = "Percentage Gain (%)",
       colour = "Climate Change
     Scenario")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = margin(5, 5, 5, 5)) +  # Adjusts plot margins
  scale_x_continuous(expand = c(0, 0)) +  # Removes extra space on x-axis
  scale_y_continuous(expand = c(0, 0))

ggsave("analysis/latitudeshift.png", latitude_shift,
       width = 8, height = 6, dpi = 300)
