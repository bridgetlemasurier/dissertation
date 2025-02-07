# Heat mapping
# 6/2/2025

# aim to look at which variables are most correlated to decide which to take forwards

# library ----
# install.packages("corrgram")
library(tidyverse)
library(corrgram)
# install.packages("ggcorrplot")
library(ggcorrplot)

# read in sponges ----
sponge_info <- read.csv("data/sponge_envinfo.csv")

# correlation matrix ----
env <- sponge_info[, c(14:29)]
corr <- round(cor(env), 1)

ggcorrplot(corr,hc.order = TRUE,type = "lower")

ggcorrplot(corr, 
           hc.order = TRUE,
           method = "circle",       # Color-coded correlation values
           type = "lower",         # Show only the lower triangle
           lab = TRUE,             # Display correlation values
           lab_size = 2.5,           # Adjust label size
           colors = c("blue", "white", "red"), # Gradient from negative to positive
           title = "Environmental Predictor Correlations",
           ggtheme = theme_minimal()) 

