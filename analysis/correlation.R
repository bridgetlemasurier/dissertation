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

correlation <- round(cor(env), 1)

env_labels <- c("TRI", "TPI", "Aspect", "Depth", "Current Direction", "Current Speed", "Dissolved Oxygen", "Iron", 
                "Nitrate", "pH", "Phosphate", "Primary Productivity", 
                "Salinity", "Silicate", "Slope", "Temperature")

colnames(correlation) <- env_labels
rownames(correlation) <- env_labels

corr_plot <- ggcorrplot(correlation, 
           hc.order = TRUE,
           method = "circle",       # Color-coded correlation values
           type = "lower",         # Show only the lower triangle
           lab = TRUE,             # Display correlation values
           lab_size = 2.5,           # Adjust label size
           colors = c("blue", "white", "red"), # Gradient from negative to positive
           ggtheme = theme_minimal() + theme(panel.grid = element_blank()))

ggsave("analysis/correlation_plot.png", corr_plot, dpi = 300, width = 8, height = 6)
       