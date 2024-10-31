########### Pre-proessing for machine learning

library(caret)

setwd("sdm_course/sections/data/ready_rasters")

pa=read.csv("Pres_abs.csv")

#pa=na.omit(pa)

head(pa)

summary(pa)

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

head(training)
