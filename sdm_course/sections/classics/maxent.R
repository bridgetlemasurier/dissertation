#####################################################################
################## maxent

library(raster)
library(dismo)
library(rJava)
library(red)
library(terra)

setwd("sdm_course/sections/data/bioclim")

datafiles = Sys.glob("*.tif") # Or whatever identifies your files
datafiles #list of predictors (7) which will be used to predict distribution

# create raster stack ----
stck <- stack() #empty raster stack for storing raster layers

for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}


stck #raster predictors as a stackplot

plot(stck,1)

### presence data ----

horn <- read.csv("hornbill_my1.csv")
head(horn)
horn1= horn[,-1]#first column not needed

points(horn1, col='blue') 

# split presence data
group = kfold(horn1, 5) #split the data into 5 portions 

#build and test models on all 5 data splits
pres_train = horn1[group != 1, ] # training 4/5
pres_test = horn1[group == 1, ] # testing 1/5
# later will look at different ways to split

####

xm = maxent(stck, pres_train) #implement maxent on the presence-only data
# stck = raster data, pres_train = presence data
plot(xm) ## variable importance

# model vs random ----

ext = extent(99, 105, 1.2, 6.7) #peninsular malaysia

# 1000 background data points
backg = randomPoints(stck, n=1000,ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group = kfold(backg, 5)

#pseudo-absences for training model performances
backg_train = backg[group != 1, ]
backg_test = backg[group == 1, ]

e = evaluate(pres_test, backg_test, xm, stck)
e
  # class          : ModelEvaluation 
  # n presences    : 177 
  # n absences     : 195 
  # AUC            : 0.7917427 
  # cor            : 0.5227453 
  # max TPR+TNR at : 0.5107535 

# heat map ----
p= predict(stck, xm, ext=ext, progress='')
# use model xm to predict species presence with stck over given ext
#0 1 scale where 1 indicates the most suitable habitat 
#and 0 least suitable habitat 

par(mfrow=c(1,1))

plot(p, main='Maxent, raw values') # yay!

# analyses ----
map.easy(horn, layers = stck, habitat = NULL, zone = NULL,
         thin = TRUE, error = NULL, move = TRUE, dem = "altitude.tif", pca = 0,
         filename = NULL, mapoption = NULL, testpercentage = 20, mintest = 20,
         runs = 0, subset = 0)
# creates csvs for each species with min and max elevation for each

##maxent 
red::map.sdm(horn1, stck, error = NULL, year = NULL,
        idconf = NULL, categorical = NULL, thres = 0,
        testpercentage = 20, mcp = TRUE, points = FALSE, eval = TRUE, runs = 0,
        subset = 0)
