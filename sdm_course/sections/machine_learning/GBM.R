#######################################################################
############### GBM gradient boosting machine
# another tree based method 

# library ----
library(caret)
library(pROC)
library(raster)
library(gbm)

#import and tidy data ----
setwd("sdm_course/sections/data/ready_rasters")

pa=read.csv("Pres_abs.csv")
pa1 <- pa%>%
  select(!(land))

#pa=na.omit(pa)

tail(pa1)

summary(pa1)

head(pa1)

#split data ----
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa1$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa1[ trainIndex,] #75% data for model training
testing= pa1[-trainIndex,] #25% for model testing

head(training)

pb <- as.factor(pa1$pb) #1 stands for presence and 0 for absence

#model ----
## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="gbm")

summary(mod_fit) #will provide relative influence measure plot

## importance of the different predictors in pres/abs
varImp(mod_fit) # not working?


## test the model----
p1=predict(mod_fit, newdata=testing) #predict on the test data

#test model fit-auc

roc= pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc)
auc # 0.9483

plot(roc)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

#raster predictors----
datafiles = Sys.glob("*.tif") #Or whatever identifies your files

datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

p1 = predict(stck, mod_fit) #use predict to implement the GBM model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="GBM Predictive Map")

###### use the whole dataset ----

mod_fit2=train(pb~.,data=pa1,trControl=train_control,method="gbm")

summary(mod_fit2)

p2 = predict(stck, mod_fit2) #use predict to implement the MARS model stored
#in mod_fit on the raster stack of our predictors
plot(p2,main="GBM Predictive Map")

# partial dependence plots - gbm package ----
# partial dependence plot allow us to evaluate impact of predictor on response (p/a)

boost = gbm(pb~. , data=training, 
            distribution = 'gaussian', # why?
            n.trees = 5000,  # 5000 trees
            interaction.depth = 4)  # these are default robust values

summary(boost)

plot(boost, i='slope')  # pdm for slope

plot(boost, i='altitude')

plot(boost, i='roughness1')
