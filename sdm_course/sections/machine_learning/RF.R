######################################################################
################## Random Forest
# robust performance for binary classification process

# library ----
library(caret)
library(pROC)
library(raster)
library(randomForest)

#import and tidy data ----
setwd("sdm_course/sections/data/ready_rasters")

pa=read.csv("Pres_abs.csv")
pa1 <- pa%>%
  select(!(land))

#pa=na.omit(pa)

tail(pa1)

summary(pa1)

pb <- as.factor(pa1$pb) #1 stands for presence and 0 for absence

head(pa1)

#split data ----
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa1$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa1[ trainIndex,] #75% data for model training
testing= pa1[-trainIndex,] #25% for model testing

head(training)

# model1 ----

## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="rf",importance=TRUE)
# importance true to get importance of predictors

summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)

## test the model----
p1 <- predict(mod_fit, newdata = testing) #predict on the test data

#test model fit-auc

roc = pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc) # 0.9821 wowee very good

plot(roc)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

## build an SDM ----

## read in all predictors
datafiles = Sys.glob("*.tif") #Or whatever identifies your files

datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

p = predict(stck, mod_fit) #use predict to implement the RF model stored
#in mod_fit on the raster stack of our predictors
plot(p,main="RF Predictive Map")


####### test the impact of individual predictors ----
m1 = randomForest(pb ~., data=training) #randomForest package
#ignore warning
#class(m1)

# partial plots
partialPlot(m1, training, preciptn, pb) # effect of 1 variable when all else constant
#rf model, training data, X, Y/response variable

p2 = predict(stck, m1) #use predict to implement the RF model stored
#in mod_fit on the raster stack of our predictors
plot(p2,main="RF Predictive Map") # results barely change so good!


#using traing and testing
m2 = randomForest(pb ~., data=pa1) #all data
p3 = predict(stck, m2) #use predict to implement the RF model stored
#in mod_fit on the raster stack of our predictors
plot(p3,main="RF Predictive Map-All")
