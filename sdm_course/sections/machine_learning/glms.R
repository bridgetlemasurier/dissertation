#############################################################
################## GLM-- logistic regression 
#(when we have binary response variable)
## link function: binomial

library(caret)
library(pROC)
library(raster)

setwd("sdm_course/sections/data/ready_rasters")

pa=read.csv("Pres_abs.csv")

# split data into test and train ----

head(pa) #1 --> species occurence
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing


# general model ----
set.seed(825) # this wasn't explained...

#y is pb
pb=as.factor(training$pb) #1 stands for presence and 0 for absence
land=as.factor(training$land) #land use categories are categorical

m1 = glm(pb ~., data=training) #base package # ~. means include all predictors
#class(m1)
summary(m1) 
# all factors statistically significant -> all have influence over presence


## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="glm",family="binomial")
# ignore warning
summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)#caret 
# altitude is most influential
# land use does correlate but not an ipmorttant predictor

## test the model----
p1=predict(mod_fit, newdata=testing) #predict on the test data
# predicted presence and absence
p2=predict(mod_fit, newdata=testing, type="raw")

#test model fit-auc
roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc = pROC::auc(roc.glmModel)
# Area under the curve: 0.8083 - GOOD!

plot(roc.glmModel)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

## build an SDM ----

## read in all predictors-since they are all significant
datafiles = Sys.glob("*.tif") #Or whatever identifies your files

datafiles #list of predictors

ext = extent(99, 105, 1.2, 6.7)

stck = stack() #empty raster stack for storing raster layers

for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

names(stck)

p1 = predict(stck, mod_fit) #use predict to implement the GLM model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="GLM Predictive Map")

p2 = predict(stck, m1) #use the basic GLM to predict 
#implement the GLM model stored
#in mod_fit on the raster stack of our predictors
plot(p2,main="GLM Predictive Map")

### remove land use as a predictor
names(stck)

s=dropLayer(stck,3)
names(s)

mod_fit2=train(pb~altitude+aspect1+preciptn+roughness1+slope+tempAvg+tempMin
               ,data=training,trControl=train_control,method="glm",family="binomial")
#remove land use type
summary(mod_fit2)

p3 = predict(s, mod_fit2) #use predict to implement the GLM model stored
#in mod_fit on the raster stack of our predictors
plot(p3,main="GLM Predictive Map")
