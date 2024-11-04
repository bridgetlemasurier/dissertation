#######################################################################
############### SVM - support vector machines

setwd("F:/SDM_in R/Data/1_Raster data/ready_rasters")
library(caret)
library(pROC)

pa=read.csv("Pres_abs.csv")
pa1 <- pa%>%
  select(!(land))

#pa=na.omit(pa)

head(pa1)

summary(pa1)

pa1$pb <- as.factor(pa1$pb) #1 stands for presence and 0 for absence

head(pa)

set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

head(training)


## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

#svm with rbf kernel - most widely used (best performance)
mod_fit1=train(pb~altitude+aspect1+preciptn+roughness1+slope+tempAvg+tempMin,
               data=training,trControl=train_control,method="svmRadial")
# svmRadial - radial bias function

summary(mod_fit1)

### for polynomial kernel specify method="svmPoly"

## importance of the different predictors
varImp(mod_fit1) # roughness seems most important

## test the model
p1=predict(mod_fit1, newdata=testing) #predict on the test data

#test model fit-auc
roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing data

#with predicted responses

auc= pROC::auc(roc)
auc # 0.9117

plot(roc.glmModel)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

## read in all predictors

datafiles = Sys.glob("*.tif") #Or whatever identifies your files

datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}

p1 = predict(stck, mod_fit1) #use predict to implement the SVM model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="RBF Kernel Predictive Map")


# use all data ----
mod_fit2=train(pb~altitude+aspect1+preciptn+roughness1+slope+tempAvg+tempMin,
               data=pa1,trControl=train_control,method="svmRadial")
p2 = predict(stck, mod_fit2)

plot(p2, main = "RBF Kernel SVM Predictive Map")
