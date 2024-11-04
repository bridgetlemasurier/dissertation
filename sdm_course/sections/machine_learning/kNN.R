#######################################################################
############### kNN - k nearest neighbour algorithm
# commonly used classifciation algorithm
# robust relusts - not usely SDM

setwd("F:/SDM_in R/Data/1_Raster data/ready_rasters")
library(caret)
library(pROC)

pa=read.csv("Pres_abs.csv")
pa1 <- pa%>%
  select(!(land))

head(pa1)
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa1$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa1[ trainIndex,] #75% data for model training
testing= pa1[-trainIndex,] #25% for model testing

set.seed(825) # why????

pb=as.factor(training$pb) #1 stands for presence and 0 for absence


## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="knn")

summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)

## test the model
p1=predict(mod_fit, newdata=testing) #predict on the test data

#test model fit-auc
roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc.glmModel)
auc # 0.908

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

p1 = predict(stck, mod_fit) #use predict to implement the MARS model stored
#in mod_fit on the raster stack of our predictors

plot(p1,main="kNN Predictive Map")

