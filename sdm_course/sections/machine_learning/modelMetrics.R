##### further model evaluation for binary classification models

# library ----
library(caret)
library(ModelMetrics)

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

# model1 ----

## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="rf",importance=TRUE)
# importance true to get importance of predictors

summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)

# test model 1
p1 = predict(mod_fit, newdata = testing)

# other ways to assess model accuracy ----

#AUC test response vs predicted response
auc(testing$pb, p1) # actual y vs predicted y
# auc = 0.9821067

#confusion matrix
# derived in case of binary and multiclass classification
# tells us how many data points have been assigned correct lable/in correct cluster

confusionMatrix(testing$pb, p1, cutoff = 0.5) # actual y, predicted y, cut off
# 105 correctly classified points for cat1, 
# 204 correctly classified points for cat2

overall = (105+204)/(105+8+16+204) # proportion of correctly classified
overall # 0.9279279 really quite good

#log loss/entropy loss
# usually computed in cases of binary outcomes
# quantifies accuracy of a classifier by penalising false classificaion
# lower value = more confidence
logLoss(testing$pb, p1, distribution = "binomial")
# 0.1921435 good job of identifying species distribution and habitat suitability