library(caret)
library(RCurl)
library(dplyr)
library(lubridate)
library(mice)
library(caret)
library(pROC)

NYTimesBlogTrain <- read.csv("/media/pdf/Desktop/edx/datasets/NYTimesBlogTrain.csv", na.strings="")

News = select(NYTimesBlogTrain, -(Headline : Abstract))

# clean up the data 

News$PubDate = ymd_hms(News$PubDate)
News = select(News, -PubDate)

#impute missing values

impute = News[c("NewsDesk", "SectionName", "SubsectionName")]
summary(impute)
set.seed(144)
imputed_1 = complete(mice(impute)) #should be converted to factors
summary(imputed_1)

News$NewsDesk = imputed_1$NewsDesk
News$SectionName = imputed_1$SectionName
News$SubsectionName = imputed_1$SubsectionName

# shuffle and split the data into three parts
set.seed(1234)
News <- News[sample(nrow(News)),]
split <- floor(nrow(News)/3)
ensembleData <- News[0:split,]
blenderData <- News[(split+1):(split*2),]
testingData <- News[(split*2+1):nrow(News),]

# set label name and predictors
labelName <- 'Popular'
predictors <- names(ensembleData)[names(ensembleData) != labelName]

News = select(News, -PubDate)
library(caret)
# create a caret control object to control the number of cross-validations performed
myControl <- trainControl(method='cv', number=5, returnResamp='none')

# quick benchmark model 
test_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)
preds <- predict(object=test_model, testingData[,predictors])

auc <- roc(testingData[,labelName], preds)
print(auc$auc) # Area under the curve: 0.9896

# train all the ensemble models with ensembleData
model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl)
model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rpart', trControl=myControl)
model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='treebag', trControl=myControl)

# get predictions for each ensemble model for two last data sets
# and add them back to themselves
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])

# see how each individual model performed on its own
auc <- roc(testingData[,labelName], testingData$gbm_PROB )
print(auc$auc) # Area under the curve: 0.9893

auc <- roc(testingData[,labelName], testingData$rf_PROB )
print(auc$auc) # Area under the curve: 0.958

auc <- roc(testingData[,labelName], testingData$treebag_PROB )
print(auc$auc) # Area under the curve: 0.9734

# run a final model to blend all the probabilities together
predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)

# See final prediction and AUC of blended ensemble
preds <- predict(object=final_blender_model, testingData[,predictors])
auc <- roc(testingData[,labelName], preds)
print(auc$auc)  # Area under the curve: 0.9922