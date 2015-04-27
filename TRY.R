library(caret)
library(RCurl)
library(dplyr)
library(lubridate)
library(mice)
library(caret)
library(pROC)

NYTimesBlogTrain <- read.csv("/media/pdf/Desktop/edx/datasets/NYTimesBlogTrain.csv", na.strings="")

NYTimesBlogTrain <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTrain.csv", na.strings="")

NYTimesBlogTest <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTest.csv", na.strings="")

NewsTrain = select(NYTimesBlogTrain, -(Headline : Abstract))

NewsTest = select(NYTimesBlogTest, -(Headline : Abstract))

# clean up the data---------------------------------------------------------------------------------- 

NewsTrain$PubDate = ymd_hms(NewsTrain$PubDate)

  NewsTrain$day = wday(NewsTrain$PubDate, label = T)

    NewsTrain$month = month(NewsTrain$PubDate, label = T)

      NewsTrain$hour = hour(NewsTrain$PubDate)

        NewsTrain$second = second(NewsTrain$PubDate)


  NewsTrain$Mod_WordCount = log(NewsTrain$WordCount + 1)

    NewsTrain = select(NewsTrain, -PubDate, -WordCount)

#impute missing values--------------------------------------------------------------------------------

impute = NewsTrain[c("NewsDesk", "SectionName", "SubsectionName")]

  summary(impute)

    set.seed(144)
      
      imputed_1 = complete(mice(impute)) #should be converted to factors
  
          summary(imputed_1)


      NewsTrain$NewsDesk = imputed_1$NewsDesk

        NewsTrain$SectionName = imputed_1$SectionName

          NewsTrain$SubsectionName = imputed_1$SubsectionName

          # save NewsTrain for reusability
          
            NewsDummy = NewsTrain

#dummify the variables--------------------------------------------------------------------------------

#Use full rank = T in iter.---------------------------------------------------------------------------
    
NewsDummy_varT <- dummyVars(" ~ .", data = NewsDummy, fullRank=T)

  NewsDummy_dfT <- data.frame(predict(NewsDummy_varT, newdata = NewsDummy))

#Use full rank = F in iter., build models to compare the differences----------------------------------

NewsDummy_varF <- dummyVars(" ~ .", data = NewsDummy, fullRank=F)

  NewsDummy_dfF <- data.frame(predict(NewsDummy_varF, newdata = NewsDummy))

    
# shuffle and split the data into three parts----------------------------------------------------------
set.seed(1234)

    NewsDummy_dfT <- NewsDummy_dfT[sample(nrow(NewsDummy_dfT)),]

      split <- floor(nrow(NewsDummy_dfT)/3)

      ensembleData <- NewsDummy_dfT[0:split,]

        blenderData <- NewsDummy_dfT[(split+1):(split*2),]

            testingData <- NewsDummy_dfT[(split*2+1):nrow(NewsDummy_dfT),]

            
# set label name and predictors-----------------------------------------------------------------------
  labelName <- 'Popular'

      predictors <- names(ensembleData)[names(ensembleData) != labelName]


# create a caret control object to control the number of cross-validations performed------------------
  myControl <- trainControl(method='repeatedcv', number=10,repeats = 10, returnResamp='none')

# quick benchmark model------------------------------------------------------------------------------- 
    test_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)

      preds <- predict(object=test_model, testingData[,predictors])

        
      
        
        # train all the ensemble models with ensembleData---------------------------------------------
        model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl, )
        
        model_rpart <- train(ensembleData[,predictors], ensembleData[,labelName], method='rf', trControl=myControl)
        
        model_treebag <- train(ensembleData[,predictors], ensembleData[,labelName], method='logreg', trControl=myControl)
        
        # get predictions for each ensemble model for two last data sets------------------------------
        # and add them back to themselves-------------------------------------------------------------
        
        blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
        
        blenderData$rf_PROB <- predict(object=model_rpart, blenderData[,predictors])
        
        blenderData$treebag_PROB <- predict(object=model_treebag, blenderData[,predictors])
        
        testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
        
        testingData$rf_PROB <- predict(object=model_rpart, testingData[,predictors])
        
        testingData$treebag_PROB <- predict(object=model_treebag, testingData[,predictors])
        
        # see how each individual model performed on its own-----------------------------------------
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
        
        
        NewsDummy_dfT
        
        preds <- predict(object=final_blender_model, testingData[,predictors])
        
        auc <- roc(testingData[,labelName], preds)
        
        print(auc$auc)
        
        
        
        
        
        
        
        
        
    # creation of test set -----------------------------------------------------------------------------
  
  NewsTest$PubDate = ymd_hms(NewsTest$PubDate)
  
    NewsTest$day = wday(NewsTest$PubDate, label = T)
  
      NewsTest$month = month(NewsTest$PubDate, label = T)
  
        NewsTest$hour = hour(NewsTest$PubDate)
  
          NewsTest$second = second(NewsTest$PubDate)
  
  
  NewsTest$Mod_WordCount = log(NewsTest$WordCount + 1)
  
    NewsTest = select(NewsTest, -PubDate, -WordCount)
  
  #NewsTest Imputing
  
  #impute missing values
  
  imputeTest = NewsTest[c("NewsDesk", "SectionName", "SubsectionName")]
  
    summary(imputeTest)
  
  set.seed(144)
  
    imputed_Test = complete(mice(imputeTest)) #should be converted to factors
  
      summary(imputed_Test)
  
  
  NewsTest$NewsDesk = imputed_1$NewsDesk
  
    NewsTest$SectionName = imputed_1$SectionName
  
      NewsTest$SubsectionName = imputed_1$SubsectionName
  
  # save NewsTest for reusability
  
  NewsDummy_Test = NewsTest
  
  #dummify the variables
  
  #Use full rank = T in iter.
  
  NewsDummy_varTest <- dummyVars(" ~ .", data = NewsDummy_Test, fullRank=T)
  
    NewsDummy_dfTest <- data.frame(predict(NewsDummy_varTest, newdata = NewsDummy))
  
  #Use full rank = F in iter., build models to compare the differences
  
  NewsDummy_varTestF <- dummyVars(" ~ .", data = NewsDummy_Test, fullRank=F)
  
  NewsDummy_dfTestF <- data.frame(predict(NewsDummy_varTestF, newdata = NewsDummy_Test))
  
  
  #CALCULATING AUC
  
  auc <- roc(testingData[,labelName], preds)
  
    print(auc$auc) 
  
  
  
  
    #Kaggle Submission
    
    # quick benchmark model 
    
    
    labelName <- 'Popular'
    
    predictors <- names(NewsDummy_dfTest)[names(NewsDummy_dfTest) != labelName]
    
    
    test_model <- train(NewsDummy_dfT[,predictors], NewsDummy_dfT[,labelName], method='gbm', trControl=myControl)
  
    PredTest = predict(test_model, newdata=NewsTest)
    
    auc <- roc(NewsDummy_dfTest[,labelName], PredTest)
    
    print(auc$auc) 
    
    
    MySubmission = data.frame(UniqueID = NewsDummy_dfTest$UniqueID, Probability1 = PredTest)
    
    write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)
    