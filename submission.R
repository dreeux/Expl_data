library(caret)
library(RCurl)
library(dplyr)
library(lubridate)
library(mice)
library(caret)
library(pROC)
library(tm)
library(randomForest)

NYTimesBlogTrain <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTrain.csv", na.strings="")

NYTimesBlogTest <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTest.csv", na.strings="")

#preprocessing the data----------------------------------------------------------------------------------

#one suggestion while preparing both test and training set remove remove Id variables and rbind both 
#test and train and then preprocess and finally seperate

NewsTrain = select(NYTimesBlogTrain, -(Headline : Abstract))

NewsTest = select(NYTimesBlogTest, -(Headline : Abstract))

NewsTrain$PubDate = ymd_hms(NewsTrain$PubDate)

NewsTrain$day = wday(NewsTrain$PubDate, label = T)

NewsTrain$hour = hour(NewsTrain$PubDate)

NewsTrain$second = second(NewsTrain$PubDate)

NewsTrain$WordCount = log(NewsTrain$WordCount + 1)

NewsTrain = select(NewsTrain, -PubDate)

#impute missing values---------------------------------------------------------------------------------

impute = NewsTrain[c("NewsDesk", "SectionName", "SubsectionName")]

summary(impute)

set.seed(144)

imputed_1 = complete(mice(impute)) #should be converted to factors

summary(imputed_1)


NewsTrain$NewsDesk = imputed_1$NewsDesk

NewsTrain$SectionName = imputed_1$SectionName

NewsTrain$SubsectionName = imputed_1$SubsectionName

# creation of test set -----------------------------------------------------------------------------

NewsTest$PubDate = ymd_hms(NewsTest$PubDate)

NewsTest$day = wday(NewsTest$PubDate, label = T)

NewsTest$month = month(NewsTest$PubDate, label = T)

NewsTest$hour = hour(NewsTest$PubDate)

NewsTest$second = second(NewsTest$PubDate)


NewsTest$WordCount = log(NewsTest$WordCount + 1)

NewsTest = select(NewsTest, -PubDate)

#NewsTest Imputing

#impute missing values

imputeTest = NewsTest[c("NewsDesk", "SectionName", "SubsectionName")]

summary(imputeTest)

set.seed(144)

imputed_Test = complete(mice(imputeTest)) #should be converted to factors

summary(imputed_Test)


NewsTest$NewsDesk = imputed_Test$NewsDesk

NewsTest$SectionName = imputed_Test$SectionName

NewsTest$SubsectionName = imputed_Test$SubsectionName

#first model-------------------------------------------------------------------------------------------

#log_reg = glm(Popular ~., data = NewsTrain, family = binomial)
#summary(log_reg)

#for pred method to work The Newdata df should have the same column names 
#Also should have same levels of the factor

#pred <- predict(object= log_reg, newdata = NewsTest, type = "response")

#labelName <- "Popular"

#predictors <- names(NewsTrain)[names(NewsTrain) != labelName]

#error during auc calculation
#auc <- roc(NewsTrain[,labelName], NewsTrain[,predictors])

#MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = pred)

#write.csv(MySubmission, "5_20150426.csv", row.names=FALSE)

#second iteration------------------------------------------------------------------------------------

#use a log model having Headline and the NewsDesk, Section and Subsection, Weekday, Hour.  

NewsTrain$Popular = as.factor(NewsTrain$Popular)

CorpusHeadline = Corpus(VectorSource(c(NYTimesBlogTrain$Headline, NYTimesBlogTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

#Headline words train set creation--------------------------------------------------------------------

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

HeadlineWordsTrain$NewsDesk = NewsTrain$NewsDesk

HeadlineWordsTrain$SectionName = NewsTrain$SectionName

HeadlineWordsTrain$SubsectionName = NewsTrain$SubsectionName

HeadlineWordsTrain$Wordcount = NewsTrain$WordCount

HeadlineWordsTrain$day = NewsTrain$day

HeadlineWordsTrain$hour = NewsTrain$hour

HeadlineWordsTrain$second = NewsTrain$second

HeadlineWordsTrain$Popular = NewsTrain$Popular

#Headline words test set creation------------------------------------------------------------------


HeadlineWordsTest$NewsDesk = NewsTest$NewsDesk

HeadlineWordsTest$SectionName = NewsTest$SectionName

HeadlineWordsTest$SubsectionName = NewsTest$SubsectionName

HeadlineWordsTest$Wordcount = NewsTest$WordCount

HeadlineWordsTest$day = NewsTest$day

HeadlineWordsTest$hour = NewsTest$hour

HeadlineWordsTest$second = NewsTest$second


#fit a log model and also a rf model -- don`t use caret----------------------------------------------

HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)

# And make predictions on our test set:

PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "1_270515.csv", row.names=FALSE)

#rf model---------------------------------------------------------------------------------------------

newsforest = randomForest(Popular ~ ., data = HeadlineWordsTrain, type="prob",ntree=2000,nodesize=50)

# And make predictions on our test set:

PredTest = predict(newsforest, newdata=HeadlineWordsTest, type="prob")[,2]

# Now we can prepare our submission file for Kaggle--------------------------------------------------


MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "2_270515.csv", row.names=FALSE)

















#third iteration-------------------------------------------------------------------------------------

#experimenting with different values

#use a log model having Headline and the NewsDesk, Section and Subsection, Weekday, Hour.  

NewsTrain$Popular = as.factor(NewsTrain$Popular)

NYTimesBlogTrain$Headline = as.character(NYTimesBlogTrain$Headline)

NYTimesBlogTrain$Abstract = as.character(NYTimesBlogTrain$Abstract) 

NYTimesBlogTest$Headline = as.character(NYTimesBlogTest$Headline)

NYTimesBlogTest$Abstract = as.character(NYTimesBlogTest$Abstract)


CorpusHeadline = Corpus(VectorSource(c(NYTimesBlogTrain$Headline, NYTimesBlogTest$Headline,NYTimesBlogTrain$Abstract, NYTimesBlogTest$Abstract )))

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

#Headline words train set creation--------------------------------------------------------------------

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

HeadlineWordsTrain$NewsDesk = NewsTrain$NewsDesk

HeadlineWordsTrain$SectionName = NewsTrain$SectionName

HeadlineWordsTrain$SubsectionName = NewsTrain$SubsectionName

HeadlineWordsTrain$Wordcount = NewsTrain$WordCount

HeadlineWordsTrain$day = NewsTrain$day

HeadlineWordsTrain$hour = NewsTrain$hour

HeadlineWordsTrain$second = NewsTrain$second

HeadlineWordsTrain$Popular = NewsTrain$Popular

#Headline words test set creation------------------------------------------------------------------


HeadlineWordsTest$NewsDesk = NewsTest$NewsDesk

HeadlineWordsTest$SectionName = NewsTest$SectionName

HeadlineWordsTest$SubsectionName = NewsTest$SubsectionName

HeadlineWordsTest$Wordcount = NewsTest$WordCount

HeadlineWordsTest$day = NewsTest$day

HeadlineWordsTest$hour = NewsTest$hour

HeadlineWordsTest$second = NewsTest$second


#fit a log model and also a rf model -- don`t use caret----------------------------------------------

HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)

# And make predictions on our test set:

PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")

# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "3_270515.csv", row.names=FALSE)

#rf model---------------------------------------------------------------------------------------------


levels(NewsTest$NewsDesk) <- levels(NewsTrain$NewsDesk)

levels(NewsTest$SectionName) <- levels(NewsTrain$SectionName)

levels(NewsTest$SubsectionName) <- levels(NewsTrain$SubsectionName)


newsforest = randomForest(Popular ~ ., data = HeadlineWordsTrain, type="prob",ntree=2000,nodesize=80)

# And make predictions on our test set:

PredTest = predict(newsforest, newdata=HeadlineWordsTest, type="prob")[,2]

# Now we can prepare our submission file for Kaggle--------------------------------------------------


MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "4_270515.csv", row.names=FALSE)

#fourth iteration-------------------------------------------------------------------------------------






