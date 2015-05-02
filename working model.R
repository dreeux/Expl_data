NewsTrain_1 <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTrain.csv", na.strings="")

NewsTest_1 <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTest.csv", na.strings="")

require(lubridate)

require(dplyr)

require(mice)

remov = select(NewsTrain_1, -Popular)

new = rbind(remov, NewsTest_1)

impute = new[c("NewsDesk", "SectionName", "SubsectionName")]

set.seed(123)

imputed_1 = complete(mice(impute)) #should be converted to factors

new$NewsDesk = imputed_1$NewsDesk

new$SectionName = imputed_1$SectionName

new$SubsectionName = imputed_1$SubsectionName

new$Headline = as.character(new$Headline)

new$Abstract = as.character(new$Abstract)

new$Snippet = as.character(new$Snippet)

new$PubDate = ymd_hms(new$PubDate)

new$weekday = wday(new$PubDate, label = T)

new$hour = hour(new$PubDate)

new$minute = minute(new$PubDate)

NewsTrain = new[1:6532, ];NewsTest = new[6533:8402,]

levels(NewsTest$NewsDesk) = levels(NewsTrain$NewsDesk)

levels(NewsTest$SectionName) = levels(NewsTrain$SectionName)

levels(NewsTest$SubsectionName) = levels(NewsTrain$SubsectionName)

library(tm)

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

CorpusHeadline = tm_map(CorpusHeadline, tolower)

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.995)

HeadlineWords = as.data.frame(as.matrix(sparse))

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

HeadlineWords$PubDate = new$PubDate

HeadlineWords$weekday = new$weekday

HeadlineWords$hour = new$hour

HeadlineWords$NewsDesk = new$NewsDesk

HeadlineWords$SectionName = new$SectionName

HeadlineWords$SubsectionName = new$SubsectionName

HeadlineWords$WordCount = log(new$WordCount)

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

HeadlineWordsTrain$Popular = NewsTrain_1$Popular

library(plyr)

HeadlineWordsTrain$Popular = as.factor(HeadlineWordsTrain$Popular)

HeadlineWordsTrain$Popular = revalue(HeadlineWordsTrain$Popular, c("0" = "Low", "1" = "High"))


# Model training process----------------------------------------------------------------------------------

ctrl <- trainControl(method = "repeatedcv", repeats = 1, classProbs = TRUE ,

                     summaryFunction = twoClassSummary )

plsFit <- train(Popular ~ .,data = HeadlineWordsTrain,method = "gbm"

,  trControl = ctrl,preProc = c("center", "scale"), metric = "ROC")




plsFit <- randomForest(Popular ~ .,data = HeadlineWordsTrain)

#prediction on the test data

plsProbs <- predict(plsFit, newdata = HeadlineWordsTest, type = "prob")

head(plsProbs)


ggplot(plsFit) + scale_x_log10()

# create confusion matrix

#confusionMatrix(data = plsClasses, testing$Class)

#resamps <- resamples(list(pls = plsFit, rda = rdaFit))

#library(pROC)

#rpartROC <- roc(testing$Class, rpartProbs[, "PS"],
                  #+ levels = rev(cell_lev))

plot(rpartROC, type = "S", print.thres = .5)

summary(resamps)

diffs <- diff(resamps)

summary(diffs)

xyplot(resamps, what = "BlandAltman")


# Now we can prepare our submission file for Kaggle:

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

#Using SVM------------------------------------------------------------------------------------------------

ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE ,

                     summaryFunction = twoClassSummary )


plsFit_svm <- train(as.factor(Popular) ~ .,data = HeadlineWordsTrain, method = "rf",

                  trControl = ctrl, preProc = c("center", "scale"), metric = "ROC")

system.time(plsFit_svm)

plsProbs_svm <- predict(plsFit, newdata = HeadlineWordsTest, type = "prob")

ggplot(plsFit_svm) + scale_x_log10()

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = plsFit_svm)

write.csv(MySubmission, "3_SubmissionHeadlineLog.csv", row.names=FALSE)


plot(plsFit_svm, metric = "ROC", scales = list(x = list(log = 2)))

#comparing results from svm and gbm-----------------------------------------------------------------------

resamp_values <- resamples(list(gbm = plsFit, SVM = plsFit_svm))

#visualising the values

dotplot(resamp_Values, metric = "ROC")


#feature selection-------------------------------------------------------------------

HeadlineWords$Q_m = ifelse(grepl("\\?", new$Headline), 1, 0)

HeadlineWords$Excl_m = ifelse(grepl("\\!", new$Headline), 1, 0)

HeadlineWords$sep_m = ifelse(grepl("\\-", new$Headline), 1, 0)



for(i in 1:length(words)){
  HeadlineWords$i = ifelse(grepl("i", new$Headline), 1, 0)
   table(HeadlineWords$i)
}