NYTimesBlogTest <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTest.csv", na.strings="", stringsAsFactors=FALSE)

NYTimesBlogTrain <- read.csv("C:/Users/amulya/Desktop/edx/NYTimesBlogTrain.csv", na.strings="", stringsAsFactors=FALSE)

maintest = NYTimesBlogTest

main = NYTimesBlogTrain

require(lubridate)

require(mice)

maintest$Date = ymd_hms(maintest$PubDate) 

maintest$day = wday(maintest$Date, label = T)

maintest$month = month(maintest$Date, label = T)

maintest$hour = hour(maintest$Date)

maintest$second = second(maintest$Date)


#training set

main$Date = ymd_hms(main$PubDate) 

main$day = wday(main$Date, label = T)

main$month = month(main$Date, label = T)

main$hour = hour(main$Date)

main$second = second(main$Date)

#impute missing values

simple_1 = maintest[c("NewsDesk", "SectionName", "SubsectionName")]
summary(simple_1)
set.seed(144)
imputed_1 = complete(mice(simple_1)) #should be converted to factors
summary(imputed_1)

maintest$NewsDesk = imputed_1$NewsDesk
maintest$SectionName = imputed_1$SectionName
maintest$SubsectionName = imputed_1$SubsectionName

#training set

simple = main[c("NewsDesk", "SectionName", "SubsectionName")]
summary(simple)

set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

main$NewsDesk = imputed$NewsDesk
main$SectionName = imputed$SectionName
main$SubsectionName = imputed$SubsectionName


library(tm)

CorpusHeadline = Corpus(VectorSource(c(main$Headline, maintest$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(main) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"

HeadlineWordsTrain = head(HeadlineWords, nrow(main))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(maintest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"

HeadlineWordsTest = tail(HeadlineWords, nrow(maintest))

# Note that this split of HeadlineWords works to properly put the observations back into the training and testing sets, because of how we combined them together when we first made our corpus.

# Before building models, we want to add back the original variables from our datasets. We'll add back the dependent variable to the training set, and the WordCount variable to both datasets. You might want to add back more variables to use in your model - we'll leave this up to you!

HeadlineWordsTrain$Popular = main$Popular

HeadlineWordsTrain$WordCount = main$WordCount
HeadlineWordsTest$WordCount = maintest$WordCount

#Impute missing values

HeadlineWordsTest$NewsDesk=  maintest$NewsDesk
HeadlineWordsTest$SectionName =maintest$SectionName  

HeadlineWordsTest$day = maintest$day
HeadlineWordsTest$hour = maintest$hour

HeadlineWordsTrain$NewsDesk=  main$NewsDesk
HeadlineWordsTrain$SectionName = main$SectionName  

HeadlineWordsTrain$day = main$day
HeadlineWordsTrain$hour = main$hour

HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)

PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")

MySubmission = data.frame(UniqueID = maintest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionHeadlineLog.csv", row.names=FALSE)

