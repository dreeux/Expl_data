library(readr)
 
library(xgboost)

set.seed(28071993)

train <- read_csv("D:/kaggle/Forecasting/DATA/train.csv")

test  <- read_csv("D:/kaggle/Forecasting/DATA/test.csv")

store <- read_csv("D:/kaggle/Forecasting/DATA/store.csv")

train <- merge(train,store)

test <- merge(test,store)

train[is.na(train)]   <- 0

test[is.na(test)]   <- 0

train <- train[ which(train$Open=='1'),]

train <- train[ which(train$Sales!='0'),]

# seperating out the elements of the date column for the train set

train$month <- as.integer(format(train$Date, "%m"))

train$year <- as.integer(format(train$Date, "%y"))

train$day <- as.integer(format(train$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)

# seperating out the elements of the date column for the test set

test$month <- as.integer(format(test$Date, "%m"))

test$year <- as.integer(format(test$Date, "%y"))

test$day <- as.integer(format(test$Date, "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)

test <- test[,-c(4,7)]

feature.names <- names(train)[c(1,2,5:19)]

feature.names

for (f in feature.names) {

    if (class(train[[f]])=="character") {
  
        levels <- unique(c(train[[f]], test[[f]]))
    
        train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
        test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

tra<-train[,feature.names]

RMPSE<- function(preds, dtrain) {

    labels <- getinfo(dtrain, "label")
  
    elab<-exp(as.numeric(labels))-1
  
    epreds<-exp(as.numeric(preds))-1
  
    err <- sqrt(mean((epreds/elab-1)^2))
  
    return(list(metric = "RMPSE", value = err))
}

split = createDataPartition(train$Sales, p = 0.9, list = F)

response <- train$Sales

response_val <- response[-split]

response_train <- response[split]

dval <- xgb.DMatrix(data=data.matrix(tra[-split, ]), label = log(response_val+1))

dtrain<-xgb.DMatrix(data=data.matrix(tra[split, ]), label = log(response_train+1))

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.025, # 0.06, #0.01,
                max_depth           = 10, #changed from default of 8
                subsample           = 0.9, # 0.7
                colsample_bytree    = 0.7 # 0.7
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

cl <- makeCluster(2); registerDoParallel(cl)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000, #300, #280, #125, #250, # changed from 300
                    verbose             = 2,
#                    early.stop.round    = 100,
                    watchlist           = watchlist,
                    maximize            = T,
                    feval=RMPSE
)

pred1 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1

submission <- data.frame(Id=test$Id, Sales=pred1)


write_csv(submission, "rf1.csv")
