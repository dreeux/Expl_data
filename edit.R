#always compute differences / raios of features help improve pred rates

rm(list = ls())

library(readr); library(xgboost); library(doParallel); library(caret); library(Metrics)

set.seed(8675309) 

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train$target

train$target <- NULL

train_ID <- train$ID

train$ID <- NULL

train$VAR_0241 <- as.numeric(as.character(train$VAR_0241))


test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test$ID

test$ID <- NULL

test$VAR_0241 <- as.numeric(as.character(test$VAR_0241))

print(dim(train)); print(dim(test))

#To find date columns -- really didn`t understand how

datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", 
                "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", 
                "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

train_cropped <- train[datecolumns]

train_cc <- data.frame(apply(train_cropped, 
                             2, 
                             function(x) as.double(strptime(x, 
                                                            format='%d%b%y:%H:%M:%S', tz="UTC"))))

#check how resonse varies with change in representation of date (month, year etc also check which day/month/year

# has highest loan buyers i.e. check for seasonality trends)

for (dc in datecolumns){
  
  train[dc] <- NULL
  
  train[dc] <- train_cc[dc]
}

train_cc <- NULL

train_cropped <- NULL

gc()

test_cropped <- test[datecolumns]

test_cc <- data.frame(apply(test_cropped, 
                            2, 
                            function(x) as.double(strptime(x, 
                                                           format='%d%b%y:%H:%M:%S', tz="UTC"))))

for (dc in datecolumns){
  
  test[dc] <- NULL
  
  test[dc] <- test_cc[dc]
}

test_cc <- NULL

test_cropped <- NULL

gc()

feature.names <- names(train)[1:ncol(train)]

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
  
}

print(dim(train)); print(dim(test))
#dates seem to be skewed towards right , applying Box Cox transforms to both train and test


train_pre <-  preProcess(train[datecolumns], method = ("BoxCox",  #"scale"))

train_pre_pred <- predict(train_pre, train[datecolumns])

train[datecolumns] <- train_pre_pred


test_pre <-  preProcess(test[datecolumns], method = ("BoxCox", #"scale"))

test_pre_pred <- predict(test_pre, test[datecolumns])

test[datecolumns] <- test_pre_pred

print(dim(train)); print(dim(test))

############################################################################################################


#This data set has large number of nzv removing them

nzv <- nearZeroVar(train)

nzv_test <- nearZeroVar(test)

train <- train[, -nzv]

test <- test[, -nzv_test]

print(dim(train)); print(dim(test))


#train_pre_total <-  preProcess(train[ , !(names(train) %in% datecolumns)], method =   "scale") 

#train_pre_pred <- predict(train_pre_total, train[ , !(names(train) %in% datecolumns)])
                                                  
#train[ , !(names(train) %in% datecolumns)] <- train_pre_pred
                                                  
                                                  
#test_pre_total <-  preProcess(test[, !(names(test) %in% datecolumns)], method = "scale")
                                                  
#test_pre_pred <- predict(test_pre_total, test[ , !(names(test) %in% datecolumns)])
                                                  
#test[ , !(names(test) %in% datecolumns)] <- test_pre_pred
                                                  
#print(dim(train)); print(dim(test))
                                                  
train[is.na(train)] <- -987654

test[is.na(test)]   <- -987654

#tmp <- rbind(train, test)


#rm(tmp)

feature.names <- names(train)[1:ncol(train)]

split <- createDataPartition(y = train_target, list = F, p = 0.9)

training <- train[split, ]

y_train <- train_target[split]

validation <- train[-split, ]

y_val <- train_target[-split]

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label= y_train)

dval <- xgb.DMatrix(data.matrix(validation[,feature.names]), label=y_val)

watchlist <- list(eval = dval, train = dtrain)

gc()

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.014, #0.06, #0.01,
                max_depth           = 10,  # changed from default of 8
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                eval_metric         = "auc",
                nthreads = -1
                # alpha = 0.0001, 
                # lambda = 1
)

rm(training, validation, test_pre_pred, train_pre_pred, test_cc, train_cc )

cl <- makeCluster(2); registerDoParallel(cl)

#clf_cv <- xgb.cv(params = param, data = dtrain, nrounds = 1500, nfold = 4, 
                 
#               showsd = T, verbose = T, maximize = T,  watchlist = watchlist)



clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2000, 
                    verbose             = 2, 
                    watchlist           = watchlist,
                    maximize            = TRUE)

gc()

submission <- data.frame(ID=test_ID)

submission$target <- NA 

submission[,"target"] <- predict(clf, data.matrix(test[,feature.names]))

write_csv(submission, "D:/kaggle/Springleaf/SUBMISSION/09192015_2.csv")

############################################################################################################

control <- trainControl(method = "cv", number = 10, verboseIter = T,
                        
                        summaryFunction = twoClassSummary
)


grid_xgb <- expand.grid(eta = 0.001,
                        
                        max_depth = 10, 
                        
                        nrounds = 2000)


caret_xgb <- train(train, train_target,
                   
                   method = "xgbTree" , trControl = control,
                   
                   metric = "auc" ,
                   
                   tuneGrid = grid_xgb,
                   
                   savePredictions=TRUE, 
                   
                   "min_child_weight" = 20,
                   
                   "subsample" = .7,
                   
                   "colsample_bytree" = .8,
                   
                   "scale_pos_weight" = 1.5
                   
)

##########################################################################################################
#09192015

#This time scaled only the date columns should see what happens when we scale the entire train set

#Tried removing predictors with corelations greater than 0.75 left with oly 400var , 0.78xx CV score

#bad cv scores when all variables are scaled

#Added caret model obj to the model