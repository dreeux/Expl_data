#always compute differences / raios of features help improve pred rates

rm(list = ls())

library(readr); library(xgboost); library(doParallel); library(caret) 

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(8675309) 

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train$target

train$target <- NULL

train_ID <- train$ID

train$ID <- NULL

levels <- unique(train$VAR_0241)

train$VAR_0241 <- as.factor(train$VAR_0241, levels = levels)

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test$ID

test$ID <- NULL

levels <- unique(test$VAR_0241)

test$VAR_0241 <- as.factor(test$VAR_0241, levels = levels)

print(dim(train)); print(dim(test))

#To find date columns -- really didn`t understand how

dates = names(train[,grep("JAN1|FEB1|MAR1", train), ])

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

#dates seem to be skewed towards right , applying Box Cox transforms to both train and test

train_pre <-  preProcess(train[datecolumns], method = ("BoxCox", "scale"))

train_pre_pred <- predict(train_pre, train[datecolumns])

train[datecolumns] <- train_pre_pred


test_pre <-  preProcess(test[datecolumns], method = ("BoxCox", "scale"))

test_pre_pred <- predict(test_pre, test[datecolumns])

test[datecolumns] <- train_pre_pred

#This data set has large number of nzv removing them

nzv <- nearZeroVar(train)

nzv_test <- nearZeroVar(test)

train <- train[, -nzv]

test <- test[, -nzv_test]

#removing predictors with corelations greater than 0.75(##HYPOTHESIS)

cor_train <- cor(train) # convert to a df and save it as a csv and delete the file

cor_test <- cor(test)

highcor_tr <- findCorrelation(cor_train, cutoff = 0.75)

highcor_te <- findCorrelation(cor_test, cutoff = 0.75)

cor_train <- data.frame(cor_train)

write_csv(cor_train, "TRAIN_COR.csv")

cor_test <- data.frame(cor_test)

write_csv(cor_test, "TEST_COR.csv")

train <- train[, -highcor_tr]

test <- test[, -highcor_te]


                                                            
train[is.na(train)] <- 0

test[is.na(test)]   <- 0

feature.names <- names(train)[1:ncol(train)]

split <- createDataPartition(y = train_target, list = F, p = 0.9)

training <- train[split, ]

validation <- train[-split, ]

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label=train_target[split])

dval <- xgb.DMatrix(data.matrix(validation[,feature.names]), label=train_target[-split])

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

cl <- makeCluster(2); registerDoParallel(cl)

#clf_cv <- xgb.cv(params = param, data = dtrain, nrounds = 1500, nfold = 4, 
                 
#               showsd = T, verbose = T, maximize = T,  watchlist = watchlist)


rm(training, validation, cor_test, cor_train, test_pre_pred, train_pre_pred, test_cc, train_cc )

gc()

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2500, 
                    verbose             = 2, 
                    watchlist           = watchlist,
                    maximize            = TRUE)


submission <- data.frame(ID=test_ID)

submission$target <- NA 

submission[,"target"] <- predict(clf, data.matrix(test[,feature.names]))

write_csv(submission, "09182015_1.csv")

########################################################################################################################

models <- c()

for( i in 1:10) {

rand_num_trees<- sample(500:1000, 1 )

rand_max_depth <- sample(5:15, 1)

rand_learn_rate <- 0.025 * sample(1:10, 1)

rand_min_rows <- sample(1:10, 1)

model_name <- paste0( "GBM_", i,

		      "rand_num_trees",

	                        "rand_max_depth",

		       "rand_learn_rate",

		        "rand_min_rows" )

model_gbm <- h2o.gbm( x = feature.names,

		        y = "target",

		        training_frame = training.hex,

	                          model_id = model_name,

		        distribution = "binomial",

		       ntrees = rand_num_trees,

		       max_depth = rand_max_depth, 

		      min_rows  = rand_min_rows,

		      learn_rate = rand_learn_rate,

		      nfolds = 5
)

models <- c(models, model_gbm)

} 

#find the best model

best_error <- #tmp

for(i in 1:length(models)) {

err <- h2o.auc(h2o.performance(models[[i]], validation.hex))

if(err < tmp) {

best_error <- err

best_model <- models[[i]]

}

}

#show the best parameters and working model

params <- best_model@allparameters

params$ntrees
params$max_depth
params$min_rows
params$learn_rate

## training set performance metrics

h2o.auc(h2o.performance(best_model, training.hex))

## validation set performance metrics

h2o.auc(h2o.performance(best_model, validation.hex))


