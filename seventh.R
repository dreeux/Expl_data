library(caret); library(lubridate) ; library(readr) ; library(xgboost)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

##REMOVE RESPONSE AND ID VARIABLES

response <- train$target

train_ID <- train$ID

test_ID <- test$ID

##MODIFY train and test set

training <- subset(train, select = -c(ID, target))

testing <- subset(test, select = -c(ID))

dim(training); dim(testing)

########################################################################################

numeric_one <- subset(numeric_ele , subset  = c(numeric_ele == 1))

ids <- c("VAR_0227", "VAR_0228")

remove_col <- c(numeric_one, ids)

######################################################################################

tmp <- rbind(training, testing)

tmp <- tmp[, !(names(tmp) %in% (remove_col))]

######################################################################################
tmp_num <- tmp[, sapply(tmp, is.numeric)] 

tmp_char <- tmp[,sapply(tmp, is.character)]

num_ele <- (lapply(tmp_num, function(x) length(unique(x))))

char_ele <- (lapply(tmp_char, function(x) length(unique(x))))

#####################################################################################
tmp_date <- tmp_char[, grep("JAN1|FEB1|MAR1", tmp_char)]

tmp_dates <- sapply(tmp_date, function(x) strptime(x, "%d%B%y :%H:%M:%S"))

tmp_dates = do.call(cbind.data.frame, tmp_date)
####################################################################################

tmp_time <- tmp_date[, names(tmp_date) %in% c("VAR_0204","VAR_0217")]

tmp_times <- (sapply(tmp_time, function(x) strftime(x, "%H:%M:%S")))

tmp_times <- do.call(cbind.data.frame, tmp_times) 

tmp_hour <- (sapply(tmp_times, function(x) as.numeric(as.character(substr(x, 1,2)))))

tmp_hour <- do.call(cbind.data.frame, tmp_hour) 

######################################################################################

training <- tmp[1:145231,]

testing <- tmp[(nrow(training)+1): nrow(tmp), ]











feature.names <- names(training)

for (f in feature.names) {
  
  if (class(training[[f]])=="character") {
    
    levels <- unique(c(training[[f]], testing[[f]]))
    
    training[[f]] <- as.integer(factor(training[[f]], levels=levels))
    
    testing[[f]]  <- as.integer(factor(testing[[f]],  levels=levels))
    
  }
}
  
training[is.na(training)] <- -9999

testing[is.na(testing)]   <- -9999

benchmark <- read_csv("D:/kaggle/Springleaf/SUBMISSION/second.csv")

first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third_first.csv")

third <- read_csv("D:/kaggle/Springleaf/SUBMISSION/fourth.csv")

fourth <- read_csv("D:/kaggle/Springleaf/sixth.csv")

fifth <- read_csv("D:/kaggle/Springleaf/eight.csv")

feature_1 <- benchmark$target[1:145231] 

training$feature1 <- feature_1

testing$feature1 <- benchmark$target

feature_2 <- first$target[1:145231] 

training$feature2 <- feature_2

testing$feature2 <- first$target

feature_3 <- second$target[1:145231] 

training$feature3 <- feature_3

testing$feature3 <- second$target

feature_4 <- third$target[1:145231] 

training$feature4 <- feature_4

testing$feature4 <- third$target

feature_5 <- fourth$target[1:145231] 

training$feature5 <- feature_5

testing$feature5 <- fourth$target

feature_6 <- fifth$target[1:145231] 

training$feature6 <- feature_6

testing$feature6 <- fifth$target

feature.names <- names(training)

dtraining <- xgb.DMatrix(data.matrix(training[,feature.names]), label= response)

param <- list(  "objective"  = "binary:logistic"
                
                , "eval_metric" = "auc"
                
                , "eta" = 0.01
                
                , "subsample" = 0.7
                
                , "colsample_bytree" = 0.5
                
                , "min_child_weight" =6
                
                , "max_depth" = 9
                
                , "alpha" = 4
                
                , "nthreads" = 2
                
                , eval_metric         = "auc"
)



#cv <- xgb.cv(params = param,data =  dtraining,nrounds = 700, nfold = 5, showsd = T, metrics = "auc"
#, verbose = 2, maximize = TRUE)

clf_first <- xgb.training( params = param, 
                        
                        data                = dtraining, 
                        
                        nrounds             = 2000, # changed from 300
                        
                        verbose             = 2,
                        
                        nthread = 2,
                        
                        maximize = TRUE)

submission_second <- data.frame(ID=test_ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf_first, data.matrix(testing[,feature.names]))

write_csv(submission_second, "nine.csv")

xgb.save(clf_first, "xgb_nine.R")

