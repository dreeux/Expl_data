#USE  STANDARD TIME FORMAT DEFINED AT THE BEGINING

time <- Sys.time()

format(time, "%d-%m-%Y--%H:%M:%S") 

##CALCULATING DIMENSIONS OF DATA WITHOUT READING IN



##READ IN DATA
require(readr)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

dim(train); dim(test)

##REMOVE RESPONSWE AND ID VARIABLES

response <- train$target

train_ID <- train$ID

test_ID <- test$ID

##MODIFY train and test set

training <- subset(train, select = -c(ID, target))

testing <- subset(test, select = -c(ID))

dim(training); dim(testing)

##U CAN CHOOSE TO COMBINE THEM FOR ANY MODS OR ANALYSE THEM SEPERATELY

##THIS CURRENT DOCUMENT CONTAINS SEPERATE ANALYSIS FOR BOTH TEST AND TRAIN

##CHECK FOR DUPLICATE ROWS AND FIND THE NUMBER OF UNIQUE VALUES IN COLUMNS 

##CHECKING FOR DUPLICATE ROWS

nrow(training) - nrow(unique(training))

##CHECKING FOR UNIQUE ELEMENTS IN A COLUMN

col_unique <- sapply(training, function(x) unique(x))

##DONT RUN MIGHT BE TO USEFUL AFTER SUBSETTING

##CHECKING FOR NUMBER OF UNIQUE VALUES IN A COLUMN

col_ele <- as.data.frame(lapply(training, function(x) length(unique(x))))

##GET THE COLUMN NAMES

col_eleNAMES <- names(col_ele)

##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

length(col_ele[col_ele == seq(1,3,1)])

length(col_ele[col_ele == 1])

length(col_ele[col_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(col_ele[col_ele == 3])

##CHECK COLUMNS WITH ONLY 1 UNIQUE VALUE

unique_one <- subset(col_ele , select = c(col_ele == 1))
 
unique_oneDF <- training[, c(names(unique_one))]

head(unique_oneDF)

##IDENTIFY AND SEPERATE NUMERIC AND NON NUMERIC COLUMNS

training_num <- training[, sapply(training, is.numeric)] #CHECK WHETHER LAPPLY WORKS

training_char <- training[,sapply(training, is.character)]

cat("Number of Numerical columns :" , dim(training_num)[2], "Number of character columns :",
    
    dim(training_char)[2])

##LOOK INTO NUMERIC COLUMNS DF

##CHECK FOR UNIQUE AND LENGTH OF UNIQUE COLUMNS

str(sapply(training_num, unique)) #TRY RUNNING WITHOUT STR

num_uniqueLEN <- sapply(training_num, function(x) length(unique(x))) #CHECK WITH LAPPLY

##################################################################################################

numeric_ele <- as.data.frame(lapply(training_num, function(x) length(unique(x))))

##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

length(numeric_ele[numeric_ele == 1])

length(numeric_ele[numeric_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(numeric_ele[numeric_ele == 3])

##CHECK COLUMNS WITH ONLY 1 UNIQUE VALUE

numeric_one <- subset(numeric_ele , select = c(numeric_ele == 1))

numeric_oneDF <- training_num[, c(names(numeric_one))]

##CHECK ELEMENTS

lapply(numeric_oneDF, table)

##CHECK COLUMNS WITH 2 UNIQUE VALUES

numeric_two <- subset(numeric_ele , select = c(numeric_ele == 2))

numeric_twoDF <- training_num[, c(names(numeric_two))]

##CHECK ELEMENTS

lapply(numeric_twoDF, table)

##DIG DEEPER INTO THE FIELDS

##DRAW A HISTOGRAM OF LENGTHS TO BETTER UNDERSTAND THE DATA

##LOOK INTO CHARACTER COLUMNS DF

str(lapply(training_char, unique), vec.len =4 )

char_ele <- as.data.frame(lapply(training_char, function(x) length(unique(x))))

##################################################################################
##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

range(char_ele)

length(char_ele[char_ele == 1])

length(char_ele[char_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(char_ele[char_ele == 3])

##SOME COLUMNS HAVE NAMES: FIRST SEPERATE DATES THEN LOOK INTO THEM 

##SEPERATE OUT DATES AND TIMES INTO  DIFFERENT DFS

training_date <- training_char[, grep("JAN1|FEB1|MAR1", training_char)]

##REMOVE DATES FROM char DF

training_charD <- training_char[, !(names(training_char) %in% names(training_date))] 

##training_charD has only CHAR variables checking further

str(lapply(training_charD, unique), vec.len =4 )

##SEPERATE FIELDS WITH BINARY VALUES

charD_ele <- as.data.frame(lapply(training_charD, function(x) length(unique(x))))

charD_two <- subset(charD_ele, select = c(charD_ele == 2))
names(charD_ele)

charD_twoDF <- training_char[, c(names(charD_two))]

##CHECK ELEMENTS OF DF

lapply(charD_twoDF, table)

##DRILL DOWN FURTHER BY REMOVING THESE COLS




#REPLACING (-1, " ", []) WITH NA

training_char[training_char == -1] = NA

training_char[training_char == ""] = NA

training_char[training_char == "[]"] = NA

