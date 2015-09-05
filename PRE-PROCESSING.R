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

col_ele <- sapply(training, function(x) length(unique(x)))

##GET THE COLUMN NAMES

col_eleNAMES <- names(col_ele)

##CHECK FOR COLUMNS WITH 1,2,3 UNIQUE ELEMENTS

length(col_ele[col_ele == seq(1,3,1)])

length(col_ele[col_ele == 1])

length(col_ele[col_ele == 2]) #MOSTLY COLUMNS WITH DUMMY VARS

length(col_ele[col_ele == 3])

##CHECK COLUMNS WITH ONLY 1 UNIQUE VALUE

unique_one <- col_eleNAMES[col_ele[col_ele == 1]]
 
unique_oneDF <- training[, c(unique_one)]
head(unique_oneDF)

##IDENTIFY AND SEPERATE NUMERIC AND NON NUMERIC COLUMNS

training_num <- training[, sapply(training, is.numeric)] #CHECK WHETHER LAPPLY WORKS

training_char <- training[,sapply(training, is.character)]

cat("Number of Numerical columns :" , dim(training_num[2]), "Number of character columns :",
    
    dim(training_char[2]))

##LOOK INTO NUMERIC COLUMNS DF

##CHECK FOR UNIQUE AND LENGTH OF UNIQUE COLUMNS

num_unique <- str(lapply(training_num, unique), vec.len = 4) #TRY RUNNING WITHOUT STR

num_uniqueLEN <- sapply(training_num, length(unique)) #CHECK WITH LAPPLY

##DIG DEEPER INTO THE FIELDS

##DRAW A HISTOGRAM OF LENGTHS TO BETTER UNDERSTAND THE DATA

#######LOTS OF TO BE DONE WRK WITH DATA##################################


##LOOK INTO CHARACTER COLUMNS DF

char_unique <- str(lapply(training_char, unique), vec.len =4 )

char_uniqueLEN <- sapply(training_char, length(unique))

##DIG DEEPER INTO THE FIELDS

#REPLACING (-1, " ", []) WITH NA

training_char[training_char == -1] = NA

training_char[training_char == ""] = NA

training_char[training_char == "[]"] = NA

##SEPERATE OUT DATES AND TIMES INTO  DIFFERENT DFS

training_date <- training_char[, grep("JAN1|FEB1|MAR1", training_char),]

##REMOVE DATES FROM char DF

training_charD <- training_char[, !(names(training_char) %in% names(training_date))] 
#CHECK THE DISSERENCE BETWEEN colnames and names

training_date