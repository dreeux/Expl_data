library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)

test <- fread("C:/Users/amulya/Documents/Kaggle/forecast/test.csv")

train <- fread("C:/Users/amulya/Documents/Kaggle/forecast/train.csv")

store <- fread("C:/Users/amulya/Documents/Kaggle/forecast/store.csv")

str(train)

str(test)

str(store)

train[, Date := as.Date(Date)]

test[, Date := as.Date(Date)]

train <- train[order(Date)]

test <- test[order(Date)]

#combine data

train <- data.frame(train) ; test <- data.frame(test)

train <- left_join(train, store, by = "Store")

train <- filter(train, Open == 1) 

#################################################################################################

#Multiplot function

ggplot(train, aes( x = Customers, y =Sales)) + 
  geom_point() + labs(title = "Customers and Sales")

ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point(aes(colour = StoreType)) + labs(title = "Customers and Sales")

ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point(aes(colour = factor(DayOfWeek))) + labs(title = "Customers and Sales")

ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point(aes(colour = factor(Assortment))) + labs(title = "Customers and Sales")

ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point(aes(colour = factor(Promo), shape = factor(StoreType))) + 
  labs(title = "Customers and Sales")

ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point(aes(colour = StateHoliday)) + labs(title = "Customers and Sales")

ggplot(train, aes(x = Customers, y = Sales)) + 
  geom_point(aes(colour = PromoInterval)) + labs(title = "Customers and Sales")



################################################################################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



###################################################################################################





# change file names

require(sqldf); require(ggplot2); require(data.table);require(bit64); require(gridExtra)

Spanish2English <- read.csv("Spanish2English.csv", stringsAsFactors=FALSE)

df <- fread("page_1.csv",  data.table = F )

train <- fread("train.csv", data.table = F)

tmp <- cbind.data.frame( ID = train$ID, 
                         
                         var3 = train$var3, 
                         
                         var15 = train$var15, 
                         
                         var38 = train$var38, 
                         
                         TARGET = train$TARGET )


train <- train[, !(names(train) %in% names(tmp))]

names(train) <- Spanish2English_1$English

train <- cbind(train, tmp)



test <- fread("test.csv", data.table = F)

tmp <- cbind.data.frame( ID = test$ID, 
                         
                         var3 = test$var3, 
                         
                         var15 = test$var15, 
                         
                         var38 = test$var38 )


names(test) <- Spanish2English_1$English

test <- cbind(test, tmp)



train_con <- sqldf('select Variable_Name
                   
                   from df
                   
                   where Type_of_Variable = "Con"')


train_Cat <- sqldf('select Variable_Name
                   
                   from df
                   
                   where Type_of_Variable = "Cat"')

features <- train_con$Variable_Name

train.cont <- (train[, features])

train.cont$Response <- train$TARGET


# histogram for categorical variables-------------------------------------------------------------------------------------------------


plotHist <- function( data.in, i ) 
  
{
  data <- data.frame( x = data.in[,i] )
  
  p <- ggplot( data=data, aes(x=factor(x))) + 
    
    geom_histogram() + 
    
    xlab(colnames(data.in)[i]) + 
    
    theme_light() + 
    
    theme(axis.text.x=element_text(size=8))
  
  return (p)
  
}


doPlots <- function(data.in, fun, ii, ncol=3) 
  
{
  
  pp <- list()
  
  for (i in ii) 
    
  {
    
    p <- fun(data.in=data.in, i=i)
    
    pp <- c(pp, list(p))
    
  }
  
  do.call("grid.arrange", c(pp, ncol=ncol))
  
}


# densities for continous variables-------------------------------------------------------------------------------------------------


plotDensity <- function(data.in, i) 
  
{
  
  data <- data.frame(x=data.in[,i], Response=data.in$Response)
  
  p <- ggplot(data) + #geom_density(aes(x=x, colour=factor(Response))) + 
    
    geom_line(aes(x=x), stat="density", size=1, alpha=1.0) +
    
    xlab(colnames(data.in)[i]) + theme_light()
  
  return (p)
  
}


# Box plots of continous features depending on response-----------------------------------------------------------------------------


plotBox <- function(data.in, i) 
  
{
  data <- data.frame(y=data.in[,i], Response=data.in$Response)
  
  p <- ggplot(data, aes(x=factor(Response), y=y)) + 
    
    geom_boxplot() + 
    
    ylab(colnames(data.in)[i]) + 
    
    theme_light()
  
  return (p)
  
}


png(filename="name.png")

doPlots(data.in=train.cont, fun=plotDensity, ii=1:4, ncol=2)

dev.off()

##############################################################################################
##############################################################################################


library(ggplot2)

library(readr)

library(Rtsne)

features <- train[, c(-1, -371)]

tsne <- Rtsne( as.matrix(features), check_duplicates = FALSE, pca = TRUE, 
               
               perplexity=30, theta=0.5, dims=2 )

embedding <- as.data.frame(tsne$Y)

embedding$Class <- as.factor(train$Response)


ggplot(embedding, aes(x=V1, y=V2, color=Class)) +
  
  geom_point(size=1.25) +
  
  guides(colour = guide_legend(override.aes = list(size=6))) +
  
  xlab("") + ylab("") +
  
  ggtitle("t-SNE 2D Embedding") +
  
  theme_light(base_size=20) +
  
  theme(strip.background = element_blank(),
        
        strip.text.x     = element_blank(),
        
        axis.text.x      = element_blank(),
        
        axis.text.y      = element_blank(),
        
        axis.ticks       = element_blank(),
        
        axis.line        = element_blank(),
        
        panel.border     = element_blank()
        
  )


##############################################################################################
##############################################################################################
