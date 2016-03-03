#Script to read, clean, merge, the training and store data sets for model training purposes

#Set Working Directory, Read and Check data sets
  setwd("C:/Users/jcotrell/Documents/Project 1/scorp_ross/input data")
  
  train<- read.csv("train.csv")
  stores<- read.csv("stores.csv")
  
  head(train); tail(train);summary(train); str(train)
  head(stores); tail(stores);summary(stores); str(stores)

#Merge and check training and store data sets
  train.merged <- merge(train, stores, by.x="Store", by.y="Store")
  head(train.merged); tail(train.merged); str(train.merged)#NA's exist and some variable needs conversion


#Convert variables to proper variable types
  train.merged$Store = as.factor(train.merged$Store)
  train.merged$Date = as.Date(train.merged$Date,"%m/%d/%Y")
  train.merged$Open = as.factor(train.merged$Open)
  train.merged$Promo = as.factor(train.merged$Promo)
  train.merged$StateHoliday = as.factor(train.merged$StateHoliday)
  train.merged$SchoolHoliday = as.factor(train.merged$SchoolHoliday)
  train.merged$StoreType = as.factor(train.merged$StoreType)
  train.merged$Assortment = as.factor(train.merged$Assortment)
  train.merged$CompetitionOpenSinceYear = as.factor(train.merged$CompetitionOpenSinceYear)
  train.merged$Promo2 = as.factor(train.merged$Promo2)
  train.merged$Promo2SinceYear = as.factor(train.merged$Promo2SinceYear)
  train.merged$Sales = as.numeric(train.merged$Sales) #necessary for functions like sum()
  
  head(train.merged); tail(train.merged);summary(train.merged); str(train.merged)

  #Break training data in 1/2 for training models and testing models 
    train_ind = sample(nrow(train.merged), nrow(train.merged)/2) #creates a training index vector of size nrow/2.
    train_d = train.merged[train_ind,]#create the training and test data sets by pulling all indexed rows from d 
    test_d = train.merged[-train_ind,]
    #Remove zeros from test_d consistent with test data provided by Daria
      test_d.n0 = test_d[test_d$Sales != 0,]
      summary(test_d.n0)
    #Repeat for training data for convenience later
      train_d.n0 = train_d[train_d$Sales != 0,]
      summary(train_d.n0)
