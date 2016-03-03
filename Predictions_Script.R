#Script to: 
  #1) read, clean, and merge the test.csv and store.csv data sets 
  #2) perform predictions using the best model determined in the script model_training
#Run Train_Data_Conditioning.R and Model_Training.R before this script

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