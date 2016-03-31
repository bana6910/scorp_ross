#<<<<<<< HEAD
#Script to read, clean, merge, and the train.csv and test.csv with store.csv data sets for data munging

library(data.table)
library(zoo)

library(forecast)
library(ggplot2)

test <- fread("C:/Users/jcotrell/Documents/Project 1/scorp_ross/input_data/test.csv")
train <- fread("C:/Users/jcotrell/Documents/Project 1/scorp_ross/input_data/train.csv") #Note error is due to StateHoliday containing 'a' values
store <- fread("C:/Users/jcotrell/Documents/Project 1/scorp_ross/input_data/stores.csv")

#Convert date field to type date
  test$Date = as.Date(test$Date,"%m/%d/%Y") #Note there is a bug in the Rossman.zip file; you must open and save train.csv in excel before this works
  train$Date  = as.Date(train$Date,"%m/%d/%Y") #Note there is a bug in the Rossman.zip file; you must open and save train.csv in excel before this works

  
#Order the time series by date
  train <- train[order(Date)] 
  test <- test[order(Date)]


#Condition the data in store.csv before merging
  # In store.csv Convert the CompetitionOpenSince... variables to one Date variable
  store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                                 store$CompetitionOpenSinceMonth, sep = "-")) 
  # Convert the Promo2Since... variables to one Date variable
  # Assume that the promo starts on the first day of the week
  store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                        store$Promo2SinceWeek, 1, sep = "-"),
                                        format = "%Y-%U-%u")
# Merge store and train 
train_store <- merge(train, store, by = "Store")  #Will need to deal with NA's in data munging script

#converting categorical data in factors
train_store$Store= as.factor(train_store$Store)
train_store$DayOfWeek= as.factor(train_store$DayOfWeek)
train_store$Open = as.factor(train_store$Open)
train_store$Promo = as.factor(train_store$Promo)
train_store$StateHoliday = as.factor(train_store$StateHoliday)
train_store$SchoolHoliday = as.factor(train_store$SchoolHoliday)
train_store$StoreType = as.factor(train_store$StoreType)
train_store$Assortment = as.factor(train_store$Assortment)
train_store$Promo2 = as.factor(train_store$Promo2)
train_store$Sales = as.numeric(train_store$Sales) #JC addition, necessary for functions like sum()
train_store$PromoInterval=as.factor(train_store$PromoInterval) #JC addition

#merging test and store to apply model
test_store <- merge(test, store, by = "Store")

test_store$Store= as.factor(test_store$Store)
test_store$DayOfWeek= as.factor(test_store$DayOfWeek)
test_store$Open = as.factor(test_store$Open)
test_store$Promo = as.factor(test_store$Promo)
test_store$StateHoliday = as.factor(test_store$StateHoliday)
test_store$SchoolHoliday = as.factor(test_store$SchoolHoliday)
test_store$StoreType = as.factor(test_store$StoreType)
test_store$Assortment = as.factor(test_store$Assortment)
test_store$Promo2 = as.factor(test_store$Promo2)
test_store$PromoInterval=as.factor(test_store$PromoInterval) #JC addition

str(train)
str(test)
str(store)

str(test_store)
str(train_store)

