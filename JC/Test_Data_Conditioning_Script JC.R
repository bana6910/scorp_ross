#Script to read, merge, clean and write testing and store data sets

#Set Working Directory, Read and Check data sets
  setwd("C:/Users/jcotrell/OneDrive/BANA MS/6910 Practicum/Project 1/Github Project Directory/scorp_ross/input data")
  
  test<- read.csv("test.csv")
  stores<- read.csv("stores.csv")
  
  head(test); tail(test);summary(test); str(test)
  head(stores); tail(stores);summary(stores); str(stores)

#Merge and check testing and store data sets
  test.merged <- merge(test, stores, by.x="Store", by.y="Store")
  head(test.merged); tail(test.merged); str(test.merged)#NA's exist and some variable needs conversion


#Convert variables to proper variable types
  test.merged$Store = as.factor(test.merged$Store)
  test.merged$Date = as.Date(test.merged$Date,"%m/%d/%Y")
  test.merged$Open = as.factor(test.merged$Open)
  test.merged$Promo = as.factor(test.merged$Promo)
  test.merged$StateHoliday = as.factor(test.merged$StateHoliday)
  test.merged$SchoolHoliday = as.factor(test.merged$SchoolHoliday)
  test.merged$StoreType = as.factor(test.merged$StoreType)
  test.merged$Assortment = as.factor(test.merged$Assortment)
  test.merged$CompetitionOpenSinceYear = as.factor(test.merged$CompetitionOpenSinceYear)
  test.merged$Promo2 = as.factor(test.merged$Promo2)
  test.merged$Promo2SinceYear = as.factor(test.merged$Promo2SinceYear)
  
  head(test.merged); tail(test.merged);summary(test.merged); str(test.merged)


