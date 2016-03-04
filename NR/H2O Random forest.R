
str(f_train_store)
str(f_test_store)


f_train_store$CompetitionOpenSinceYear = as.integer (f_train_store$CompetitionOpenSinceYear)
f_train_store$Promo2SinceYear = as.integer (f_train_store$Promo2SinceYear)
f_train_store$CompetitionOpenSince = as.integer (f_train_store$CompetitionOpenSince)
f_train_store$promo2Days = as.integer (f_train_store$promo2Days)

f_train_store$Sales = as.numeric (f_train_store$Sales)

f_test_store$CompetitionOpenSinceYear = as.integer (f_test_store$CompetitionOpenSinceYear)
f_test_store$Promo2SinceYear = as.integer (f_test_store$Promo2SinceYear)
f_test_store$CompetitionOpenSince = as.integer (f_test_store$CompetitionOpenSince)
f_test_store$promo2Days = as.integer (f_test_store$promo2Days)

f_train_store$logSales = log1p(f_train_store$Sales)


## trying Random forest from Jamies work
#######################################################3

## Jamie's RF Stuff
library(ElemStatLearn)
library(MASS)
library(randomForest)
library(tree)

rf_rossmann = randomForest(Sales ~ DayOfWeek + Store + DayOfWeek + Date + Sales + Customers + Promo + Open + StateHoliday + SchoolHoliday + StoreType + Assortment + CompetitionDistance + CompetitionOpenSinceMonth + CompetitionOpenSinceYear+ Promo2 + Promo2SinceWeek + Promo2SinceYear + PromoInterval + CompetitionOpenSince + Promo2Since + OpenonStateHoliday + promo2Days + schoolOff ,
                           data=f_train_store, ntree=20, mtry=4, na.action=na.omit)

rf_predict = predict(rf_rossmann, f_test_store, method ="class")

varImpPlot(rf_rossmann)
plot(rf_rossmann, log="y")

rfsubmit <- data.frame(Id = f_test_store$Id, Sales = rf_predict)
write.csv(rfsubmit, file = "rfsubmission.csv", row.names = FALSE)
#################################################################################


library(h2o)

h2o.init(nthreads=-1,max_mem_size='12G')

f_train_store[!complete.cases(f_train_store),]
colSums(is.na(f_train_store))

trainHex<-as.h2o(f_train_store)
features<-colnames(f_train_store)[!(colnames(f_train_store) %in% c("StateHoliday", "SchoolHoliday", "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear",  "Promo2SinceWeek", "Promo2SinceYear", "PromoInterval", "CompetitionOpenSince", "Promo2Since", "OpenonStateHoliday", "promo2Days", "schoolOff"))]

rfHex <- h2o.randomForest(x = features,
                          y = "logSales", 
                          ntrees = 100,
                          max_depth = 30,
                          nbins_cats = 1000,
                          training_frame=trainHex)

summary(rfHex)

testHex<-as.h2o(f_test_store)

predictions<-as.data.frame(h2o.predict(rfHex,testHex))

pred <- expm1(predictions[,1])
str(f_test_store)

submission <- data.frame(Id=f_test_store$Id, Sales=pred)
write.csv(submission, "h2o_rf.csv",row.names=F)

#################################################################################3

h2o.init(nthreads=-1)
trainHex<-as.h2o(f_train_store)
f_train_store$logSales = log1p(f_train_store$Sales)

rfHex <- h2o.randomForest(x=c(1:3,5:ncol(f_train_store)),
                          y="logSales", 
                          training_frame=trainHex)


