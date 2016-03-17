#Script to perform predictions using the best model determined in the script Train_Model.R
#Run Condition_Data.R and Train_Models.R before this script

#Prediction Libraries
#library(lattice)
library(ggplot2)
library(caret)
#library(survival)
#library(splines)
#library(parallel)
#library(tree)
#library(randomForest)
library(gbm)
#library(bst)
#library(plyr)
#library(Cubist)

library(Metrics)# for RSME

# 
# #Predict sales for test.csv;
#   #Using mean.model, compute the mean on all data available to us in train.merged.csv but exclude zeros  
#       #mean.pred.test <- mean(train.merged.n0$Sales); mean.model #7063 
# 
#   #Using rf.model2, repeat training for  all data available to us in train.merged.csv and repeat predictions for test.csv
#        
# rf.model5 = randomForest(Sales ~ DayOfWeek +  Promo + promo2Days + StoreType +  CompetitionDistance, data = f_train_store.n0, mtry=2, ntree=5) 
#     rf.pred5.test = predict(rf.model5, newdata = f_test_store)
#  
# #Create the output dataframe, check the output, and write results
#   rf5.test.predictions <- cbind(test,rf.pred5.test)
#   names(rf5.test.predictions)
#   names(rf5.test.predictions)[9] <- "Sales"
#   head( rf5.test.predictions); summary(rf5.test.predictions); str(rf5.test.predictions)
#   qplot(rf5.test.predictions$Sales)
#   write.csv(rf5.test.predictions, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/Scorpian_rf5_predictions.csv")
#   
  #GBM Model
  #Repeat training for  all data available to us in train.merged.csv and repeat predictions for test.csv
  
  #Read data
  load("f_train_store.RData", verbose = FALSE)
  load("f_test_store.RData", verbose = FALSE)  
  
  su#Remove zeros
  f_train_store.n0 = f_train_store[f_train_store$Sales != 0,]
 
  #Replace sales in datasets with log(sales) to estimate RMSPE for the optimization
  f_train_store.n0$Sales = log(f_train_store.n0$Sales)  


  #GBM training
 
  fitControl <- trainControl(method = "CV",
                             repeats = 2,
                             number = 2,
                             verboseIter = TRUE
  )
  
  gbmGrid <-  expand.grid(interaction.depth = 35,
                          n.trees = 1300,
                          shrinkage = .1,
                          n.minobsinnode = 30
  )
  
  gbm.model<- train(Sales ~  DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance, 
                    data = f_train_store.n0,
                    method = "gbm",
                    metric = "RMSE",
                    trControl = fitControl,
                    tuneGrid = gbmGrid
  )
  
  save(gbm.model, file = "model_gbm4.RData")
  print(gbm.model) 
  Sys.time()
  
  #predictions
  gbm.pred = predict(gbm.model, newdata=f_test_store)
  
  #Create the output dataframe, check the output, and write results
  library(data.table)
  test <- fread("C:/Users/jcotrell/Documents/Project 1/scorp_ross/input_data/test.csv")
  gbm.test.predictions <- cbind(test,exp(gbm.pred))
  names(gbm.test.predictions)
  names(gbm.test.predictions)[9] <- "Sales"
  head( gbm.test.predictions); summary(gbm.test.predictions); str(gbm.test.predictions)
  qplot(gbm.test.predictions$Sales)
 
  
  write.csv(gbm.pred, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/Scorpian_gbm_predictions JC.csv")
  
  
  