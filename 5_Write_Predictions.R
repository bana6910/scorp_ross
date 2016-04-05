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
library(data.table) #for fread

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
  f_test_store = setorder(f_test_store, Id,na.last=FALSE)  #Order Test Data by ID to align with test.csv
  
  #Remove unusual months
  f_train_store = f_train_store[f_train_store$Week %in% c(1:48)]
  
  #Remove zeros
  f_train_store.n0 = f_train_store[f_train_store$Sales != 0,]
 
  #Replace sales in datasets with log(sales) to estimate RMSPE for the optimization
  f_train_store.n0$Sales = log(f_train_store.n0$Sales)  

  #GBM training with CV
    # 
    # fitControl <- trainControl(method = "CV",
    #                            repeats = 2,
    #                            number = 2,
    #                            verboseIter = TRUE
    # )
    # 
    # gbmGrid <-  expand.grid(interaction.depth = 35,
    #                         n.trees = 1300,
    #                         shrinkage = .1,
    #                         n.minobsinnode = 30
    # )
    # 
    # gbm.model<- train(Sales ~  DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance, 
    #                   data = f_train_store.n0,
    #                   method = "gbm",
    #                   metric = "RMSE",
    #                   trControl = fitControl,
    #                   tuneGrid = gbmGrid
    # )
    # 
    # save(gbm.model, file = "model_gbm.RData")
    # print(gbm.model) 
    # Sys.time()
  
  #GBM training with no CV and Year added
  
    fitControl <- trainControl(method = "none",
                             #   number = 1,
                             verboseIter = TRUE
  )
  
  gbmGrid <-  expand.grid(interaction.depth = 20,
                          n.trees = 1000,
                          shrinkage = .1,
                          n.minobsinnode = 30
  )
  
  gbm.model<- train(Sales ~  DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance + Year, 
                    data = f_train_store.n0,
                    method = "gbm",
                    metric = "RMSE",
                    trControl = fitControl,
                    tuneGrid = gbmGrid
  )
  
  save(gbm.model, file = "model_gbm_Mar30B.RData")
  load(file = "model_gbm_Mar30B.RData")
  print(gbm.model)
  Sys.time()
  
  #predictions
  gbm.pred = predict(gbm.model, newdata=f_test_store)
  f_test_store$Sales = exp(gbm.pred)
  
  #Check results
  head( f_test_store); summary(f_test_store$Sales)
  qplot(f_test_store$Sales)
  boxplot(f_test_store$Sales)

  #Write results
  write.csv(f_test_store, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/Scorpian_gbm_predictions_March30D.csv")
  
  #Write subset of results
  f_test_store_Sm <- subset(f_test_store, select = c(Id, Sales))
  write.csv(f_test_store_Sm, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/Scorpian_gbm_predictions_March30D_Sm.csv")
  