#Script to perform predictions using the best model determined in the script Train_Model.R
#Run Condition_Data.R and Train_Models.R before this script


#Predict sales for test.csv;
  #Using mean.model, compute the mean on all data available to us in train.merged.csv but exclude zeros  
      #mean.pred.test <- mean(train.merged.n0$Sales); mean.model #7063 

  #Using rf.model2, repeat training for  all data available to us in train.merged.csv and repeat predictions for test.csv
       
    rf.model3.test = randomForest(Sales ~ DayOfWeek  + Assortment  + StoreType, data = f_train_store.n0, mtry=2, ntree=5)
    rf.pred3.test = predict(rf.model3.test, newdata = f_test_store)
   
    
#Create the output dataframe, check the output, and write results
  rf3.test.predictions <- cbind(test,rf.pred3.test)
  names(rf3.test.predictions)[9] <- "Sales"
  head( rf3.test.predictions); summary(rf3.test.predictions); str(rf3.test.predictions)
  qplot(rf3.test.predictions$Sales)
  write.csv(rf3.test.predictions, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/Scorpian_rf3_predictions.csv")
  