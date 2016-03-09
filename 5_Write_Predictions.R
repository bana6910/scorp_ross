#Script to perform predictions using the best model determined in the script Train_Model.R
#Run Condition_Data.R and Train_Models.R before this script


#Predict sales for test.csv;
  #Using mean.model, compute the mean on all data available to us in train.merged.csv but exclude zeros  
      #mean.pred.test <- mean(train.merged.n0$Sales); mean.model #7063 

  #Using rf.model2, repeat training for  all data available to us in train.merged.csv and repeat predictions for test.csv
       
rf.model5 = randomForest(Sales ~ DayOfWeek +  Promo + promo2Days + StoreType +  CompetitionDistance, data = f_train_store.n0, mtry=2, ntree=5) 
    rf.pred5.test = predict(rf.model5, newdata = f_test_store)
 
#Create the output dataframe, check the output, and write results
  rf5.test.predictions <- cbind(test,rf.pred5.test)
  names(rf5.test.predictions)
  names(rf5.test.predictions)[9] <- "Sales"
  head( rf5.test.predictions); summary(rf5.test.predictions); str(rf5.test.predictions)
  qplot(rf5.test.predictions$Sales)
  write.csv(rf5.test.predictions, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/Scorpian_rf5_predictions.csv")
  