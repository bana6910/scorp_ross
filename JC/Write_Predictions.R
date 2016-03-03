#Script to perform predictions using the best model determined in the script Train_Model.R
#Run Condition_Data.R and Train_Models.R before this script



#Predict sales for test.csv;
  #Using mean.model, compute the mean on all data available to us in train.merged.csv but exclude zeros  
      #mean.pred.test <- mean(train.merged.n0$Sales); mean.model #7063 

  #Using rf.model2, repeat training for  all data available to us in train.merged.csv and repeat predictions for test.csv
       
    rf.model2.test = randomForest(Sales ~ DayOfWeek  + Assortment  + StoreType, data = train.merged.n0, mtry=2, ntree=25)
    rf.pred2.test = predict(rf.model2.test, newdata = test.merged)
   
    
  
    
#Create the output dataframe, check the output, and write results
  rf2.test.predictions <- cbind(test,rf.pred2.test)
  names(rf2.test.predictions)[9] <- "Sales"
  head( rf2.test.predictions); summary(rf2.test.predictions); str(rf2.test.predictions)
  qplot(rf2.test.predictions$Sales)
  write.csv(rf2.test.predictions, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/rf2.predictions")
  