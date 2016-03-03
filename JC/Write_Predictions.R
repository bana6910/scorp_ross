#Script to perform predictions using the best model determined in the script Train_Model.R
#Run Condition_Data.R and Train_Models.R before this script



#Predict sales for test.csv using the model mean.model 
  #Compute the mean on all data available to us in train.merged.csv but exclude zeros  
  mean.pred.test <- mean(train.merged.n0$Sales); mean.model #7063 

  
  
  
    
#Create the output dataframe, check the output, and write results
  test.predictions <- cbind(test,mean.pred.test)
  head(test.predictions); tail(test.predictions);summary(test.predictions); str(test.predictions)
  write.csv(test.predictions, file = "C:/Users/jcotrell/Documents/Project 1/scorp_ross/output/test_predictions.csv")
