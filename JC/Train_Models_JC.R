#Rossman Project Script to train models.  
#scripts to condition (merge and clean) both the training data and test data should have been run previously
 
  #Prediction Libraries
    library(lattice)
    library(ggplot2)
    #library(caret)
    library(survival)
    library(splines)
    library(parallel)
    #library(tree)
    library(randomForest)
    library(gbm)
    library(bst)
    library(plyr)
    library(Cubist)

  #Check Training Data
    head(train.merged)
    tail(train.merged)
    str(train.merged)
    dim(train.merged)
    summary(train.merged)
    colSums(is.na(train.merged))  #Check for NA (misssumming) value


  #Predictions
  
    #Compute mean of sales as a baseline prediction with zeros removed**************************
      mean.model <- mean(train_d.n0$Sales); mean.model #7070
      RMSPE.mean =  sqrt( (sum( (test_d.n0$Sales - mean.model)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.mean # Compute Root Mean Square Percentage Error (RMSPE)
    
   #Fitting a linear model*********************************************
     
     lm1.model = lm(Sales ~ DayOfWeek + StateHoliday + Assortment + StoreType, data = train_d.n0)
       #Notes 
          #I had an error when trying to use "Open" so I removed it
          #CompetitionDistance has NA values so I removedDayOfWeek 
          #Removed school holiday due to high P value
     
      summary(lm1.model) #State holiday b and C aren't significant, but leave them for now as removing doesn't help Adj R square .7954
     
       #Check quality of prediction (RMSPE) on test_d.n0$Sales
         lm1.pred <- predict(lm1.model, newdata = test_d.n0)
         head(lm1.pred); str(lm1.pred)
         qplot(lm1.pred)
         RMSPE.lm1 =  sqrt( (sum( (test_d.n0$Sales - lm1.pred)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.lm1 #111% 
     
 #Fitting a random forest tree********************************* 
         library(randomForest)
         
         #mtry=1
         rf.model = randomForest(Sales ~ DayOfWeek + StateHoliday + Assortment  + StoreType, data = train_d.n0, mtry=1, ntree=5)
         print(rf.model)
         importance(rf.model)
         
         rf.model.pred = predict(rf.model, newdata=test_d.n0)
         head(rf.model.pred); str(rf.model.pred)
         qplot(rf.model.pred)
         RMSPE.rf =  sqrt( (sum( (test_d.n0$Sales - rf.model.pred)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf #117%
         
         #mtry=2 (Bagging)
         rf.model2 = randomForest(Sales ~ DayOfWeek +  Assortment  + StoreType, data = train_d.n0, mtry=2, ntree=25)
         #removed state holiday due to factors of different levels in test.merged.
         print(rf.model2)
         importance(rf.model2)
         
         
         rf.pred2 = predict(rf.model2, newdata=test_d.n0)
         head(rf.pred2); str(rf.pred2)
         qplot(rf.pred2)
         RMSPE.rf2 =  sqrt( (sum( (test_d.n0$Sales - rf.pred2)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf2 #105%
         
         #mtry=2 (Bagging with all variables)
         rf.model3 = randomForest(Sales ~ DayOfWeek + StateHoliday + Promo + Assortment  + StoreType + SchoolHoliday, data = train_d.n0, mtry=2, ntree=25)
         #Competition Distance is removed due to missing values
         print(rf.model3)
         importance(rf.model3)
         
         rf.pred3 = predict(rf.model3, newdata=test_d.n0)
         head(rf.pred3); str(rf.pred3)
         qplot(rf.pred3)
         RMSPE.r32 =  sqrt( (sum( (test_d.n0$Sales - rf.pred3)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf3 #106%
         