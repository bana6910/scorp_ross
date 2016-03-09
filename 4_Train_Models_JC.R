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
    # head(train.merged)
    # tail(train.merged)
    # str(train.merged)
    # dim(train.merged)
    # summary(train.merged)
    # colSums(is.na(train.merged))  #Check for NA (misssumming) value


 #Break training data in 1/2 for training models and testing models (this should be changed to 75%/25%) 
    set.seed(123) #set a seed to provide some repeatability when sampling
    train_ind = sample(nrow(f_train_store), nrow(f_train_store)/2) #creates a training index vector of size nrow/2.
    train_d = f_train_store[train_ind,]#create the training and test data sets by pulling all indexed rows from d 
    test_d = f_train_store[-train_ind,]
    #Remove zeros from test_d consistent with test data provided by Daria
    test_d.n0 = test_d[test_d$Sales != 0,]
    summary(test_d.n0)
    #Repeat for training data for convenience later
    train_d.n0 = train_d[train_d$Sales != 0,]
    summary(train_d.n0)
    #Repeat for all training data for convenience later
    f_train_store.n0 = f_train_store[f_train_store$Sales != 0,]
    summary(train_d.n0)
    
    
  #Predictions
  
    #Compute mean of sales as a baseline prediction with zeros removed**************************
      mean.model <- mean(train_d.n0$Sales); mean.model #7070
      RMSPE.mean =  sqrt( (sum( (test_d.n0$Sales - mean.model)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.mean # Compute Root Mean Square Percentage Error (RMSPE) 121
    
   #Fitting a linear model*********************************************
     
     lm1.model = lm(Sales ~ DayOfWeek + StateHoliday + Assortment + StoreType + Promo + CompetitionDistance + CompetitionOpenSince + promo2Days, data = train_d.n0)
       #Notes 
          #I had an error when trying to use "Open" so I removed it
          #Removed school holiday due to high P value
          #Note Promo2Days is not valuable 
          #CompetitionOPenSince is not valuable
     
      summary(lm1.model) #State holiday b and C aren't significant, but leave them for now as removing doesn't help Adj R square .7954
     
       #Check quality of prediction (RMSPE) on test_d.n0$Sales
         lm1.pred <- predict(lm1.model, newdata = test_d.n0)
         head(lm1.pred); str(lm1.pred)
         qplot(lm1.pred)
         RMSPE.lm1 =  sqrt( (sum( (test_d.n0$Sales - lm1.pred)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.lm1 #92% 
     
 #Fitting a random forest tree********************************* 
         library(randomForest)
         
        
         #mtry=2 (Bagging)
         rf.model2 = randomForest(Sales ~ DayOfWeek + Assortment + StoreType + Promo + promo2Days, data = train_d.n0, mtry=2, ntree=5)
         #removed state holiday due to factors of different levels in test.merged.
         print(rf.model2)
         importance(rf.model2)
         
         
         rf.pred2 = predict(rf.model2, newdata=test_d.n0)
         head(rf.pred2); str(rf.pred2)
         qplot(rf.pred2)
         RMSPE.rf2 =  sqrt( (sum( (test_d.n0$Sales - rf.pred2)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf2 #88%
         
         #mtry=2 (Bagging with all variables)
         rf.model3 = randomForest(Sales ~ DayOfWeek + StateHoliday + Promo + Assortment  + StoreType + SchoolHoliday + promo2Days + CompetitionDistance, data = train_d.n0, mtry=2, ntree=5)
         print(rf.model3)
         importance(rf.model3)
         
         rf.pred3 = predict(rf.model3, newdata=test_d.n0)
         head(rf.pred3); str(rf.pred3)
         qplot(rf.pred3)
         RMSPE.rf3 =  sqrt( (sum( (test_d.n0$Sales - rf.pred3)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf3 #81%
         
         #Remove SchoolHoliday & StateHoliday
         rf.model3 = randomForest(Sales ~ DayOfWeek +  Promo + Assortment  + StoreType +  promo2Days + CompetitionDistance, data = train_d.n0, mtry=2, ntree=5)
         #note promo2Days is making this run slow
         print(rf.model3)
         importance(rf.model3)
         
         rf.pred3 = predict(rf.model3, newdata=test_d.n0)
         head(rf.pred3); str(rf.pred3)
         qplot(rf.pred3)
         RMSPE.rf3 =  sqrt( (sum( (test_d.n0$Sales - rf.pred3)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf3 #74%
         