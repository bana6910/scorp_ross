#Rossman Project Script to train models.  
#scripts to condition (merge and clean) both the training data and test data should have been run previously
 
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
    
    #function for rmspe
    rmspe <- function(truth_y, pred_y) {
      nonzero <- truth_y > 0
      n <- sum(nonzero)
      diff <- (truth_y[nonzero] - pred_y[nonzero]) / truth_y[nonzero]
      diff <- diff**2
      diff <- sqrt(sum(diff)/n)
      return(diff)
    }    
    

  #Check Training Data
    # head(train.merged)
    # tail(train.merged)
    # str(train.merged)
    # dim(train.merged)
    # summary(train.merged)
    # colSums(is.na(train.merged))  #Check for NA (misssumming) value

    
#Read data from 3_Data_Munging.R
    load("f_train_store.RData", verbose = FALSE)
    load("f_test_store.RData", verbose = FALSE)  
    
    
    

 #Break training data in 1/2 for training models and testing models
    set.seed(123) #set a seed to provide some repeatability when sampling
    
    fraction = .1
    train_ind = sample(nrow(f_train_store), nrow(f_train_store)*fraction) #creates a training index vector of size nrow*fraction.
    train_d = f_train_store[train_ind,]#create the training and test data sets by pulling all indexed rows from d 
    test_d = f_train_store[-train_ind,]
    
    #Remove zeros from test_d consistent with test data provided by Daria
    test_d.n0 = test_d[test_d$Sales != 0,]
    summary(test_d.n0)
    
    #Repeat for training data for convenience later
    train_d.n0 = train_d[train_d$Sales != 0,]
    summary(train_d.n0)
    
    #Repeat for all training data for convenience later
    #f_train_store.n0 = f_train_store[f_train_store$Sales != 0,]
    #summary(train_d.n0)
    
    #Replace sales in datasets with log(sales) to estimate RMSPE for the optimization
    test_d.n0$Sales = log(test_d.n0$Sales) 
    train_d.n0$Sales = log(train_d.n0$Sales) 
    #f_train_store.n0$Sales = log(train_d.n0$Sales)
    
    
    
 #  #Predictions
 #  
 #    #Compute mean of sales as a baseline prediction with zeros removed**************************
 #      mean.model <- mean(train_d.n0$Sales); mean.model #7070
 #      RMSPE.mean =  sqrt( (sum( (test_d.n0$Sales - mean.model)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.mean # Compute Root Mean Square Percentage Error (RMSPE) 121
 #    
 #   #Fitting a linear model*********************************************
 #     
 #     lm1.model = lm(Sales ~ DayOfWeek + StateHoliday + Assortment + StoreType + Promo + CompetitionDistance + CompetitionOpenSince + promo2Days, data = train_d.n0)
 #       #Notes 
 #          #I had an error when trying to use "Open" so I removed it
 #          #Removed school holiday due to high P value
 #          #Note Promo2Days is not valuable 
 #          #CompetitionOPenSince is not valuable
 #     
 #      summary(lm1.model) #State holiday b and C aren't significant, but leave them for now as removing doesn't help Adj R square .7954
 #     
 #       #Check quality of prediction (RMSPE) on test_d.n0$Sales
 #         lm1.pred <- predict(lm1.model, newdata = test_d.n0)
 #         head(lm1.pred); str(lm1.pred)
 #         qplot(lm1.pred)
 #         RMSPE.lm1 =  sqrt( (sum( (test_d.n0$Sales - lm1.pred)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.lm1 #92% 
 #     
 # #Fitting a random forest tree********************************* 
 #         library(randomForest)
 #         
 #        
 #         #mtry=2 (Bagging)
 #         rf.model2 = randomForest(Sales ~ DayOfWeek + Assortment + StoreType + Promo + promo2Days, data = train_d.n0, mtry=2, ntree=5)
 #         #removed state holiday due to factors of different levels in test.merged.
 #         print(rf.model2)
 #         importance(rf.model2)
 #         
 #         
 #         rf.pred2 = predict(rf.model2, newdata=test_d.n0)
 #         head(rf.pred2); str(rf.pred2)
 #         qplot(rf.pred2)
 #         RMSPE.rf2 =  sqrt( (sum( (test_d.n0$Sales - rf.pred2)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf2 #88%
 #         
 #         #mtry=2 (Bagging with all variables)
 #         rf.model3 = randomForest(Sales ~ DayOfWeek + StateHoliday + Promo + Assortment  + StoreType + SchoolHoliday + promo2Days + CompetitionDistance, data = train_d.n0, mtry=2, ntree=5)
 #         print(rf.model3)
 #         importance(rf.model3)
 #         
 #         rf.pred3 = predict(rf.model3, newdata=test_d.n0)
 #         head(rf.pred3); str(rf.pred3)
 #         qplot(rf.pred3)
 #         RMSPE.rf3 =  sqrt( (sum( (test_d.n0$Sales - rf.pred3)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf3 #81%
 #         
 #         #Remove SchoolHoliday & StateHoliday
 #         rf.model3 = randomForest(Sales ~ DayOfWeek +  Promo + Assortment  + StoreType +  CompetitionDistance, data = train_d.n0, mtry=2, ntree=25)
 #         #note promo2Days was making this run slow "30 min", but improve the prediction by roughly 10%
 #         print(rf.model3)
 #         importance(rf.model3)
 #         
 #         rf.pred3 = predict(rf.model3, newdata=test_d.n0)
 #         head(rf.pred3); str(rf.pred3)
 #         qplot(rf.pred3)
 #         RMSPE.rf3 =  sqrt( (sum( (test_d.n0$Sales - rf.pred3)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf3 #83%
 #         
    #Change variable type of promo2Days and add back to model
    #train_d.n0$promo2Days = as.numeric(train_d.n0$promo2Days)
    #test_d.n0$promo2Days = as.numeric(test_d.n0$promo2Days)
    #         
    #         rf.model4 = randomForest(Sales ~ DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance, data = train_d.n0, mtry=2, ntree=25)
    #         print(rf.model4)
    #         importance(rf.model4)
    #         
    #         rf.pred4 = predict(rf.model4, newdata=test_d.n0)
    #         head(rf.pred4); str(rf.pred4)
    #         qplot(rf.pred4)
    #         RMSPE.rf4 =  sqrt( (sum( (test_d.n0$Sales - rf.pred4)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf4 #75%
    #         #Note 25 trees appears to be too many; 5 trees reduced the error by 5%
    
    # #Remove assortment and reduce number of trees to 5
    # rf.model5 = randomForest(Sales ~   Promo + promo2Days+CompetitionDistance, data = train_d.n0, mtry=2, ntree=5)
    # print(rf.model5)
    # importance(rf.model5)
    # 
    # rf.pred5 = predict(rf.model5, newdata=test_d.n0)
    # head(rf.pred5); str(rf.pred5)
    # qplot(rf.pred5)
    # RMSPE.rf5 =  sqrt( (sum( (test_d.n0$Sales - rf.pred5)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.rf5 #74%
    
    #Try the caret package

  
    
    # rf.model6 = train(Sales ~  CompetitionDistance, 
    #                   method = "rf",
    #                   data = train_d.n0, 
    #                   trControl=fitControl)
    # print(rf.model6)
    
    
    # #Created new stochastic gradient boosting model
    #   gbm.model = train(test_d.n0$Sales ~  Promo + promo2Days + CompetitionDistance, 
    #                     data = train_d.n0, 
    #                     method = "gbm",
    #                     trControl=fitControl,
    #                     verbose = TRUE)
    #   print(gbm.model)
    #   #The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1
    #   #and n.minobsinnode = 10. 
    #   plot(gbm.model)
    #   
    #   #Created new boosted tree model
    #   bt.model <- train(test_d.n0$Sales ~  Promo + promo2Days + CompetitionDistance, 
    #                     data = train_d.n0,
    #                     method='bstTree',
    #                     trControl=fitControl)
    #   print(bt.model)
    #   #The final values used for the model were mstop = 150, maxdepth = 3 and nu = 0.1.
    #   plot(bt.model)
    
    #Cubist Model
  
       #  fitControl = trainControl(method='CV', #use cross validation
       #                        number=2, #set the number of folds
       #                        summaryFunction = defaultSummary, 
       #                        repeats = 1,
       #                        verboseIter = TRUE,
       #                        classProbs = FALSE)
       # 
       # 
       # 
       # cb.model <- train(Sales ~  DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance, 
       #                    data = train_d.n0,
       #                    method='cubist', 
       #                    trControl=fitControl)
       # 
       #  save(cb.model, file = "model_cb7.RData")
       #  print(cb.model) 
       #  plot(cb.model) #The final values used for the model were committees = 20 and neighbors = 5. 
       #  
       #  #Comput RSME for log of sales value (the estimate for RMSPE)
       #  cb.pred6 = predict(cb.model, newdata=test_d.n0)
       #  rmse.cb.pred6 = rmse(test_d.n0$Sales, cb.pred6);rmse.cb.pred6 #29.41%
       #  
       #  #Check that RMSPE is being computed correctly
       # # RMSPE.cb6 =  sqrt( (sum( (test_d.n0$Sales - cb.pred6)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.cb6 #56%
       #  actual = exp(test_d.n0$Sales)
       #  predicted =exp(cb.pred6)
       #  RMSPE.cb6 = rmspe(actual,predicted); RMSPE.cb6 #36.86%
       #  
       #  Sys.time()
       #  
    # #GBM Model
    # 
    # 
    #   
    # 
    #   fitControl <- trainControl(method = "CV",
    #       repeats = 1,
    #       number = 2,
    #       verboseIter = TRUE
    #     )
    #     
    #     gbmGrid <-  expand.grid(interaction.depth = c(11,15,20),
    #                             n.trees = c(400,800),
    #                             shrinkage = c(0.1,.01),
    #                             n.minobsinnode = 30
    #     )
    #     
    #     gbm.model<- train(Sales ~  DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance, 
    #                      data = train_d.n0,
    #                      method = "gbm",
    #                      metric = "RMSE",
    #                      trControl = fitControl,
    #                      tuneGrid = gbmGrid
    #     )
    #     
    #     save(gbm.model, file = "model_gbm.RData")
    #     print(gbm.model) 
    #     plot(gbm.model) 
    #     
    #     #Comput RSME for log of sales value (the estimate for RMSPE)
    #     gbm.pred = predict(gbm.model, newdata=test_d.n0)
    #     rmse.gbm.pred = rmse(test_d.n0$Sales, gbm.pred);rmse.gbm.pred #21%
    #     
    #     #Check that RMSPE is being computed correctly
    #     # RMSPE.cb6 =  sqrt( (sum( (test_d.n0$Sales - cb.pred6)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.cb6 #56%
    #     actual = exp(test_d.n0$Sales)
    #     predicted =exp(gbm.pred)
    #     RMSPE.gbm = rmspe(actual,predicted); RMSPE.gbm #28%
    #     
    #     Sys.time()
    
    #GBM Model 3
    
    fitControl <- trainControl(method = "CV",
                               repeats = 1,
                               number = 2,
                               verboseIter = TRUE
    )
    
    gbmGrid <-  expand.grid(interaction.depth = 35,
                            n.trees = 1500,
                            shrinkage = .1,
                            n.minobsinnode = 30
    )
    
    gbm.model<- train(Sales ~  DayOfWeek +  Promo + promo2Days + Assortment  + StoreType +  CompetitionDistance, 
                      data = train_d.n0,
                      method = "gbm",
                      metric = "RMSE",
                      trControl = fitControl,
                      tuneGrid = gbmGrid
    )
    
    save(gbm.model, file = "model_gbm3.RData")
    print(gbm.model) 
    plot(gbm.model) 
    
    #Comput RSME for log of sales value (the estimate for RMSPE)
    gbm.pred = predict(gbm.model, newdata=test_d.n0)
    rmse.gbm.pred = rmse(test_d.n0$Sales, gbm.pred);rmse.gbm.pred #19.8%
    
    #Check that RMSPE is being computed correctly
    # RMSPE.cb6 =  sqrt( (sum( (test_d.n0$Sales - cb.pred6)/test_d.n0$Sales )^2 ) / nrow(test_d.n0) ); RMSPE.cb6 #56%
    actual = exp(test_d.n0$Sales)
    predicted =exp(gbm.pred)
    RMSPE.gbm = rmspe(actual,predicted); RMSPE.gbm #27%
    
    Sys.time()