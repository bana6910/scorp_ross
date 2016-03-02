#Rossman Project Script to perform predictions.  
#scripts to condition (merge and clean) both the training data and test data should have been run previously


  #Set Working Directory, Read Data, and Check Training Data
    data.train <- train.merged
   
    head(data.train)
    tail(data.train)
    str(data.train)
    dim(data.train)
    summary(data.train)
    colSums(is.na(data.train))  #Check for NA (misssumming) value
  
  #Prediction Libraries
    library(lattice)
    library(ggplot2)
    #library(caret)
    library(survival)
    library(splines)
    library(parallel)
    library(randomForest)
    library(gbm)
    library(bst)
    library(plyr)
    library(Cubist)
  
  
    #Computing the mean of sales as a baseline**************************
    mean.pred <- mean(data.train$Sales) #5859.024
    
    
    
    #Fitting a linear model*********************************************
    lm.model = train(power.mean ~ ., 
                     data = data.train, 
                     method = "lm", 
                     trControl=fitControl)
    print(lm.model)
    
    lm.model.pred <- predict(lm.model, newdata = data.test)
    mean((lm.model.pred-data.test$power.mean)^2) #42759.209
    sqrt(mean((lm.model.pred-data.test$power.mean)^2)) #206.783
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=lm.model.pred, color="predictions")) +
      ggtitle("Linear Predictions") +
      theme(legend.title=element_blank())
    
#     #Fitting a regression tree********************************* 
#     library(ggplot2)
#     library(tree)
#     
#     #Create train/test set using Andy's method 
#     set.seed(123)
#     ntrain = floor(nrow(data.in)/2) 
#     train = rep(FALSE,nrow(data.in))
#     train[sample(nrow(data.in), ntrain)] = TRUE
#     data.train = data.in[train == TRUE,]
#     data.test = data.in[train == FALSE,]
#     
#     #Fit the tree to the training data
#     tree.model=tree(power.mean~., data.train)
#     summary(tree.model)
#     
#     #Plot the tree
#     plot(tree.model)
#     text(tree.model,pretty=0)
#     
#     #Use tree to make predictions to the train set
#     tree.model.trn.predictions=predict(tree.model,newdata=data.train)
#     plot(tree.model.trn.predictions,data.train$power.mean)
#     abline(0,1)
#     mean((tree.model.trn.predictions-data.train$power.mean)^2) #MSE=9368.11
#     sqrt(mean((tree.model.trn.predictions-data.train$power.mean)^2)) #RMSE=96.79
#     
#     #Use tree to make predictions to the test set
#     tree.model.tst.predictions=predict(tree.model,newdata=data.test)
#     plot(tree.model.tst.predictions,data.test$power.mean)
#     abline(0,1)
#     mean((tree.model.tst.predictions-data.test$power.mean)^2) #MSE=9775.94
#     sqrt(mean((tree.model.tst.predictions-data.test$power.mean)^2)) #RMSE=98.87
    
    
#     #Create random forest model*******************************************************
#     rf.model = train(power.mean ~ ., 
#                      method = "rf",
#                      data = data.train, 
#                      trControl=fitControl)
#     print(rf.model)
#     #The final value used for the model was mtry = 3. 
#     plot(rf.model)
#     
#     rf.model.pred <- predict(rf.model, newdata = data.test)
#     mean((rf.model.pred-data.test$power.mean)^2) #256.756
#     sqrt(mean((rf.model.pred-data.test$power.mean)^2)) #16.024
#     
#     ggplot(data.test, aes(x=ws.HH)) + 
#       geom_point(aes(y=power.mean, color="actual power")) + 
#       geom_point(aes(y=rf.model.pred, color="predictions")) +
#       ggtitle("Random Forest Predictions") +
#       theme(legend.title=element_blank())
#     
#     #Created new stochastic gradient boosting model
#     gbm.model = train(power.mean ~ ., 
#                       data = data.train, 
#                       method = "gbm",
#                       trControl=fitControl,
#                       verbose = FALSE)
#     print(gbm.model)
#     #The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1
#     #and n.minobsinnode = 10. 
#     plot(gbm.model)
#     
#     gbm.model.pred <- predict(gbm.model, newdata = data.test)
#     mean((gbm.model.pred-data.test$power.mean)^2) #342.370
#     sqrt(mean((gbm.model.pred-data.test$power.mean)^2)) #18.503
#     #While MSE and RMSE can be improved upon by changing the final values above, the model becomes 
#     #quite overfit.
#     
#     ggplot(data.test, aes(x=ws.HH)) + 
#       geom_point(aes(y=power.mean, color="actual power")) + 
#       geom_point(aes(y=gbm.model.pred, color="predictions")) +
#       ggtitle("Stochastic Gradient Boosting Predictions") +
#       theme(legend.title=element_blank())
#     
#     #Created boosted tree model***************************************************
#     bt.model <- train(power.mean ~ ., 
#                       data = data.train,
#                       method='bstTree',
#                       trControl=fitControl)
#     print(bt.model)
#     #The final values used for the model were mstop = 150, maxdepth = 3 and nu = 0.1.
#     plot(bt.model)
#     
#     bt.pred <- predict(bt.model, newdata = data.test)
#     mean((bt.pred-data.test$power.mean)^2) #241.578
#     sqrt(mean((bt.pred-data.test$power.mean)^2)) #15.543
#     
#     ggplot(data.test, aes(x=ws.HH)) + 
#       geom_point(aes(y=power.mean, color="actual power")) + 
#       geom_point(aes(y=bt.pred, color="predictions")) +
#       ggtitle("Boosted Tree Predictions") +
#       theme(legend.title=element_blank())
#     
#     #Created new cubist model which is producing the BEST results.*******************
#     cb.model <- train(power.mean ~ ., 
#                       data = data.train,
#                       method='cubist', 
#                       trControl=fitControl)
#     print(cb.model)
#     #The final values used for the model were committees = 20 and neighbors = 5. 
#     plot(cb.model)
#     
#     cb.pred <- predict(cb.model, newdata = data.test)
#     mean((cb.pred-data.test$power.mean)^2) #25.885
#     sqrt(mean((cb.pred-data.test$power.mean)^2)) #5.088
#     
#     ggplot(data.test, aes(x=ws.HH)) + 
#       geom_point(aes(y=power.mean, color="actual power")) + 
#       geom_point(aes(y=cb.pred, color="predictions")) +
#       ggtitle("Cubist Predictions") +
#       theme(legend.title=element_blank())
 