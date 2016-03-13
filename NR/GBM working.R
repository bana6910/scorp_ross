library(caret)
library(Metrics) # for rmse


# Read in the data
#write.csv(f_train_store, "f_train_store.csv",row.names=F)

#data <- read.csv("C:/Users/ratho/Desktop/Practicum/scorp_ross/f_train_store.csv")

data = f_train_store

data$Sales = log(data$Sales)


data$Sales[data$Sales==-Inf] = 0


# Look at a summary of the data
summary(data)


# set up the target and feature names, only consider one-story properties, and remove some error values
target = "Sales"
features <- c("Store" , "DayOfWeek", "Open" ,  "StateHoliday" , "SchoolHoliday" , "CompetitionDistance" , "Promo2", "promo2Days" )

cols <- append(features, target)

data2 <- data
data2 <- data2[,cols]
str(data2)

dummyEncoding <- dummyVars(Sales ~ ., data = data2)

# Create training (70% sample of the data) and test sets (remaining 30%)
#set.seed(800)
n <- dim(data2)[1]

trainIndex <- sample(1:n, n * 0.70)

training <- data2[ trainIndex, ]
test  <- data2[-trainIndex,]

warning()

# Now encode, center, and scale the training data
trainProcessed <- predict(dummyEncoding, newdata = training)
centerAndScale <- preProcess(trainProcessed, method=c("center","scale"))
trainProcessed <- predict(centerAndScale, newdata = trainProcessed)
trainProcessed <- cbind(trainProcessed, price = training$Sales)
trainProcessed <- as.data.frame(trainProcessed)

testX <- predict(dummyEncoding, newdata = test)
textX <- predict(centerAndScale, newdata = testX)
testY <- test[,c(target)]

# Fit k-nearest neigbor
#set.seed(825)


fitControl <- trainControl(
  method = "repeatedcv"
 
  )

gbmGrid <-  expand.grid(interaction.depth = c(3,7,11),
                        n.trees = (2:5)*75,
                        shrinkage = 0.1,
                        n.minobsinnode = 30
                        )

modelGbm<- train(price ~ ., 
                  data = trainProcessed,
                  method = "gbm",
                  metric = "RMSE",
                  trControl = fitControl,
                  tuneGrid = gbmGrid
                )



# Look at model results
summary(modelGbm)
plot(modelGbm)

# Predict on the test data (this uses the best model)
predY <- predict(modelGbm, newdata = testX)
rmse(predY, testY)