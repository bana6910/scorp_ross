<<<<<<< HEAD
library(data.table)
library(zoo)

library(forecast)
library(ggplot2)

test <- fread("C:\\Users\\ratho\\Desktop\\Practicum\\rossman_data\\test.csv")
train <- fread("C:/Users/ratho/Desktop/Practicum/rossman_data/train.csv")
store <- fread("C:/Users/ratho/Desktop/Practicum/rossman_data/stores.csv")


str(train)
str(test)
str(store)

head(train); tail(train)
head(test); tail(test)


train[, Date := as.Date(Date,"%m/%d/%Y")]
test[, Date := as.Date(Date)]


train <- train[order(Date)]
test <- test[order(Date)]
summary(train)
summary(test)

# Unique values per column
train[, lapply(.SD, function(x) length(unique(x)))]
test[, lapply(.SD, function(x) length(unique(x)))]

# All test stores are also in the train data
sum(unique(test$Store) %in% unique(train$Store)) 

# All the train stores are in the test data
sum(!(unique(train$Store) %in% unique(test$Store))) 

table(train$Open) / nrow(train) # Percent Open Train
table(test$Open) / nrow(test) # Percent Open Test 

table(train$Promo) / nrow(train) # Percent of the time promo in train
table(test$Promo) / nrow(test) # Percent of the time promo in test

table(train$StateHoliday) / nrow(train) # Percent of the time holiday in train
table(test$StateHoliday) / nrow(test) # Percent of time holiday in test; No HOLIDAYS ARE THERE

table(train$SchoolHoliday) / nrow(train) # Percent of the time school holiday in train
table(test$SchoolHoliday) / nrow(test) # Percent of the time school holiday in test

plot(train$Date, type = "l")
plot(test$Date, type = "l")

all(table(test$Date) == 1000) 

hist(train$Sales, 100)

hist(aggregate(train[Sales != 0]$Sales, 
               by = list(train[Sales != 0]$Store), mean)$x, 100, 
     main = "Mean sales per store when store was not closed")

hist(train$Customers, 100)

hist(aggregate(train[Sales != 0]$Customers, 
               by = list(train[Sales != 0]$Store), mean)$x, 100,
     main = "Mean customers per store when store was not closed")


ggplot(train[Sales != 0], aes(x = factor(SchoolHoliday), y = Sales)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "blue", outlier.colour = NA , fill = NA)

ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = log(Customers), y = log(Sales))) + 
  geom_point(alpha = 0.2) + geom_smooth()

ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Sales)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Customers)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

with(train[train$Sales != 0 & train$Promo == 0], mean(Sales / Customers))

with(train[train$Sales != 0 & train$Promo == 1], mean(Sales / Customers))


table(ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
      ifelse(train$Promo, "Promo", "No promo"))


table(ifelse(train$Open == 1, "Opened", "Closed"),
      ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))


train[Open == 1 & Sales == 0]


train[Customers >0 & Sales == 0]


zerosPerStore <- sort(tapply(train$Sales, list(train$Store), function(x) sum(x == 0)))
hist(zerosPerStore,100)

# Stores with the most zeros in their sales:
tail(zerosPerStore, 10)


#321 
#911 
#507  
#55 
# Some stores were closed for some time, some of those were closed multiple times
plot(train[Store == 321, Sales], ylab = "Sales", xlab = "Days", main = "Store 321")
plot(train[Store == 911, Sales], ylab = "Sales", xlab = "Days", main = "Store 911")
plot(train[Store == 507, Sales], ylab = "Sales", xlab = "Days", main = "Store 507")
plot(train[Store == 55, Sales], ylab = "Sales", xlab = "Days", main = "Store 55")

sundaySales = subset(train[train$Open == 1 & train$DayOfWeek ==7])
sundaySales

sunsales <- sort(tapply(sundaySales$Sales, list(sundaySales$Store), function(x) sum(x)))

tail(sunsales, 10)

sundaySales <- sort(tapply(sundaySales$Sales, list(sundaySales$Store), function(x) sum(x == 0)))

ggplot(train[Store == 392], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
  geom_point(size = 3) + ggtitle("Sales of store 392 (True if sunday)")


ggplot(train[Sales != 0],
       aes(x = factor(DayOfWeek), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

summary(store)

train[Sales != 0 & DayOfWeek==7]

table(store$StoreType)
table(store$Assortment)


# There is a connection between store type and type of assortment
table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))

hist(store$CompetitionDistance, 100)

# Convert the CompetitionOpenSince... variables to one Date variable
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))
# One competitor opened 1900
hist(as.yearmon("2015-10") - store$CompetitionOpenSince, 100, 
     main = "Years since opening of nearest competition")


# Convert the Promo2Since... variables to one Date variable
# Assume that the promo starts on the first day of the week
store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                      store$Promo2SinceWeek, 1, sep = "-"),
                                format = "%Y-%U-%u")
hist(as.numeric(as.POSIXct("2015-10-01", format = "%Y-%m-%d") - store$Promo2Since), 
     100, main = "Days since start of promo2")


table(store$PromoInterval)


# Merge store and train 
train_store <- merge(train, store, by = "Store")

#sales by promo interval
ggplot(train_store[Sales != 0], aes(x = factor(PromoInterval), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA)

#sales by storetype
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)


#cust by storetype
ggplot(train_store[Customers != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
  geom_smooth(size = 2)

#sales by assortment
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
  geom_smooth(size = 2)

#customers by assortment
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + 
  geom_smooth(size = 2)

#sales by competition distance
salesByDist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
                         by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), mean)
colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + 
  geom_point() + geom_smooth()

# sales vs comp-true/false
ggplot(train_store[Sales != 0],
       aes(x = factor(!is.na(CompetitionOpenSinceYear)), y = Sales)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA) +
  ggtitle("Any competition?")


# Sales before and after competition opens
train_store$DateYearmon <- as.yearmon(train_store$Date)
train_store <- train_store[order(Date)]
timespan <- 100 # Days to collect before and after Opening of competition
beforeAndAfterComp <- function(s) {
  x <- train_store[Store == s]
  daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
  if (any(!daysWithComp)) {
    compOpening <- head(which(!daysWithComp), 1) - 1
    if (compOpening > timespan & compOpening < (nrow(x) - timespan)) {
      x <- x[(compOpening - timespan):(compOpening + timespan), ] 
      x$Day <- 1:nrow(x)
      return(x)
    }
  }
}
temp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), beforeAndAfterComp)
temp <- do.call(rbind, temp)
# 130 stores first had no competition but at least 100 days before the end
# of the data set
length(unique(temp$Store))


ggplot(temp[Sales != 0], aes(x = Day, y = Sales)) + 
  geom_smooth() + 
  ggtitle(paste("Competition opening around day", timespan))


temp <- train
temp$year <- format(temp$Date, "%Y")
temp$month <- format(temp$Date, "%m")
temp[, StoreMean := mean(Sales), by = Store]
temp <- temp[, .(MonthlySalesMean = mean(Sales / (StoreMean)) * 100), 
             by = .(year, month)]
temp <- as.data.frame(temp)
SalesTS <- ts(temp$MonthlySalesMean, start=2013, frequency=12)
col = rainbow(3)

#seasonality sales graph
seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, las=1)

str(train_store)
head(train_store)
