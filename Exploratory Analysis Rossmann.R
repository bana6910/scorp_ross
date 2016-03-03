#<<<<<<< HEAD
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

train$Date  = as.Date(train$Date,"%m/%d/%Y")

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


# sales vs Promo2
ggplot(train_store,
       aes(x = factor(Promo), y = Sales)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "blue", outlier.colour = NA, fill = NA) +
  ggtitle("sales vs Promo")


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
write.csv(summary(train_store), "train_store_summary.csv",row.names=F)

#converting categorical data in factors
train_store$Store= as.factor(train_store$Store)
train_store$DayOfWeek= as.factor(train_store$DayOfWeek)
train_store$Open = as.factor(train_store$Open)
train_store$Promo = as.factor(train_store$Promo)
train_store$StateHoliday = as.factor(train_store$StateHoliday)
train_store$SchoolHoliday = as.factor(train_store$SchoolHoliday)
train_store$StoreType = as.factor(train_store$StoreType)
train_store$Assortment = as.factor(train_store$Assortment)
train_store$Promo2 = as.factor(train_store$Promo2)

write.csv(summary(train_store), "train_store_summary1.csv",row.names=F)

str(test)

#merging test and store to apply model
test_store <- merge(test, store, by = "Store")

test_store$Store= as.factor(test_store$Store)
test_store$DayOfWeek= as.factor(test_store$DayOfWeek)
test_store$Open = as.factor(test_store$Open)
test_store$Promo = as.factor(test_store$Promo)
test_store$StateHoliday = as.factor(test_store$StateHoliday)
test_store$SchoolHoliday = as.factor(test_store$SchoolHoliday)

test_store$StoreType = as.factor(test_store$StoreType)
test_store$Assortment = as.factor(test_store$Assortment)
test_store$Promo2 = as.factor(test_store$Promo2)

str(test_store)

##111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
library(dplyr)
mdl1_gm_train = train_store
mdl1_gm_test = test_store
mdl1_gm_train = mdl1_gm_train[mdl1_gm_train$Sales>0,]

#defining vector for group by vars
mdl1_grpbyvars=c('Store','DayOfWeek','Promo')

#applying sales by group by vars on train
mdl1 = mdl1_gm_train %>% group_by_(.dots=mdl1_grpbyvars) %>% summarise(mdl1_grpbyvarsales=exp(mean(log(Sales)))) %>% ungroup()

#applying learning of model in test
mdl1_predictions = mdl1_gm_test %>% left_join(mdl1,by=mdl1_grpbyvars) %>% select(Id,mdl1_grpbyvarsales) %>% rename(Sales=mdl1_grpbyvarsales)

#making null values to zero
mdl1_predictions$Sales[is.na(mdl1_predictions$Sales)]=0

#writing model1 predictions in csv
write.csv(mdl1_gm_predictions, "mdl1_predictions.csv",row.names=F)

####11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111

## Cleaning Data
str(train_store)

f_train_store = train_store
f_store = store

#In stores data we have Three stores with Store numbers 81, 862 and 957 has competition distance == NA
sum(is.na(store$CompetitionDistance))
store[is.na(CompetitionDistance)]

#in stores data, But 317 Stores have NA for competition starting information i.e. month, yr, week
sum(is.na(store$CompetitionOpenSinceYear))

# Thats why in train data we have CompetitionDistance NA's are only 2516;
#although CompetitionOpenSince NA's are 276100; so there are lot of missing values.
sum(is.na(f_train_store$CompetitionDistance))
sum(is.na(f_train_store$CompetitionOpenSinceYear))


#imputing Avg compDistance i.e. 5382 where it's NA and avg compopensince i.e. Apr 2009 where it's NA
nrow(f_train_store[is.na(f_train_store$CompetitionDistance)])

f_train_store$CompetitionDistance[is.na(f_train_store$CompetitionDistance)] = 5382
f_train_store$CompetitionOpenSinceMonth[is.na(f_train_store$CompetitionOpenSinceMonth)] = 4
f_train_store$CompetitionOpenSinceYear[is.na(f_train_store$CompetitionOpenSinceYear)] = 2009

f_train_store$CompetitionOpenSince <- as.yearmon(paste(f_train_store$CompetitionOpenSinceYear, 
                                               f_train_store$CompetitionOpenSinceMonth, sep = "-"))

#so now we have 0 missing values for compdistance and opensince year/month
sum(is.na(f_train_store$CompetitionDistance))
sum(is.na(f_train_store$CompetitionOpenSinceYear))

#replicating above code in f_store
sum(is.na(f_store$CompetitionDistance))
sum(is.na(f_store$CompetitionOpenSinceYear))

f_store$CompetitionDistance[is.na(f_store$CompetitionDistance)] = 5382
f_store$CompetitionOpenSinceMonth[is.na(f_store$CompetitionOpenSinceMonth)] = 4
f_store$CompetitionOpenSinceYear[is.na(f_store$CompetitionOpenSinceYear)] = 2009

f_store$CompetitionOpenSince <- as.yearmon(paste(f_store$CompetitionOpenSinceYear, 
                                                 f_store$CompetitionOpenSinceMonth, sep = "-"))
sum(is.na(f_store$CompetitionDistance))
sum(is.na(f_store$CompetitionOpenSinceYear))


# In these 861 cases the 143 unique stores were open even though it was stateholiday; not error but exceptional stores
# We will add column named exceptional store if the store is open in case of stateholiday to train and test data 
openstores_on_stateHolidays = f_train_store[Open == 1 & StateHoliday != 0]
nrow(openstores_on_stateHolidays)

unique(openstores_on_stateHolidays$Store)

# Exceptinal store if open on stateholiday
f_train_store$OpenonStateHoliday[f_train_store$StateHoliday != 0 & f_train_store$Open ==1 ] = 1
f_train_store$OpenonStateHoliday[is.na(f_train_store$OpenonStateHoliday)] = 0

# 861 cases stores open on state holiday
nrow(f_train_store[OpenonStateHoliday ==1])

# sales vs openonstateholiday
ggplot(f_train_store, aes(x=factor(OpenonStateHoliday), y=Sales)) + 
  stat_summary(fun.y="mean", geom="bar")
                                 
# For 166,190 cases the store is not continuing with promo2 eventhough they ran promo1
f_train_store[Promo == 1 & Promo2==0]

# sales vs openonstateholiday
ggplot(f_train_store, aes(x=factor(Promo==1,Promo2==1), y=Sales)) + 
  stat_summary(fun.y="mean", geom="bar")


#Added How Many days since promo started column: promo2Days
#convert promo2Since to date format
f_train_store$Promo2Since = as.Date(f_train_store$Promo2Since)


str(f_train_store)
sum(is.na(f_train_store$Promo2Since))

min(f_train_store$Date)
max(f_train_store$Date)


sum(is.na(f_train_store$Promo2Since))
f_train_store$Promo2Since[is.na(f_train_store$Promo2Since)] = f_train_store$Date[is.na(f_train_store$Promo2Since)] +1500
sum(is.na(f_train_store$Promo2Since))

#Added promo2Days column = diff of days bet current store date - promo started date 
# Negative value means how many days after that day the promo will start
f_train_store$promo2Days = f_train_store$Date - f_train_store$Promo2Since

remove(abc)
abc = f_train_store

abc$Promo2SinceYear =  format(abc$Promo2Since, "%Y")
abc$Promo2SinceWeek =  format(abc$Promo2Since, "%W") 



#Promo Interval - Each round started month; so we can add column of promo2 round Number x started and 
#watch effect / drop in sales after how many days


## as given all schools are closed on public holidays and weekends (weekends == Sundays)
## trying to identify cases where schools are off and has no effect on sales
# Adding column if schoolOff 1 means off and 0 means running (no off)
f_train_store$schoolOff = 0
f_train_store$schoolOff[f_train_store$StateHoliday == "a" | f_train_store$DayOfWeek == 7 | f_train_store$SchoolHoliday == 1] = 1

#Found 1,34,583 cases where store sales not affected even though schools are off (121350 (weekends off) + 13505(public holiday off) - 272 (both))
nrow(f_train_store[SchoolHoliday == 0 & schoolOff==1])


# sales vs schooloff
ggplot(f_train_store, aes(x=factor(schoolOff), y=Sales)) + 
  stat_summary(fun.y="mean", geom="bar")


f_train_store[f_train_store$SchoolHoliday == 1]
#150963

nrow(f_train_store[SchoolHoliday == 1 & StateHoliday != 0])
# 2438 weekend
# 4663 public holiday A
# 13889 state holiday

#= values[match()]


f_train_store$schoolOff  <- sapply(f_train_store,isSchoolOff)

str(train_store)
head(train_store)

train_store[is.na(train_store$schoolOff)]
train_store[is.na(schoolOff)]
any(is.na(train_store$schoolOff))

train_store[, lapply(.SD, function(x) length(unique(x)))]
head(train_store)


