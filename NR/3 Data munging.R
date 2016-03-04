
## Cleaning Data
str(train_store)
str(test_store)

f_train_store = train_store
f_test_store = test_store
f_store = store

# engineering in f_train_store

f_train_store$CompetitionDistance[is.na(f_train_store$CompetitionDistance)] = 5382
f_train_store$CompetitionOpenSinceMonth[is.na(f_train_store$CompetitionOpenSinceMonth)] = 4
f_train_store$CompetitionOpenSinceYear[is.na(f_train_store$CompetitionOpenSinceYear)] = 2009

f_train_store$CompetitionOpenSince <- as.yearmon(paste(f_train_store$CompetitionOpenSinceYear, 
                                               f_train_store$CompetitionOpenSinceMonth, sep = "-"))


#replicating above code in f_store

f_store$CompetitionDistance[is.na(f_store$CompetitionDistance)] = 5382
f_store$CompetitionOpenSinceMonth[is.na(f_store$CompetitionOpenSinceMonth)] = 4
f_store$CompetitionOpenSinceYear[is.na(f_store$CompetitionOpenSinceYear)] = 2009

f_store$CompetitionOpenSince <- as.yearmon(paste(f_store$CompetitionOpenSinceYear, 
                                                 f_store$CompetitionOpenSinceMonth, sep = "-"))

f_train_store$CompetitionOpenSinceYear = as.yearmon(paste(f_train_store$CompetitionOpenSince)) # converting iin year format


#replicating above code in f_test_store

f_test_store$CompetitionDistance[is.na(f_test_store$CompetitionDistance)] = 5382
f_test_store$CompetitionOpenSinceMonth[is.na(f_test_store$CompetitionOpenSinceMonth)] = 4
f_test_store$CompetitionOpenSinceYear[is.na(f_test_store$CompetitionOpenSinceYear)] = 2009

f_test_store$CompetitionOpenSince <- as.yearmon(paste(f_test_store$CompetitionOpenSinceYear, 
                                                 f_test_store$CompetitionOpenSinceMonth, sep = "-"))

f_test_store$CompetitionOpenSinceYear = as.yearmon(paste(f_test_store$CompetitionOpenSince)) # converting iin year format


# Exceptinal store if open on stateholiday
f_train_store$OpenonStateHoliday[f_train_store$StateHoliday != 0 & f_train_store$Open ==1 ] = 1
f_train_store$OpenonStateHoliday[is.na(f_train_store$OpenonStateHoliday)] = 0

#replicating exceptional store in test
f_test_store$OpenonStateHoliday[f_test_store$StateHoliday != 0 & f_test_store$Open ==1 ] = 1
f_test_store$OpenonStateHoliday[is.na(f_test_store$OpenonStateHoliday)] = 0


#Added How Many days since promo started column: promo2Days
#convert promo2Since to date format
f_train_store$Promo2Since = as.Date(f_train_store$Promo2Since)
f_test_store$Promo2Since = as.Date(f_test_store$Promo2Since)

# adding 1500 days in case no promo2
f_train_store$Promo2Since[is.na(f_train_store$Promo2Since)] = f_train_store$Date[is.na(f_train_store$Promo2Since)] +1500
sum(is.na(f_train_store$Promo2Since))
#doing it in test
f_test_store$Promo2Since[is.na(f_test_store$Promo2Since)] = f_test_store$Date[is.na(f_test_store$Promo2Since)] +1500
sum(is.na(f_test_store$Promo2Since))


#Added promo2Days column = diff of days bet current store date - promo started date 
# Negative value means how many days after that day the promo will start
f_train_store$promo2Days = f_train_store$Date - f_train_store$Promo2Since

f_train_store$Promo2SinceYear = format(f_train_store$Promo2Since, "%Y") ## extracting year from date
f_train_store$Promo2SinceYear = as.yearmon(paste(f_train_store$Promo2Since)) # converting iin year format


f_train_store$Promo2SinceWeek = format(f_train_store$Promo2Since, "%W") ## extracting week from date
f_train_store$Promo2SinceWeek = as.factor(f_train_store$Promo2SinceWeek) # converting weeks in factors

str(f_train_store)


#replicating in test
f_test_store$promo2Days = f_test_store$Date - f_test_store$Promo2Since

f_test_store$Promo2SinceYear = format(f_test_store$Promo2Since, "%Y") ## extracting year from date
f_test_store$Promo2SinceYear = as.yearmon(paste(f_test_store$Promo2Since)) # converting iin year format

f_test_store$Promo2SinceWeek = format(f_test_store$Promo2Since, "%W") ## extracting week from date
f_test_store$Promo2SinceWeek = as.factor(f_test_store$Promo2SinceWeek) # converting weeks in factors


#Promo Interval - Each round started month; so we can add column of promo2 round Number x started and 
#watch effect / drop in sales after how many days


## as given all schools are closed on public holidays and weekends (weekends == Sundays)
## trying to identify cases where schools are off and has no effect on sales
# Adding column if schoolOff 1 means off and 0 means running (no off)
f_train_store$schoolOff = 0
f_train_store$schoolOff[f_train_store$StateHoliday == "a" | f_train_store$DayOfWeek == 7 | f_train_store$SchoolHoliday == 1] = 1

#replicating in test
f_test_store$schoolOff = 0
f_test_store$schoolOff[f_test_store$StateHoliday == "a" | f_test_store$DayOfWeek == 7 | f_test_store$SchoolHoliday == 1] = 1


#Found 1,34,583 cases where store sales not affected even though schools are off (121350 (weekends off) + 13505(public holiday off) - 272 (both))
nrow(f_train_store[SchoolHoliday == 0 & schoolOff==1])
nrow(f_test_store[SchoolHoliday == 0 & schoolOff==1]) # 5942


f_train_store[f_train_store$SchoolHoliday == 1]
#150963

f_test_store[f_test_store$SchoolHoliday == 1]  #12101

nrow(f_train_store[SchoolHoliday == 1 & StateHoliday != 0])
# 2438 weekend
# 4663 public holiday A
# 13889 state holiday

#= values[match()]


f_train_store$schoolOff  <- sapply(f_train_store,isSchoolOff)
f_test_store$schoolOff  <- sapply(f_test_store,isSchoolOff)

f_train_store$schoolOff = as.factor(f_train_store$schoolOff ) # converting in factor
f_test_store$schoolOff  = as.factor(f_test_store$schoolOff ) # converting in factor

f_train_store$SchoolHoliday = as.factor(f_train_store$SchoolHoliday) # converting in factor
f_test_store$SchoolHoliday = as.factor(f_test_store$SchoolHoliday) # converting in factor

str(f_train_store)
head(f_train_store)

str(f_test_store)

any(is.na(train_store$schoolOff))

################################################################################################3

#exploration / graphs after engineering 

####### Exploration

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




#so now we have 0 missing values for compdistance and opensince year/month
sum(is.na(f_train_store$CompetitionDistance))
sum(is.na(f_train_store$CompetitionOpenSinceYear))

#replicating above code in f_store
sum(is.na(f_store$CompetitionDistance))
sum(is.na(f_store$CompetitionOpenSinceYear))


sum(is.na(f_store$CompetitionDistance))
sum(is.na(f_store$CompetitionOpenSinceYear))


# In these 861 cases the 143 unique stores were open even though it was stateholiday; not error but exceptional stores
# We will add column named exceptional store if the store is open in case of stateholiday to train and test data 
openstores_on_stateHolidays = f_train_store[Open == 1 & StateHoliday != 0]
nrow(openstores_on_stateHolidays)

unique(openstores_on_stateHolidays$Store)



# 861 cases stores open on state holiday
nrow(f_train_store[OpenonStateHoliday ==1])

# For 166,190 cases the store is not continuing with promo2 eventhough they ran promo1
f_train_store[Promo == 1 & Promo2==0]

str(f_train_store)
sum(is.na(f_train_store$Promo2Since))

min(f_train_store$Date)
max(f_train_store$Date)


sum(is.na(f_train_store$Promo2Since))

####### Graphs

# sales vs openonstateholiday
ggplot(f_train_store, aes(x=factor(Promo==1,Promo2==1), y=Sales)) + 
  stat_summary(fun.y="mean", geom="bar")


# sales vs openonstateholiday
ggplot(f_train_store, aes(x=factor(OpenonStateHoliday), y=Sales)) + 
  stat_summary(fun.y="mean", geom="bar")


# sales vs schooloff
ggplot(f_train_store, aes(x=factor(schoolOff), y=Sales)) + 
  stat_summary(fun.y="mean", geom="bar")

##111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111

library(dplyr)
mdl1_gm_train = f_train_store
mdl1_gm_test = f_test_store
mdl1_gm_train = mdl1_gm_train[mdl1_gm_train$Sales>0,]

#defining vector for group by vars
mdl1_grpbyvars=c('Store','DayOfWeek','Promo')

#applying sales by group by vars on train
mdl1 = mdl1_gm_train %>% group_by_(.dots=mdl1_grpbyvars) %>% summarise(mdl1_grpbyvarsales=exp(mean(log(Sales)))) %>% ungroup()

#applying learning of model in test
mdl1_gm_predictions = mdl1_gm_test %>% left_join(mdl1,by=mdl1_grpbyvars) %>% select(Id,mdl1_grpbyvarsales) %>% rename(Sales=mdl1_grpbyvarsales)

#making null values to zero
mdl1_gm_predictions$Sales[is.na(mdl1_predictions$Sales)]=0

#writing model1 predictions in csv
write.csv(mdl1_gm_predictions, "FEmdl1_predictions.csv",row.names=F)

####11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111


