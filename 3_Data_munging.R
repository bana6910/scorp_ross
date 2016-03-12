#<<<<<<< HEAD
#Script to munge data in test_store and train_store
#Run 1_preparing_Data.R before this script

## Data cleaning and munging 
    str(train_store)
    #creating a data frame for features (designated by adding an "f" to the name)
    f_train_store = train_store
    f_store = store
    f_test_store = test_store
    
  # Dealing with NA's in store (and train_store) for CompetitionDistance, CompetitionOpenSinceYear, and CompetitionOpenSince
   
      #In stores data we have Three stores with Store numbers 81, 862 and 957 have competition distance == NA
      sum(is.na(store$CompetitionDistance))
      store[is.na(CompetitionDistance)]
      
      #in stores data, 317 Stores have NA for competition starting information i.e. month, yr, week
      sum(is.na(store$CompetitionOpenSinceYear))
      
      # As a result, in train_store, we have only CompetitionDistance NA's of 2516;
      #but in CompetitionOpenSince there are 276100 NA's--alot of missing values
      sum(is.na(f_train_store$CompetitionDistance))
      sum(is.na(f_train_store$CompetitionOpenSinceYear))
      
      #imputing Avg compDistance i.e. 5382 where it's NA and avg CompetitionOpenSince i.e. Apr 2009 where it's NA
      mean_CompetitionDistance = mean( f_train_store$CompetitionDistance[!is.na(f_train_store$CompetitionDistance)])
      f_train_store$CompetitionDistance[is.na(f_train_store$CompetitionDistance)] =  mean_CompetitionDistance 
  
      mean( f_train_store$CompetitionOpenSinceMonth[!is.na(f_train_store$CompetitionOpenSinceMonth)])
      f_train_store$CompetitionOpenSinceMonth[is.na(f_train_store$CompetitionOpenSinceMonth)] = 4
      
      mean( f_train_store$CompetitionOpenSinceYear[!is.na(f_train_store$CompetitionOpenSinceYear)])
      f_train_store$CompetitionOpenSinceYear[is.na(f_train_store$CompetitionOpenSinceYear)] = 2009
      
      f_train_store$CompetitionOpenSince <- as.yearmon(paste(f_train_store$CompetitionOpenSinceYear, 
                                                     f_train_store$CompetitionOpenSinceMonth, sep = "-"))
      
      #so now we have 0 missing values for compdistance and opensince year/month
      sum(is.na(f_train_store$CompetitionDistance))
      sum(is.na(f_train_store$CompetitionOpenSinceYear))
      
      #replicating above code in f_store
      sum(is.na(f_store$CompetitionDistance))
      sum(is.na(f_store$CompetitionOpenSinceYear))
      
      f_store$CompetitionDistance[is.na(f_store$CompetitionDistance)] = mean_CompetitionDistance
      f_store$CompetitionOpenSinceMonth[is.na(f_store$CompetitionOpenSinceMonth)] = 4
      f_store$CompetitionOpenSinceYear[is.na(f_store$CompetitionOpenSinceYear)] = 2009
      
      f_store$CompetitionOpenSince <- as.yearmon(paste(f_store$CompetitionOpenSinceYear, 
                                                       f_store$CompetitionOpenSinceMonth, sep = "-"))
      sum(is.na(f_store$CompetitionDistance))
      sum(is.na(f_store$CompetitionOpenSinceYear))
  
      #replicating above code in test_store (added by JC)
      
      f_test_store$CompetitionDistance[is.na(f_test_store$CompetitionDistance)] =  mean_CompetitionDistance 
      f_test_store$CompetitionOpenSinceMonth[is.na(f_test_store$CompetitionOpenSinceMonth)] = 4
      f_test_store$CompetitionOpenSinceYear[is.na(f_test_store$CompetitionOpenSinceYear)] = 2009
      
      f_test_store$CompetitionOpenSince <- as.yearmon(paste(f_test_store$CompetitionOpenSinceYear, 
                                                             f_test_store$CompetitionOpenSinceMonth, sep = "-"))
      sum(is.na(f_test_store$CompetitionDistance))
      sum(is.na(f_test_store$CompetitionOpenSinceYear))

  # Creating a feature for exceptional stores
    #In these 861 cases the 143 unique stores were open even though it was stateholiday; not error but exceptional stores
    # We will add column named exceptional store if the store is open in case of stateholiday to train and test data 
    openstores_on_stateHolidays = f_train_store[Open == 1 & StateHoliday != 0] #creates a dataframe with exceptional stores
    
    nrow(openstores_on_stateHolidays) #861
    length(unique(openstores_on_stateHolidays$Store)) #143
  
    # Add column to f_train_store with exceptional stores 
    f_train_store$OpenonStateHoliday[f_train_store$StateHoliday != 0 & f_train_store$Open ==1 ] = 1 #open
    f_train_store$OpenonStateHoliday[is.na(f_train_store$OpenonStateHoliday)] = 0 #closed
    f_train_store$OpenonStateHoliday = as.factor(f_train_store$OpenonStateHoliday) #Change to a factor; JC addition
    nrow(f_train_store[OpenonStateHoliday == "1"]) #Check to result to ensure total is 861 instances
    
    ggplot(f_train_store, aes(x=factor(OpenonStateHoliday), y=Sales)) + 
      stat_summary(fun.y="mean", geom="bar") #plot to see how signifcant a factor this is
  
    #repeat calculations for f_test_store
    openstores_on_stateHolidays2 = f_test_store[Open == 1 & StateHoliday != 0] #creates a dataframe with exceptional stores
    nrow(openstores_on_stateHolidays2) #0, therefore may not be useful
    length(unique(openstores_on_stateHolidays2$Store)) #0  
    
    f_test_store$OpenonStateHoliday[f_test_store$StateHoliday != 0 & f_test_store$Open ==1 ] = 1 #open
    f_test_store$OpenonStateHoliday[is.na(f_test_store$OpenonStateHoliday)] = 0 #closed
    f_test_store$OpenonStateHoliday = as.factor(f_test_store$OpenonStateHoliday) #Change to a factor; JC addition
    nrow(f_test_store[OpenonStateHoliday == "1"]) #Check to result to ensure total is 0 instances
 
  # # Creating a feature for promo 1 only
  #   # For 166,190 cases the store is not continuing with promo2 eventhough they ran promo1
  #   f_train_store[Promo == 1 & Promo2==0]
  #   
  #   # sales vs promo1 only
  #   ggplot(f_train_store, aes(x=factor(Promo==1,Promo2==0), y=Sales)) + 
  #     stat_summary(fun.y="mean", geom="bar")

  # Promo2Since: Dealing with NA's and adding a column for number of days Promo2 has been run (Promo2Days)
    #Replace NA's with the mean value
    f_train_store$Promo2Since = as.Date(f_train_store$Promo2Since) #convert promo2Since from Posi to date format
    sum(is.na(f_train_store$Promo2Since)) #433,719 NA's
   
    f_train_store$Promo2Since[is.na(f_train_store$Promo2Since)] = mean(f_train_store$Date) #JC changed from +1500 to the mean
    sum(is.na(f_train_store$Promo2Since))

    #Added promo2Days column = difference of days between the current date and promo start date 
    # Negative value means how many days after that day the promo will start
    f_train_store$promo2Days = f_train_store$Date - f_train_store$Promo2Since

    #repeat Promo2Since operations for f_test_store
    f_test_store$Promo2Since = as.Date(f_test_store$Promo2Since) #convert promo2Since from Posi to date format
    sum(is.na(f_test_store$Promo2Since)) #20,412 NA's
    
    f_test_store$Promo2Since[is.na(f_test_store$Promo2Since)] = mean(f_train_store$Date) #JC changed from +1500 to the mean
    sum(is.na(f_test_store$Promo2Since))
   
    f_test_store$promo2Days = f_test_store$Date - f_test_store$Promo2Since
    max( f_test_store$promo2Days)
    min( f_test_store$promo2Days)

 #Not sure of the value of this munging below, recommending we discuss before investing more time, JC
    # ## as given all schools are closed on public holidays and weekends (weekends == Sundays)
    # ## trying to identify cases where schools are off and has no effect on sales
    # # Adding column if schoolOff 1 means off and 0 means running (no off)
    # f_train_store$schoolOff = 0
    # f_train_store$schoolOff[f_train_store$StateHoliday == "a" | f_train_store$DayOfWeek == 7 | f_train_store$SchoolHoliday == 1] = 1
    # 
    # #Found 1,34,583 cases where store sales not affected even though schools are off (121350 (weekends off) + 13505(public holiday off) - 272 (both))
    # nrow(f_train_store[SchoolHoliday == 0 & schoolOff==1])
    # 
    # # sales vs schooloff
    # ggplot(f_train_store, aes(x=factor(schoolOff), y=Sales)) + 
    #   stat_summary(fun.y="mean", geom="bar")
    # 
    # f_train_store[f_train_store$SchoolHoliday == 1]
    # #150963
    # 
    # nrow(f_train_store[SchoolHoliday == 1 & StateHoliday != 0])
    # # 2438 weekend
    # # 4663 public holiday A
    # # 13889 state holiday
    # 
    # #= values[match()]
    # 
    # 
    # f_train_store$schoolOff  <- sapply(f_train_store,isSchoolOff)
    # 
    # str(train_store)
    # head(train_store)
    # 
    # train_store[is.na(train_store$schoolOff)]
    # train_store[is.na(schoolOff)]
    # any(is.na(train_store$schoolOff))
    # 
    # train_store[, lapply(.SD, function(x) length(unique(x)))]
    # head(train_store)

str(f_train_store)
str(f_test_store)

colSums(is.na(f_train_store)) #it's OK if Promo2Since Week and Promo2SinceYear have NA's since we won't use them
colSums(is.na(f_test_store)) 

