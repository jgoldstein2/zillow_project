###############################################################
# Common scripts to check missing data
###############################################################

sort(sapply(cleanTraining, function(x) { sum(is.na(x)) }), decreasing=TRUE)
sapply(cleanTraining, typeof)
sapply(cleanTraining, class)
nrow(cleanTraining)
nrow(cleanTraining2)
names(cleanTraining)
names(cleanTraining)
dim(cleanTraining2)
View(cleanTraining)
table(cleanTraining$hashottuborspa)

# save(cleanTraining, file='cleanTraining_master.Rda')
# 
# names(cleanTraining)
###############################################################
# Reading files 
###############################################################

train_df <- fread('train_2016_v2.csv')
properties_df <- fread('properties_2016.csv')
# sample_df <- fread('sample_submission.csv')

###############################################################
# joining tain_df with properties_df
###############################################################

training_df <- left_join(train_df, properties_df, by='parcelid')

###############################################################
# Dropping columns
###############################################################

name_list <- names(training_df)

cols_drop <- c(name_list[55], name_list[5:6], name_list[9], 
               name_list[11], name_list[45], name_list[13],
               name_list[15:20], name_list[52], name_list[22],
               name_list[51], name_list[31:35],
               name_list[37:39], name_list[60], name_list[41],
               name_list[43:44], name_list[46], name_list[48:49],
               name_list[59])

cleanTraining <- training_df[ , !(names(training_df) %in% cols_drop)]

# lat_long <- data.frame(c(training_df['parcelid'], training_df['latitude'], training_df['longitude']))
# names(lat_long)
# names(cleanTraining2)
# 
# library(dplyr)
# cleanTraining2 <- cleanTraining
# cleanTraining2 <- left_join(cleanTraining, lat_long, by ='parcelid')
# x <- unique(cleanTraining2)
# nrow(cleanTraining2)
# nrow(cleanTraining)
# dim(x)
# sum(is.na(x))
###############################################################
# Imputation of Binary Variables 
###############################################################

cleanTraining$decktypeid[is.na(cleanTraining$decktypeid)] = 0
cleanTraining$fireplacecnt[is.na(cleanTraining$fireplacecnt)] = 0
cleanTraining$poolcnt[is.na(cleanTraining$poolcnt)] = 0
cleanTraining$unitcnt[is.na(cleanTraining$unitcnt)] = 1

cleanTraining$taxdelinquencyflag = ifelse(cleanTraining$taxdelinquencyflag == '', 0, 1)

cleanTraining$hashottuborspa = ifelse(cleanTraining$hashottuborspa == '', 0, 1)

cleanTraining$airconditioningtypeid = ifelse(is.na(cleanTraining$airconditioningtypeid),
                                      ifelse(cleanTraining$heatingorsystemtypeid == 2, 1, cleanTraining$airconditioningtypeid),
                                      ifelse(cleanTraining$airconditioningtypeid == 5, 0, 1))

cleanTraining$heatingorsystemtypeid = ifelse(is.na(cleanTraining$heatingorsystemtypeid), 0, 
                                      ifelse(cleanTraining$heatingorsystemtypeid == 13, 0, 1))
###############################################################
# Imputation by Mean
###############################################################

library(Hmisc)
imputed.taxvaluedollarcnt = impute(cleanTraining$taxvaluedollarcnt, mean)
cleanTraining$taxvaluedollarcnt = imputed.taxvaluedollarcnt

imputed.structuretaxvaluedollarcnt = impute(cleanTraining$structuretaxvaluedollarcnt, mean)
cleanTraining$structuretaxvaluedollarcnt = imputed.structuretaxvaluedollarcnt

imputed.landtaxvaluedollarcnt = impute(cleanTraining$landtaxvaluedollarcnt, mean)
cleanTraining$landtaxvaluedollarcnt = imputed.landtaxvaluedollarcnt

imputed.taxamount = impute(cleanTraining$taxamount, mean)
cleanTraining$taxamount = imputed.taxamount

cleanTraining$bathroomcnt <- ifelse(cleanTraining$bathroomcnt == 0, sample(c(2,2.5)), cleanTraining$bathroomcnt)

###############################################################
# Mutating Age of Home Variable and Imputing
###############################################################

library(dplyr)
cleanTraining <- cleanTraining %>% mutate(age_of_home = 2017 - cleanTraining$yearbuilt)

imputed.age_of_home = impute(cleanTraining$age_of_home, mean)
cleanTraining$age_of_home = imputed.age_of_home
cleanTraining$yearbuilt <- NULL

###############################################################
# Remove blank zip code rows 
###############################################################

cleanTraining <- cleanTraining %>% filter(!(is.na(cleanTraining$regionidzip)))

###############################################################
# Changing variables to factors 
###############################################################

cols_reduced <- names(cleanTraining)
cols_factors <- c('airconditioningtypeid', 'buildingqualitytypeid', 'decktypeid',
                  'fireplacecnt', 'hashottuborspa', 'heatingorsystemtypeid', 
                  'poolcnt', 'propertylandusetypeid', 'regionidcounty', 
                  'regionidzip', 'taxdelinquencyflag')
cleanTraining[cols_factors] <- lapply(cleanTraining[cols_factors], factor)
cleanTraining$garagecarcnt = as.numeric(cleanTraining$garagecarcnt)
cleanTraining$unitcnt = as.numeric(cleanTraining$unitcnt)

###############################################################
# Imputation by MICE package
###############################################################

### Maybe we should impute with other packages to see differences?
# install.packages("Amelia")
# library(Amelia)
# ?amelia
library(mice)

### Code if you need to exclude rows from imputation, run 
### before the imputation:
# exclude <- c("latitude", "longitude")
# include <- setdiff(names(cleanTraining), exclude)
# train_raw <- cleanTraining

# Imputation code, running m=5 did not work, maybe will try again later
imp.train_raw <- mice(cleanTraining, m=1, method='cart', printFlag=FALSE)

# Merging imputed values into data set
impute_complete <- complete(imp.train_raw)
cleanTraining <- impute_complete

### Code if you need to add columns back in that you excluded
# cleanTraining <- cbind(impute_complete, cleanTraining[exclude])

###############################################################
# Exploring the Imputations
###############################################################

# Shows distribution of imputed values within the existing data set
library(lattice)
xyplot(imp.train_raw, calculatedfinishedsquarefeet ~ logerror)
xyplot(imp.train_raw, buildingqualitytypeid ~ logerror)

# Checking the values it assigned to missing variables
# Some viz would be good to explore the imputed values/data
table(imp.train_raw$imp$calculatedfinishedsquarefeet)
table(imp.train_raw$imp$buildingqualitytypeid)
table(imp.train_raw$imp$garagecarcnt)
table(imp.train_raw$imp$garagetotalsqft)
table(imp.train_raw$imp$lotsizesquarefeet)
table(imp.train_raw$imp$airconditioningtypeid)

names(imp.train_raw$imp)

###############################################################
# Types of Imputations Performed
###############################################################
### No missing values
# parcelid, logerror, transactiondate, bedroomcnt, regionidcounty, propertylandusetypeid

### Assigned missing values as 0s
# decktypeid, fireplacecnt, hashottuborspa, poolcnt,
# taxdelinquencyflag, taxvaluedollarcnt, 

### Assigned missing values as 1
# unitcnt: assumed all missing values were 1

### Imputed by mean, not very many missing values relative to the data set
# taxvaluedollarcnt, structuretaxvaluedollarcnt, landtaxvaluedollarcnt, taxamount

### Created new varibale subtracting year built from present year, imputed by mean
# age_of_home

### Removed observations without a zip code, only 35 observations
# regionidzip

### Performed ANOVA to test for significance, 
# airconditioningtypeid: siginificant, converted to binary
# heatingorsystemtypeid: 

### Performed "cart" method impuatation from MICE pacakge, 
### "cart" method stands for classification and regression trees
### performs a regression tree analysis for the imputed values. 
### Imputed values inferred from other variables.
# garagecarcnt, garagetotalsqft, lotsizesquarefeet, calculatedfinishedsquarefeet, 
# buildingqualitytypeid, airconditioningtypeid

















