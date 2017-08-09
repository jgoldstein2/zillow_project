###############################################################
# Reading files 
###############################################################

train_df <- fread('train_2016_v2.csv')
sample_df <- fread('sample_submission.csv')
properties_df <- fread('properties_2016.csv')

###############################################################
# joining tain_df with properties_df
###############################################################

training_df <- left_join(train_df, properties_df, by='parcelid')

###############################################################
# Common scripts to check missing data
###############################################################

sort(sapply(cleanTraining, function(x) { sum(is.na(x)) }), decreasing=TRUE)
nrow(cleanTraining)
nrow(cleanTraining2)
names(cleanTraining)

###############################################################
# Dropping columns
###############################################################

name_list <- names(training_df)

cols_drop <- c(name_list[55], name_list[5:6], name_list[9], 
               name_list[11], name_list[45], name_list[13],
               name_list[15:20], name_list[52], name_list[22],
               name_list[27:28], name_list[51], name_list[31:35],
               name_list[37:39], name_list[60], name_list[41],
               name_list[43:44], name_list[46], name_list[48:49],
               name_list[59])

cleanTraining <- training_df[ , !(names(training_df) %in% cols_drop)]
# save(cleanTraining, file='cleanTraining_AGT.Rda')

###############################################################
# Imputation of Binary Variables 
###############################################################

cleanTraining$decktypeid[is.na(cleanTraining$decktypeid)] = 0
cleanTraining$fireplacecnt[is.na(cleanTraining$fireplacecnt)] = 0
cleanTraining$hashottuborspa[is.na(cleanTraining$hashottuborspa)] = 0
cleanTraining$poolcnt[is.na(cleanTraining$poolcnt)] = 0
cleanTraining$unitcnt[is.na(cleanTraining$unitcnt)] = 1
cleanTraining$taxdelinquencyflag[is.na(cleanTraining$taxdelinquencyflag)] = 0

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

# sapply(cleanTraining, class)

cols_reduced <- names(cleanTraining)
cols_factors <- c(cols_reduced[6:7], cols_reduced[9], cols_reduced[10],
                  cols_reduced[12], cols_reduced[14:18], cols_reduced[23],
                  cols_reduced[25:26])
cleanTraining[cols_factors] <- lapply(cleanTraining[cols_factors], factor)

###############################################################
# Imputation by MICE package, visit web address below for explanation
# https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r
###############################################################

install.packages("Amelia")
library(Amelia)
library(mice)
?amelia

exclude <- c('airconditioningtypeid', 'heatingorsystemtypeid')
include <- setdiff(names(cleanTraining), exclude)

train_raw <- cleanTraining[include]
imp.train_raw <- mice(train_raw, m=1, method='cart', printFlag=FALSE)

# Shows distribution of imputed values within the existing data set
library(lattice)
xyplot(imp.train_raw, calculatedfinishedsquarefeet ~ logerror)

# Checking the values it assigned to missing variables
# Some viz would be good to explore the imputed values/data
table(imp.train_raw$imp$calculatedfinishedsquarefeet)
table(imp.train_raw$imp$buildingqualitytypeid)
table(imp.train_raw$imp$buildingqualitytypeid)
table(imp.train_raw$imp$garagecarcnt)
table(imp.train_raw$imp$garagecarcnt)

# Merging imputed values into data set
impute_complete <- complete(imp.train_raw)
cleanTraining <- cbind(impute_complete, cleanTraining[exclude])




