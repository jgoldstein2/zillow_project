library(DT)
library(data.table)
library(dplyr)
library(lubridate)
library(Hmisc)
library(mice)

###############################################################
# Reading files 
###############################################################
properties_df = fread("./properties_2016.csv")
train_df = fread("./train_2016_v2.csv")

###############################################################
# joining train_df with properties_df
###############################################################

training_df <- left_join(train_df, properties_df, by='parcelid')

###############################################################
# Dropping columns
###############################################################
name_list <- names(training_df)

cols_drop <- c("assessmentyear", "architecturalstyletypeid", "basementsqft", 
               "buildingclasstypeid", "calculatedbathnbr", "threequarterbathnbr", 
               "finishedfloor1squarefeet", "finishedsquarefeet12", "finishedsquarefeet13",        
               "finishedsquarefeet15", "finishedsquarefeet50", "finishedsquarefeet6", 
               "fips", "fireplaceflag", "fullbathcnt", "numberofstories", "poolsizesum",
               "pooltypeid10", "pooltypeid2", "pooltypeid7", "propertycountylandusecode", 
               "propertyzoningdesc", "rawcensustractandblock", "regionidcity", "censustractandblock",
               "regionidneighborhood", "roomcnt", "storytypeid", "typeconstructiontypeid", 
               "yardbuildingsqft17", "yardbuildingsqft26", "taxdelinquencyyear", "garagetotalsqft")

cleanTraining <- training_df[ , !(names(training_df) %in% cols_drop)]

###############################################################
# Mutating/Adding Features
###############################################################

cleanTraining = cleanTraining %>% mutate(latitude = latitude/1e6, longitude = longitude/1e6)
cleanTraining = cleanTraining %>% mutate(month = month(transactiondate))
cleanTraining <- cleanTraining %>% mutate(age_of_home = 2017 - cleanTraining$yearbuilt)

###############################################################
# Imputation of Binary Variables
###############################################################

cleanTraining$poolcnt[is.na(cleanTraining$poolcnt)] = 0

cleanTraining$unitcnt[is.na(cleanTraining$unitcnt)] = 1

cleanTraining$decktypeid = ifelse(is.na(cleanTraining$decktypeid), 0, 1) 

cleanTraining$fireplacecnt = ifelse(is.na(cleanTraining$fireplacecnt), 0, 1) 

cleanTraining$taxdelinquencyflag = ifelse(cleanTraining$taxdelinquencyflag == '', 0, 1)

cleanTraining$hashottuborspa = ifelse(cleanTraining$hashottuborspa == '', 0, 1)

cleanTraining$airconditioningtypeid = ifelse(is.na(cleanTraining$airconditioningtypeid),
                                             ifelse(cleanTraining$heatingorsystemtypeid == 2, 1, cleanTraining$airconditioningtypeid),
                                             ifelse(cleanTraining$airconditioningtypeid == 5, 0, 1))

cleanTraining$heatingorsystemtypeid = ifelse(is.na(cleanTraining$heatingorsystemtypeid), 0, 
                                             ifelse(cleanTraining$heatingorsystemtypeid == 13, 0, 1))

###############################################################
# Imputation by Mean/Mode
###############################################################

cleanTraining$taxvaluedollarcnt = as.numeric(impute(cleanTraining$taxvaluedollarcnt, mean))

cleanTraining$structuretaxvaluedollarcnt = as.numeric(impute(cleanTraining$structuretaxvaluedollarcnt, mean))

cleanTraining$landtaxvaluedollarcnt = as.numeric(impute(cleanTraining$landtaxvaluedollarcnt, mean))

cleanTraining$taxamount = as.numeric(impute(cleanTraining$taxamount, mean))

cleanTraining$lotsizesquarefeet <- as.numeric(impute(cleanTraining$lotsizesquarefeet, mean))

cleanTraining$calculatedfinishedsquarefeet <- as.numeric(impute(cleanTraining$lotsizesquarefeet, mean))

mode_ <- function(x) {
  names(which.max(table(cleanTraining$bathroomcnt)))
}

cleanTraining$bathroomcnt <- as.numeric(ifelse(cleanTraining$bathroomcnt == 0, 
                                               mode_(cleanTraining$bathroomcnt), 
                                               cleanTraining$bathroomcnt))

cleanTraining$age_of_home = round(as.numeric(impute(cleanTraining$age_of_home, mean)), 0)
cleanTraining$yearbuilt <- NULL

###############################################################
# Imputation by Weighted Sample
###############################################################

impDistr <- function(vec) {
  i=1; s=c()
  samples <- sort(unique(vec))
  numNAs <- sum(is.na(vec))
  x <- table(vec)/length(vec)
  while (i <= length(x)) {
    s[i] = x[[i]][1]
    i = i + 1
  }
  return(sample(samples, numNAs, prob = s, replace=T))
}

cleanTraining$garagecarcnt[is.na(cleanTraining$garagecarcnt)] = impDistr(cleanTraining$garagecarcnt)

cleanTraining$airconditioningtypeid[is.na(cleanTraining$airconditioningtypeid)] = impDistr(cleanTraining$airconditioningtypeid)

cleanTraining$buildingqualitytypeid[is.na(cleanTraining$buildingqualitytypeid)] = impDistr(cleanTraining$buildingqualitytypeid)

###############################################################
# Remove blank zip code rows 
###############################################################

cleanTraining <- cleanTraining %>% filter(!(is.na(cleanTraining$regionidzip)))

###############################################################
# Changing variables types
###############################################################

cols_reduced <- names(cleanTraining)
cols_factors <- c('airconditioningtypeid', 'buildingqualitytypeid', 'decktypeid',
                  'fireplacecnt', 'hashottuborspa', 'heatingorsystemtypeid', 
                  'poolcnt', 'propertylandusetypeid', 'regionidcounty', 
                  'regionidzip', 'taxdelinquencyflag', 'month')
cleanTraining[cols_factors] <- lapply(cleanTraining[cols_factors], factor)

cleanTraining$garagecarcnt = as.numeric(cleanTraining$garagecarcnt)
cleanTraining$unitcnt = as.numeric(cleanTraining$unitcnt)

###############################################################
# Rename Binary Variables to Flags
###############################################################
cleanTraining = rename(cleanTraining, acflag = airconditioningtypeid, deckflag = decktypeid, 
                       fireplaceflag = fireplacecnt, hottubflag = hashottuborspa, heatflag = heatingorsystemtypeid, poolflag = poolcnt)


###############################################################
# Rename Binary Variables to Flags
###############################################################

cleanTraining$property_group = as.factor(ifelse(cleanTraining$propertylandusetypeid %in% c(31,46,47), "Commercial",
                                                       ifelse(cleanTraining$propertylandusetypeid %in% c(266,267,246,247,248), "Apartment",
                                                              ifelse(cleanTraining$propertylandusetypeid %in% c(269,290,291,274,270), "Land", "House"))))

cleanTraining$building_quality = as.factor(ifelse(cleanTraining$buildingqualitytypeid %in% c(1,2,3,4), "Good",
                                                       ifelse(cleanTraining$buildingqualitytypeid %in% c(5,6,7,8), "Average", "Bad")))

cleanTraining$propertylandusetypeid <- NULL
cleanTraining$buildingqualitytypeid <- NULL
cleanTraining$transactiondate <- NULL

save(cleanTraining, file='cleanTraining_final.Rda')

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
# heatingorsystemtypeid: assumed all NAs were 0 

### Performed "cart" method impuatation from MICE pacakge, 
### "cart" method stands for classification and regression trees
### performs a calssification and regression tree analysis to impute values. 
### Imputed values inferred from other variables.
# garagecarcnt, garagetotalsqft, lotsizesquarefeet, calculatedfinishedsquarefeet, 
# buildingqualitytypeid, airconditioningtypeid