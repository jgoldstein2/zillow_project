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
dim(cleanTraining)
View(cleanTraining)
table(cleanTraining$hashottuborspa)
blank_count = as.data.frame(sapply(cleanTraining, function(x) (sum(x == "")/length(x)) * 100))

# save(cleanTraining, file='cleanTraining_master.Rda')
# 
# names(cleanTraining)
###############################################################
# Scratch Space
###############################################################
cleanTraining2 <- cleanTraining

###############################################################
# Changing variables to binary for multiple linear regression
###############################################################

cleanTraining$property_group_apt <- ifelse(cleanTraining$property_group == 'Apartment', 1, 0)
cleanTraining$property_group_com <- ifelse(cleanTraining$property_group == 'Commercial', 1, 0)
cleanTraining$property_group_house <- ifelse(cleanTraining$property_group == 'House', 1, 0)
cleanTraining$property_group_land <- ifelse(cleanTraining$property_group == 'Land', 1, 0)

cleanTraining$building_quality_good <- ifelse(cleanTraining$building_quality == 'Good', 1, 0)
cleanTraining$building_quality_avg <- ifelse(cleanTraining$building_quality == 'Average', 1, 0)
cleanTraining$building_quality_bad <- ifelse(cleanTraining$building_quality == 'Bad', 1, 0)

cols_drop <- c("regionidzip", "regionidcounty", "building_quality", "property_group")
cleanTraining <- cleanTraining[,!(names(cleanTraining) %in% cols_drop)]

###############################################################
# Reading files 
###############################################################
library(DT)
library(data.table)
train_df <- fread('train_2016_v2.csv')
properties_df <- fread('properties_2016.csv')
# sample_df <- fread('sample_submission.csv')

###############################################################
# joining tain_df with properties_df
###############################################################
library(dplyr)
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
               "yardbuildingsqft17", "yardbuildingsqft26", "taxdelinquencyyear")

cleanTraining <- training_df[ , !(names(training_df) %in% cols_drop)]

###############################################################
# Mutating/adding Features
###############################################################

library(lubridate)
cleanTraining = cleanTraining %>% mutate(latitude = latitude/1e6, longitude = longitude/1e6)
cleanTraining = cleanTraining %>% mutate(month = month(transactiondate))
cleanTraining = cleanTraining %>% mutate(totalroom = bathroomcnt + bedroomcnt)

###############################################################
# Imputation of Binary Variables
###############################################################

cleanTraining$poolcnt[is.na(cleanTraining$poolcnt)] = 0
cleanTraining$unitcnt[is.na(cleanTraining$unitcnt)] = 1

cleanTraining$decktypeid[is.na(cleanTraining$decktypeid)] = 0
cleanTraining$decktypeid = ifelse(cleanTraining$decktypeid == 66, 1, 0)

cleanTraining$fireplacecnt[is.na(cleanTraining$fireplacecnt)] = 0
cleanTraining$fireplacecnt = ifelse(cleanTraining$fireplacecnt == 0, 0, 1)

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
imputed.taxvaluedollarcnt = as.numeric(impute(cleanTraining$taxvaluedollarcnt, mean))
cleanTraining$taxvaluedollarcnt = imputed.taxvaluedollarcnt

imputed.structuretaxvaluedollarcnt = as.numeric(impute(cleanTraining$structuretaxvaluedollarcnt, mean))
cleanTraining$structuretaxvaluedollarcnt = imputed.structuretaxvaluedollarcnt

imputed.landtaxvaluedollarcnt = as.numeric(impute(cleanTraining$landtaxvaluedollarcnt, mean))
cleanTraining$landtaxvaluedollarcnt = imputed.landtaxvaluedollarcnt

imputed.taxamount = as.numeric(impute(cleanTraining$taxamount, mean))
cleanTraining$taxamount = imputed.taxamount

mode_ <- function(vec) {
  names(which.max(table(vec)))
}

cleanTraining$bathroomcnt <- as.numeric(ifelse(cleanTraining$bathroomcnt == 0, 
                                                  mode_(cleanTraining$bathroomcnt), 
                                                            cleanTraining$bathroomcnt))

cleanProperties$bathroomcnt <- as.numeric(impute(cleanProperties$bathroomcnt, mode_(cleanProperties$bathroomcnt)))

cleanProperties$bedroomcnt <- as.numeric(impute(cleanProperties$bedroomcnt, mode_(cleanProperties$bedroomcnt)))

cleanProperties$regionidcounty <- as.numeric(impute(cleanProperties$regionidcounty, mode_(cleanProperties$regionidcounty)))

cleanProperties$longitude <- as.numeric(impute(cleanProperties$longitude, mode_(cleanProperties$longitude)))

cleanProperties$latitude <- as.numeric(impute(cleanProperties$latitude, mode_(cleanProperties$latitude)))

cleanProperties$regionidzip <- as.numeric(impute(cleanProperties$regionidzip, mode_(cleanProperties$regionidzip)))

###############################################################
# Mutating Age of Home Variable and Imputing
###############################################################
cleanTraining <- cleanTraining %>% mutate(age_of_home = 2017 - cleanTraining$yearbuilt)

imputed.age_of_home = as.numeric(round(impute(cleanTraining$age_of_home, mean), 0))
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
                  'regionidzip', 'taxdelinquencyflag', 'month')
cleanTraining[cols_factors] <- lapply(cleanTraining[cols_factors], factor)

cleanTraining$garagecarcnt = as.numeric(cleanTraining$garagecarcnt)
cleanTraining$unitcnt = as.numeric(cleanTraining$unitcnt)
cleanTraining$garagetotalsqft = as.numeric(cleanTraining$garagetotalsqft)
cleanTraining$month = as.factor(cleanTraining$month)
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
# Mutating building type id to groups
###############################################################

bldg_vec <- unique(cleanTraining$buildingqualitytypeid)



###############################################################
# Rename flag variables so they make more sense
###############################################################
cleanTraining = rename(cleanTraining, acflag = airconditioningtypeid, deckflag = decktypeid, 
                       fireplaceflag = fireplacecnt, hottubflag = hashottuborspa, heatflag = heatingorsystemtypeid, poolflag = poolcnt)


###############################################################
# Exploring the Imputations
###############################################################

# Shows distribution of imputed values within the existing data set
library(lattice)

xyplot(imp.train_raw, calculatedfinishedsquarefeet ~ logerror)
xyplot(imp.train_raw, garagetotalsqft ~ logerror)
xyplot(imp.train_raw, lotsizesquarefeet ~ logerror)
xyplot(imp.train_raw, buildingqualitytypeid ~ logerror)
xyplot(imp.train_raw, airconditioningtypeid ~ logerror)
xyplot(imp.train_raw, garagecarcnt ~ logerror)

densityplot(imp.train_raw, logerror ~ calculatedfinishedsquarefeet + lotsizesquarefeet + garagetotalsqft)
densityplot(imp.train_raw, logerror ~ buildingqualitytypeid + airconditioningtypeid + garagecarcnt)

# Checking the values it assigned to missing variables
# Some viz would be good to explore the imputed values/data
table(imp.train_raw$imp$calculatedfinishedsquarefeet)
table(imp.train_raw$imp$buildingqualitytypeid)
table(imp.train_raw$imp$garagecarcnt)
table(imp.train_raw$imp$garagetotalsqft)
table(imp.train_raw$imp$lotsizesquarefeet)
table(imp.train_raw$imp$airconditioningtypeid)

imp.train_raw

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
### performs a calssification and regression tree analysis to impute values. 
### Imputed values inferred from other variables.
# garagecarcnt, garagetotalsqft, lotsizesquarefeet, calculatedfinishedsquarefeet, 
# buildingqualitytypeid, airconditioningtypeid


















###############################################################
# Feature Engineering and Selection 
###############################################################

library(dplyr)
cleanTraining$assessValueMetricNF = cleanTraining$structuretaxvaluedollarcnt / cleanTraining$landtaxvaluedollarcnt
cleanTraining$livingareaMetricNF = cleanTraining$calculatedfinishedsquarefeet * cleanTraining$lotsizesquarefeet 
cleanTraining$totalroomNF = cleanTraining$bathroomcnt + cleanTraining$bedroomcnt

cleanTraining <- cleanTraining %>% dplyr::mutate(luxuryMetric = ifelse(building_quality == 'Good', (as.numeric(hottubflag)+as.numeric(poolflag)+as.numeric(deckflag))*totalroomNF,
                                                                       ifelse(building_quality == 'Average', ((as.numeric(hottubflag)+as.numeric(poolflag)+as.numeric(deckflag))/2)*totalroomNF,
                                                                              ((as.numeric(hottubflag)+as.numeric(poolflag)+as.numeric(deckflag))/3)*totalroomNF)))

taxgroup = cleanTraining %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanTraining = dplyr::left_join(cleanTraining, taxgroup, by='regionidzip')

cols_drop <- c("bathroomcnt","bedroomcnt", "regionidzip", "hottubflag", "taxvaluedollarcnt", "taxamount",
               "taxdelinquencyflag", "deckflag", "unitcnt", "month", "building_quality", "property_group",
               "acflag", 'heatflag', "longitude", "latitude", "regionidcounty", "poolflag",
               "structuretaxvaluedollarcnt", "landtaxvaluedollarcnt")

cleanTraining <- cleanTraining[ , !(names(cleanTraining) %in% cols_drop)]

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

# load('cleanProperties_final.Rda')

###This must be updated if any new features are added or dropped!

cleanProperties$assessValueMetricNF = cleanProperties$structuretaxvaluedollarcnt / cleanProperties$landtaxvaluedollarcnt
cleanProperties$livingareaMetricNF = cleanProperties$calculatedfinishedsquarefeet * cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt

cleanProperties <- cleanProperties %>% dplyr::mutate(luxuryMetric = ifelse(building_quality == 'Good', (as.numeric(hottubflag)+as.numeric(poolflag)+as.numeric(deckflag)),
                                                                           ifelse(building_quality == 'Average', (as.numeric(hottubflag)+as.numeric(poolflag)+as.numeric(deckflag))/2,
                                                                                  (as.numeric(hottubflag)+as.numeric(poolflag)+as.numeric(deckflag))/3)))

taxgroup = cleanProperties %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanProperties = dplyr::left_join(cleanProperties, taxgroup, by='regionidzip')

cols_drop <- c("bathroomcnt","bedroomcnt", "regionidzip", "hottubflag", "taxvaluedollarcnt", "taxamount",
               "taxdelinquencyflag", "deckflag", "unitcnt", "month", "building_quality", "property_group",
               "acflag", 'heatflag', "longitude", "latitude", "regionidcounty", "poolflag",
               "structuretaxvaluedollarcnt", "landtaxvaluedollarcnt")

cleanProperties <- cleanProperties[ , !(names(cleanProperties) %in% cols_drop)]


