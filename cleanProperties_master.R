library(DT)
library(data.table)
library(dplyr)
library(lubridate)
library(Hmisc)
library(mice)
library(lattice)

###############################################################
# Reading files 
###############################################################

properties_df = as.data.frame(fread("./properties_2016.csv"))

###############################################################
# Dropping columns
###############################################################
name_list <- names(properties_df)

cols_drop <- c("assessmentyear", "architecturalstyletypeid", "basementsqft", 
               "buildingclasstypeid", "calculatedbathnbr", "threequarterbathnbr", 
               "finishedfloor1squarefeet", "finishedsquarefeet12", "finishedsquarefeet13",        
               "finishedsquarefeet15", "finishedsquarefeet50", "finishedsquarefeet6", 
               "fips", "fireplaceflag", "fullbathcnt", "numberofstories", "poolsizesum",
               "pooltypeid10", "pooltypeid2", "pooltypeid7", "propertycountylandusecode", 
               "propertyzoningdesc", "rawcensustractandblock", "regionidcity", "censustractandblock",
               "regionidneighborhood", "roomcnt", "storytypeid", "typeconstructiontypeid", 
               "yardbuildingsqft17", "yardbuildingsqft26", "taxdelinquencyyear", "garagetotalsqft")

cleanProperties <- properties_df[ , !(names(properties_df) %in% cols_drop)]

###############################################################
# Mutating/Adding Features
###############################################################

cleanProperties = cleanProperties %>% mutate(latitude = latitude/1e6, longitude = longitude/1e6)
cleanProperties <- cleanProperties %>% mutate(age_of_home = 2017 - cleanProperties$yearbuilt)

###############################################################
# Imputation of Binary Variables
###############################################################

cleanProperties$poolcnt[is.na(cleanProperties$poolcnt)] = 0

cleanProperties$unitcnt[is.na(cleanProperties$unitcnt)] = 1

cleanProperties$decktypeid = ifelse(is.na(cleanProperties$decktypeid), 0, 1) 

cleanProperties$fireplacecnt = ifelse(is.na(cleanProperties$fireplacecnt), 0, 1) 

cleanProperties$taxdelinquencyflag = ifelse(cleanProperties$taxdelinquencyflag == '', 0, 1)

cleanProperties$hashottuborspa = ifelse(cleanProperties$hashottuborspa == '', 0, 1)

cleanProperties$airconditioningtypeid = ifelse(is.na(cleanProperties$airconditioningtypeid),
                                               ifelse(cleanProperties$heatingorsystemtypeid == 2, 1, cleanProperties$airconditioningtypeid),
                                               ifelse(cleanProperties$airconditioningtypeid == 5, 0, 1))

cleanProperties$heatingorsystemtypeid = ifelse(is.na(cleanProperties$heatingorsystemtypeid), 0, 
                                               ifelse(cleanProperties$heatingorsystemtypeid == 13, 0, 1))

###############################################################
# Imputation by Mean/Mode
###############################################################

cleanProperties$taxvaluedollarcnt = as.numeric(impute(cleanProperties$taxvaluedollarcnt, mean))

cleanProperties$structuretaxvaluedollarcnt = as.numeric(impute(cleanProperties$structuretaxvaluedollarcnt, mean))

cleanProperties$landtaxvaluedollarcnt = as.numeric(impute(cleanProperties$landtaxvaluedollarcnt, mean))

cleanProperties$taxamount = as.numeric(impute(cleanProperties$taxamount, mean))

mode_ <- function(x) {
  names(which.max(table(cleanProperties$bathroomcnt)))
}

cleanProperties$bathroomcnt <- as.numeric(ifelse(cleanProperties$bathroomcnt == 0, 
                                                 mode_(cleanProperties$bathroomcnt), 
                                                 cleanProperties$bathroomcnt))

cleanProperties$age_of_home = round(as.numeric(impute(cleanProperties$age_of_home, mean)), 0)
cleanProperties$yearbuilt <- NULL

cleanProperties$lotsizesquarefeet <- as.numeric(impute(cleanProperties$lotsizesquarefeet, mean))

cleanProperties$calculatedfinishedsquarefeet <- as.numeric(impute(cleanProperties$calculatedfinishedsquarefeet, mean))

###############################################################
# Remove blank zip code rows 
###############################################################

cleanProperties <- cleanProperties %>% filter(!(is.na(cleanProperties$regionidzip)))

###############################################################
# Changing variables types
###############################################################

cols_reduced <- names(cleanProperties)

cols_factors <- c('airconditioningtypeid', 'buildingqualitytypeid', 'decktypeid',
                  'fireplacecnt', 'hashottuborspa', 'heatingorsystemtypeid', 
                  'poolcnt', 'propertylandusetypeid', 'regionidcounty', 
                  'regionidzip', 'taxdelinquencyflag')#, 'month')
cleanProperties[cols_factors] <- lapply(cleanProperties[cols_factors], factor)

cleanProperties$garagecarcnt = as.numeric(cleanProperties$garagecarcnt)

cleanProperties$unitcnt = as.numeric(cleanProperties$unitcnt)

###############################################################
# Imputation by Random Sampling
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

cleanProperties$garagecarcnt[is.na(cleanProperties$garagecarcnt)] = impDistr(cleanProperties$garagecarcnt)

cleanProperties$airconditioningtypeid[is.na(cleanProperties$airconditioningtypeid)] = impDistr(cleanProperties$airconditioningtypeid)

cleanProperties$buildingqualitytypeid[is.na(cleanProperties$buildingqualitytypeid)] = impDistr(cleanProperties$buildingqualitytypeid)

###############################################################
# Rename Binary Variables to Flags
###############################################################

cleanProperties = rename(cleanProperties, acflag = airconditioningtypeid, deckflag = decktypeid, 
                         fireplaceflag = fireplacecnt, hottubflag = hashottuborspa, heatflag = heatingorsystemtypeid, poolflag = poolcnt)


###############################################################
# Rename Binary Variables to Flags
###############################################################

cleanProperties$property_group = as.factor(ifelse(cleanProperties$propertylandusetypeid %in% c(31,46,47), "Commercial",
                                                  ifelse(cleanProperties$propertylandusetypeid %in% c(266,267,246,247,248), "Apartment",
                                                         ifelse(cleanProperties$propertylandusetypeid %in% c(269,290,291,274,270), "Land", "House"))))

cleanProperties$building_quality = as.factor(ifelse(cleanProperties$buildingqualitytypeid %in% c(1,2,3,4), "Good",
                                                    ifelse(cleanProperties$buildingqualitytypeid %in% c(5,6,7,8), "Average", "Bad")))

cleanProperties$propertylandusetypeid <- NULL

cleanProperties$buildingqualitytypeid <- NULL

save(cleanProperties, file='cleanProperties_final.Rda')
