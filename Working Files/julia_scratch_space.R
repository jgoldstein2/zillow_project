library(lubridate)

load("C:/Users/jgoldste/Documents/Zillow/zillow_project/cleanTraining_master.Rda")

# Checking for blank values
blank_count = as.data.frame(sapply(cleanTraining, function(x) (sum(x == "")/length(x)) * 100))

#Mutate longitude and latitude 
cleanTraining = cleanTraining %>% mutate(latitude = latitude/1e6, longitude = longitude/1e6)

#Create month variable with month of transaction 
cleanTraining = cleanTraining %>% mutate(month = month(transactiondate))

#Create room count variable
cleanTraining = cleanTraining %>% mutate(totalroom = bathroomcnt + bedroomcnt)

#Look at structure
str(cleanTraining)

## Deck type needs to be fixed - values listed as 66 should be 1!
cleanTraining$decktypeid = ifelse(cleanTraining$decktypeid == 66, 1, 0)
cleanTraining$decktypeid = as.factor(cleanTraining$decktypeid)

## Fireplace count needs to be fixed - should be binary!
cleanTraining$fireplacecnt = as.factor(ifelse(cleanTraining$fireplacecnt == 0, 0, 1))

## Has hot tub or spa should be a factor 
cleanTraining$hashottuborspa = as.factor(cleanTraining$hashottuborspa)

## Tax delinquency flag should be a factor
cleanTraining$taxdelinquencyflag = as.factor(cleanTraining$taxdelinquencyflag)

## Convert age of home to numeric and get rid of decimal points
cleanTraining$age_of_home = round(as.numeric(cleanTraining$age_of_home),0)

##Convert tax variables to numeric
cleanTraining$structuretaxvaluedollarcnt = as.numeric(cleanTraining$structuretaxvaluedollarcnt)
cleanTraining$taxvaluedollarcnt = as.numeric(cleanTraining$taxvaluedollarcnt)
cleanTraining$landtaxvaluedollarcnt = as.numeric(cleanTraining$landtaxvaluedollarcnt)
cleanTraining$taxamount = as.numeric(cleanTraining$taxamount)

#Convert garage squarefeet to numeric
cleanTraining$garagetotalsqft = as.numeric(cleanTraining$garagetotalsqft)

#Rename flag variables so they make more sense

cleanTraining = rename(cleanTraining, acflag = airconditioningtypeid, deckflag = decktypeid, 
                       fireplaceflag = fireplacecnt, hottubflag = hashottuborspa, heatflag = heatingorsystemtypeid, poolflag = poolcnt)

save(cleanTraining, file = "cleanTraining2.rda")

# What should we do with zipcode? Can't leave it as a factor? Can we join other data based on latitude and longitude.
# What about lat/long? Right now they are numeric, does this make sense?
# Change imputation of bathroomcount to mode 