#properties = read.csv("./Zillow/properties_2016.csv")
#train = read.csv("./Zillow/train_2016_v2.csv")

load("C:/Users/jgoldste/Documents/Zillow/zillow_project/training_df.Rda")

# Get percentage of missing values in each column
na_count = as.data.frame(sapply(training_df, function(x) (sum(is.na(x))/length(x)) * 100))

# See if any variables have a single value and can be dropped
rapply(training_df, function(x)length(unique(x)))
unique(training_df$assessmentyear) 
# assessmentyear only has one value (2015), it can probably be dropped

####Variables With Missing Values####

unique(training_df$airconditioningtypeid)
table(training_df$airconditioningtypeid)
# Represents type of cooling system in home
# There is a None category (5), we may be able to say missing values should be = None
# Should be a factor (not numerical)

unique(training_df$architecturalstyletypeid)
table(training_df$architecturalstyletypeid)
# Represents architectural style of home
# Huge number of missing values (99%), not sure if there's a way to determine correct value
# May want to throw out this variable

unique(training_df$basementsqft)
table(training_df$basementsqft)
# Represents below-ground living area square feet
# Huge number of missing values (99%), seems unlikely that only 20 homes have finished basements
# Could assume answer is 0 or remove

unique(training_df$buildingclasstypeid)
table(training_df$buildingclasstypeid)
# Represents building framing type
# Five possible values here, but the only actual values are NA and 4 (wood framed homes)
# This value doesn't seem like it would be useful since almost all homes are uncategorized, 
# may want to remove

unique(training_df$buildingqualitytypeid)
table(training_df$buildingqualitytypeid)
# Represents overall assessment of condition of building (1 = best, 12 = worst)
# 36% of this data is missing, but variable seems like it would be useful (assesses building condition)
# May want to impute using the mean value - most values are either 4 or 7

unique(training_df$calculatedbathnbr)
table(training_df$calculatedbathnbr)
# This variable shows number of bathrooms, only 1% of data is missing
# May want to impute using the mean since most observations have a small # of bathrooms - we don't want to 
# amplify outliers

unique(training_df$decktypeid)
table(training_df$decktypeid)
# Represents type of deck (if any)
# Most values are missing (99%) and the only values are NA or 66
# It seems like missing values probably represent homes without decks, we might want to create a "no deck" value

unique(training_df$finishedfloor1squarefeet)
table(training_df$finishedfloor1squarefeet)
# Variable shows size of finished living area, there are many different values
# Missing values (93%) may mean there is no finished living area, or could just be missing
# We may want to randomly impute if we think a value exists, or drop because so many are missing

unique(training_df$calculatedfinishedsquarefeet)
# Variable shows total finished living area, there are many different values
# Less than 1% is missing; we may want to impute using random numbers or the mean

unique(training_df$finishedsquarefeet12)
# Variable shows finished living area, there are many different values
# 5% is missing; we may want to impute using random numbers or the mean
# Probably don't want to use all of these values that represent living area sqft because of multicollinearity

unique(training_df$finishedsquarefeet13)
# Variable shows living area perimeter, there are about 10 different values
# 99% is missing; we can probably drop this variable and use one of the others that represents living area sqft

unique(training_df$finishedsquarefeet15)
# Variable shows living area perimeter, there are many different values
# 96% is missing; we can probably drop this variable and use one of the others that represents living area sqft

unique(training_df$finishedsquarefeet50)
# Variable shows total finished living area, description is the same as "calculatedfinishedsquarefeet"
# There are many values but 92% are missing; probably better to use "calculatedfinishedsquarefeet"

unique(training_df$finishedsquarefeet6)
# Variable shows base unfinished and finished area
# 99% are missing; we can probably drop this variable and use one of the finished sqft variables

unique(training_df$fireplacecnt)
# Represents number of fireplaces in the home, can be either NA or 1 - 5
# Missing values (89%) likely represent homes without fireplaces; we can probably impute with 0

unique(training_df$fullbathcnt)
# Represents number of full bathrooms in home, can be either NA or a number
# Missing values (1.3%) could represent homes without full bathrooms (another variable represents 3/4 bathrooms)
# Can impute with 0, or use total bathroom variable that has few missing values

unique(training_df$garagecarcnt)
table(training_df$garagecarcnt)
# Represents number of garages in home, can either be NA, 0, or number greater than 0
# Missing values (67%) could represent homes without garages even though there is a 0 value

unique(training_df$garagetotalsqft)
table(training_df$garagetotalsqft)
# Represents garage square footage, can be NA, 0, or larger number
# Missing values (67%) match number of missing values for "garagecarcnt"
# Could represent homes without garages even though they should be coded as 0

unique(training_df$heatingorsystemtypeid)
table(training_df$heatingorsystemtypeid)
# Represents type of home heating system
# There is a None category (13), we may be able to say missing values should be = None
# Should be a factor (not numerical)

unique(training_df$lotsizesquarefeet)
table(training_df$lotsizesquarefeet)
# Represents area of the lot, there are many different values
# 11% of values are missing, we can probably impute using mean

unique(training_df$poolcnt)
table(training_df$poolcnt)
# Represents number of pools, value is either NA or 1
# NA likely represents homes without a pool, we can impute with 0

unique(training_df$poolsizesum)
table(training_df$poolsizesum)
# Represents square footage of pools, there are many different values
# 99% of values are missing (more than the number of homes without pools), so we may want to delete 

unique(training_df$pooltypeid10)
# Represents homes with a spa or hot tub, value is either NA or 1 (has hot tub)
# 99% of values are missing, we can probably impute with 0 

unique(training_df$pooltypeid2)
# Represents homes with a pool and spa or hot tub, value is either NA or 1
# 99% of values are missing, we can probably impute with 0 

unique(training_df$pooltypeid7)
# Represents homes with a pool and NO spa or hot tub, value is either NA or 1
# 82% of values are missing, we can probably impute with 0
# We should probably cut down on the number of pool-related variables to avoid multicollinearity

unique(training_df$regionidcity)
# Represents city where home is located, 2% of values are missing
# Might be able to impute using other regional variables (like zipcode) or use another regional variable
# with fewer missing values

unique(training_df$regionidneighborhood)
# Represents neighborhood where home is located, 60% of values are missing
# Can probably use another regional variable with fewer missing values

unique(training_df$regionidzip)
# Represents zip code where home is located, only .03% of values are missing
# Not sure what to do about missing values - maybe drop since there are so few, or impute based on another value?

unique(training_df$storytypeid)
# Represents types of floors in a multistory, many possible values but only NA and 7 are present
# 99% of values are missing, we should probably drop it

unique(training_df$threequarterbathnbr)
table(training_df$threequarterbathnbr)
# Represents number of 3/4 bathrooms in home, either NA or 1-4
# Can impute with 0, or use total bathroom variable that has few missing values

unique(training_df$typeconstructiontypeid)
# Represents type of construction material used, either NA, 4,6,13
# 99% of values are missing, we should probably drop it 

unique(training_df$unitcnt)
table(training_df$unitcnt)
# Represents number of units the structure is built into (2=duplex, etc.)
# 35% of values are missing and most values equal 1, we may be able to impute with 1

unique(training_df$yardbuildingsqft17)
table(training_df$yardbuildingsqft17)
# Represents patio in the yard - there are many possible numerical values
# Not exactly sure what these values represent and 97% are missing, may want to delete

unique(training_df$yardbuildingsqft26)
# Represents storage shed/building in the yard - there are many possible numerical values
# Not exactly sure what these values represent and 99% are missing, may want to delete

unique(training_df$yearbuilt)
table(training_df$yearbuilt)
# Represents the year the home was built
# Less than 1% of data is missing - we may want to impute with the most common year or drop those values

unique(training_df$numberofstories)
table(training_df$numberofstories)
# Represents number of levels, values are either NA or 1-4
# 77% of values are missing, may want to drop because it's hard to determine the correct value

unique(training_df$structuretaxvaluedollarcnt)
# Represents assessed value of the built structure, values are continuous
# .4% of values are missing, we may want to impute with mean or by using 'taxvaluedollarcnt' 
# which has fewer missing values

unique(training_df$taxvaluedollarcnt)
# Represents total tax assessed value, values are continous
# Only .1% of values are missing, we can probably impute with the mean

unique(training_df$landtaxvaluedollarcnt)
# Represents assessed value of the land, values are continuous
# Only .1% of values are missing, we can probably impute with the mean
# We should check multicolinearity with other assessed value variables

unique(training_df$taxamount)
# Represents the total property tax, values are continuous
# Less than .1% of values are missing, we can probably impute with the mean (or drop?)

unique(training_df$taxdelinquencyyear)
# Represents year of unpaid property taxes
# 98% of values are missing, missing values probably represent homes without delinquencies
# If we use this variable, we may want to create a variable with two values (delinquent and not delinquent)

####Variables Without Missing Values####
unique(training_df$bathroomcnt)
# Represents total number of bathrooms, values are numeric (0.0 - 20.0)
# This is probably the most helpful bathroom-related variable since no values are missing

unique(training_df$bedroomcnt)
# Represents number of bedrooms in the home, values are numeric (0-15)

unique(training_df$fips)
# Federal Information Processing Standards (FIPS) code identifies counties and county equivalents
# There are three unique values (should be factors)

unique(training_df$hashottuborspa)
# Represents whether home has hot tub or spa 
# Values are either "" or "true" - should probably be converted to 0 and 1 if we use

unique(training_df$latitude)
unique(training_df$longitude)
# Latitude and longitude of home

unique(training_df$propertycountylandusecode)
# Represents zoning by county level - there are about 80 different codes
# No missing values but there are "" values

unique(training_df$propertylandusetypeid)
# Represents type of land use the property is zoned for (eg, Mobile Homes)
# There are about 10 different numerical values (should be factors)

unique(training_df$propertyzoningdesc)
# Represents description of allowed land uses
# No missing values but over a thousand different codes - probably not useful to us

unique(training_df$rawcensustractandblock)
# Represents Census tract and block ID number
# There are thousands of different numbers - probably not useful except for mapping

unique(training_df$regionidcounty)
# Represents county where the property is located
# There are three unique values (should be factors)

unique(training_df$roomcnt)
# Represents total number of rooms in home, values are numeric (0-18)
# We probably don't want to use multiple room count variables because of multicollinearity

unique(training_df$fireplaceflag)
# Represents whether fireplace is present in the home
# Values are either "" or "true" - should be replaced with 0 and 1 if we use
# We probably should use this over "fireplacecnt" variable since there are no missing values here

unique(training_df$taxdelinquencyflag)
# Represents whether property taxes are past due in 2005
# Values are either "" or "Y" - should be replaced with 0 or 1 if we use
# This is probably a more useful variable than "taxdeliquency

unique(training_df$censustractandblock)
# Represents Census tract and block ID number
# There are thousands of different numbers - probably not useful except for mapping

#####Variables From Train Group#####
length(unique(training_df$parcelid))
# 90275 observations in the data frame, but only 90150 unique parcels - some must be listed more than once

transaction_dates = training_df %>% group_by(transactiondate) %>% summarise(n())
# 352 different dates ranging from 1/1/16 to 12/30/16