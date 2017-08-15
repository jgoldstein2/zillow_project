library(dplyr)
library(DT)
library(car)
library(caret)
library(MASS)
library(vcd)
library(lubridate)

# load('cleanTraining_final.Rda')

cleanTraining = data.table(cleanTraining)
###############################################################
# Feature Engineering and Selection 
###############################################################

# cleanTraining$property_group_apt <- ifelse(cleanTraining$property_group == 'Apartment', 1, 0)
# cleanTraining$property_group_com <- ifelse(cleanTraining$property_group == 'Commercial', 1, 0)
# cleanTraining$property_group_house <- ifelse(cleanTraining$property_group == 'House', 1, 0)
# cleanTraining$property_group_land <- ifelse(cleanTraining$property_group == 'Land', 1, 0)
# 
# cleanTraining$building_quality_good <- ifelse(cleanTraining$building_quality == 'Good', 1, 0)
# cleanTraining$building_quality_avg <- ifelse(cleanTraining$building_quality == 'Average', 1, 0)
# cleanTraining$building_quality_bad <- ifelse(cleanTraining$building_quality == 'Bad', 1, 0)
# 
cleanTraining$valueratioNF = cleanTraining$taxvaluedollarcnt / cleanTraining$taxamount
cleanTraining$assessValueMetricNF = cleanTraining$structuretaxvaluedollarcnt / cleanTraining$landtaxvaluedollarcnt
cleanTraining$livingareapropNF = cleanTraining$calculatedfinishedsquarefeet / cleanTraining$lotsizesquarefeet 
cleanTraining$totalroomNF = cleanTraining$bathroomcnt + cleanTraining$bedroomcnt

# cleanTraining$regionidzip <- as.factor(cleanTraining$regionidzip)
cleanTraining$regionidcounty <- as.factor(cleanTraining$regionidcounty)

cols_drop <- c("building_quality", "property_group", 'regionidzip', 'month')
cleanTraining <- cleanTraining[,!(names(cleanTraining) %in% cols_drop)]

###############################################################
# Multiple Linear Regression Using Coefficient Significance
###############################################################

### This is for the train and test set, may not need
set.seed(0)
train = sample(1:nrow(cleanTraining), 7.5*nrow(cleanTraining)/10)
test = (-train) 

### I removed land property and bad building quality because the coefficients returned NA
multLinear_train = lm(logerror ~ . -parcelid -totalroomNF, data = cleanTraining[train,])
summary(multLinear_train)
(sum(multLinear_train$residuals**2)) # 1730.171
vif(multLinear_train)
train1_pred <- predict(multLinear_train, cleanTraining[test,])

train1_pred
actual_logerror <- cleanTraining[test, 'logerror']
RSS_test1 <- as.data.frame(cbind(train1_pred, actual_logerror))
RSS_test1
RSS_test1 <- RSS_test1 %>% mutate(Residuals = actual_logerror - train1_pred)
sum(RSS_test1$Residuals)**2

### I removed these coefficients because the VIF for a lot of them were crazy big,
### Do not remove all of one category, use better judgement to select which variables to use
multLinear_train2 = lm(logerror ~ . -parcelid -taxvaluedollarcnt -landtaxvaluedollarcnt 
                       -taxamount -longitude -latitude -regionidcounty -totalroomNF, 
                       data = cleanTraining[train,]) # can try removing different tax values

summary(multLinear_train2)
# plot(multLinear_train2)
vif(multLinear_train2)

### This is the model with only significant coefficients, as per the summary from 2
multLinear_train3 = lm(logerror ~ +bathroomcnt +calculatedfinishedsquarefeet +hottubflag
                                  +lotsizesquarefeet +poolflag +unitcnt +taxdelinquencyflag
                                  +age_of_home +valueratioNF +livingareapropNF, 
                                   data = cleanTraining[train,])

multLinear_train3$coefficients
(sum(multLinear_train3$residuals**2)) # 1735.612
summary(multLinear_train3)
vif(multLinear_train3)

train3_pred <- predict(multLinear_train3, cleanTraining[test,])

###############################################################
# Multiple Linear Regression Using Stepwise Selection
###############################################################

### AIC
model.empty = lm(logerror ~ 1, data = cleanTraining[train,])
model.full = lm(logerror ~ . -parcelid -totalroomNF, data = cleanTraining[train,])
scope = list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

summary(forwardAIC)
vif(forwardAIC) # some of the VIFs are extremely high
(sum(forwardAIC$residuals**2)) # 1730.5
(sum(backwardAIC$residuals**2)) # 1730.299
(sum(bothAIC.empty$residuals**2)) # 1730.5
(sum(bothAIC.full$residuals**2)) # 1730.299

bothAIC.full_pred <- predict(bothAIC.full, cleanTraining[test,])

### BIC
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))

summary(forwardBIC)
vif(forwardBIC) # some of the VIFs are extremely high
(sum(forwardBIC$residuals**2)) # 1730.576
(sum(backwardBIC$residuals**2)) # 1730.576
(sum(bothBIC.empty$residuals**2)) # 1730.576
(sum(bothBIC.full$residuals**2)) # 1730.576


fwdBIC_pred <- predict(forwardBIC, cleanTraining[test,])

###############################################################
# Multiple Linear Regression Using Caret Package
###############################################################

### An alternative way to select feature stepwise
multLinear_train4_var <- 
    train(logerror~. -parcelid -totalroomNF, data=cleanTraining[train,], 
          method="glmStepAIC", k=2,
          trControl=trainControl(method="none"), preProc=c('center', 'scale'))

summary(multLinear_train4_var)
multLinear_train4_var$finalModel

# Based on results from multLinear_train4_var, the following variables were kept in
multLinear_train4 <- 
    lm(logerror ~ +calculatedfinishedsquarefeet +fireplaceflag +hottubflag + landtaxvaluedollarcnt
       +lotsizesquarefeet +poolflag +unitcnt +taxvaluedollarcnt + taxamount 
       +livingareapropNF +structuretaxvaluedollarcnt +taxdelinquencyflag, 
       data = cleanTraining[train,])

summary(multLinear_train4)
vif(multLinear_train4) # some VIFs are extremely high
(sum(multLinear_train4$residuals**2)) # 1730.541

train4_pred <- predict(multLinear_train4, cleanTraining[test,])

###############################################################
# Anova Tests
###############################################################

anova(multLinear_train3 ,multLinear_train)
anova(multLinear_train4 ,multLinear_train)
anova(bothAIC.full, multLinear_train)
anova(forwardBIC, multLinear_train)

# The ANOVA test results indicate that our full model is better than 
# all of the reduce models.  However, this my be attributed to the large 
# sample size (variance becomes more influential).  This actually makes
# sense with our results since the full model has the lowest RSS.

# I think a few of our samples violated Cook's distance rule and 
# the inlfuence plots suggested the same.  We may wat to consider 
# removing outliers from our training set, especially in the variables 
# we keep.

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

# load('cleanProperties_final.Rda')

###This must be updated if any new features are added or dropped!

# cleanProperties$property_group_apt <- ifelse(cleanProperties$property_group == 'Apartment', 1, 0)
# cleanProperties$property_group_com <- ifelse(cleanProperties$property_group == 'Commercial', 1, 0)
# cleanProperties$property_group_house <- ifelse(cleanProperties$property_group == 'House', 1, 0)
# cleanProperties$property_group_land <- ifelse(cleanProperties$property_group == 'Land', 1, 0)
# 
# cleanProperties$building_quality_good <- ifelse(cleanProperties$building_quality == 'Good', 1, 0)
# cleanProperties$building_quality_avg <- ifelse(cleanProperties$building_quality == 'Average', 1, 0)
# cleanProperties$building_quality_bad <- ifelse(cleanProperties$building_quality == 'Bad', 1, 0)

cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount
cleanProperties$assessValueMetricNF = cleanProperties$structuretaxvaluedollarcnt / cleanProperties$landtaxvaluedollarcnt
cleanProperties$livingareapropNF = cleanProperties$calculatedfinishedsquarefeet / cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt

# cleanProperties$regionidzip <- as.factor(cleanProperties$regionidzip)
cleanProperties$regionidcounty <- as.factor(cleanProperties$regionidcounty)

cols_drop <- c("building_quality", "property_group",'regionidzip')
cleanProperties <- cleanProperties[,!(names(cleanProperties) %in% cols_drop)]

###############################################################
# Make Prediction for Submission 
###############################################################

makePrediction <- function(model, newdata, months, labels) {
  predictions <- cleanProperties[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cleanProperties$month <- months[i]
    predictions[, labels[i]] <- predict(multLinear_train, newdata = cleanProperties) #change 1st argument to whichever model you want predicitons from
  }
  write.csv(x = predictions, file = "multLinear_full.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}
                      # change 1st argument to whichever model you want predictions from
makePrediction(multLinear_train, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))


