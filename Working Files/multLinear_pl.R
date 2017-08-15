library(dplyr)
library(DT)
library(car)
library(caret)
library(MASS)
library(vcd)

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

cols_drop <- c("building_quality", "property_group")
cleanTraining <- cleanTraining[,!(names(cleanTraining) %in% cols_drop)]

###############################################################
# Multiple Linear Regression Using Coefficient Significance
###############################################################

### This is for the train and test set, may not need

x = model.matrix(logerror ~ ., data=cleanTraining)[, -1] 
y = cleanTraining$logerror

set.seed(0)
train = sample(1:nrow(x), 7.5*nrow(x)/10)
test = (-train) 
y.test = y[test]

### I removed land property and bad building quality because the coefficients returned NA
multLinear_train = lm(logerror ~ . -parcelid -regionidzip -totalroomNF, data = cleanTraining[train,])
summary(multLinear_train)
### plot(multLinear_train)
vif(multLinear_train)

### I removed these coefficients because the VIF for a lot of them were crazy big,
### Do not remove all of one category, use better judgement to select which variables to use
multLinear_train2 = lm(logerror ~ . -parcelid -taxvaluedollarcnt -landtaxvaluedollarcnt 
                       -taxamount -longitude -latitude -regionidcounty -regionidzip -totalroomNF, 
                       data = cleanTraining[train,])

summary(multLinear_train2)
# plot(multLinear_train2)
vif(multLinear_train2)

### This is the model with only significant coefficients, as per the summary from 2
multLinear_train3 = lm(logerror ~ +bathroomcnt +calculatedfinishedsquarefeet +hottubflag
                                  +lotsizesquarefeet +poolflag +unitcnt +taxdelinquencyflag
                                  +age_of_home +valueratioNF +livingareapropNF, 
                                   data = cleanTraining[train,])
multLinear_train3$coefficients
summary(multLinear_train3)
vif(multLinear_train3)

predict(multLinear_train3, cleanTraining[test,])

###############################################################
# Multiple Linear Regression Using Stepwise Selection
###############################################################
model.empty = lm(logerror ~ 1, data = cleanTraining[train,])
model.full = lm(logerror ~ . -parcelid -property_group_land -building_quality_bad, data = cleanTraining[train,])
scope = list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

summary(forwardAIC)
# plot(forwardAIC)
# influencePlot(forwardAIC)
vif(forwardAIC)
# avPlots(forwardAIC)
# confint(forwardAIC)
# forwardAIC$fitted.values
predict(forwardAIC, cleanTraining[test,])

forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))

summary(forwardBIC)
# plot(forwardBIC)
# influencePlot(forwardBIC)
vif(forwardBIC)
# avPlots(forwardBIC)
# confint(forwardBIC)
# forwardAIC$fitted.values
predict(forwardBIC, cleanTraining[test,])

###############################################################
# Multiple Linear Regression Using Caret Package
###############################################################

### An alternative way to select feature stepwise
multLinear_train4_var <- 
    train(logerror~. -parcelid -regionidzip -totalroomNF, data=cleanTraining[train,], 
          method="glmStepAIC", k=2,
          trControl=trainControl(method="none"), preProc=c('center', 'scale'))

multLinear_train4 <- 
    lm(logerror ~ +bathroomcnt +calculatedfinishedsquarefeet +hottubflag
       +lotsizesquarefeet +poolflag +unitcnt +taxdelinquencyflag
       +age_of_home +valueratioNF +livingareapropNF, 
       data = cleanTraining[train,])

predict(multLinear_train4, cleanTraining[test,])

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

# load('cleanProperties_final.Rda')

###This must be updated if any new features are added or dropped!

cleanProperties$property_group_apt <- ifelse(cleanProperties$property_group == 'Apartment', 1, 0)
cleanProperties$property_group_com <- ifelse(cleanProperties$property_group == 'Commercial', 1, 0)
cleanProperties$property_group_house <- ifelse(cleanProperties$property_group == 'House', 1, 0)
cleanProperties$property_group_land <- ifelse(cleanProperties$property_group == 'Land', 1, 0)

cleanProperties$building_quality_good <- ifelse(cleanProperties$building_quality == 'Good', 1, 0)
cleanProperties$building_quality_avg <- ifelse(cleanProperties$building_quality == 'Average', 1, 0)
cleanProperties$building_quality_bad <- ifelse(cleanProperties$building_quality == 'Bad', 1, 0)

cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount
cleanProperties$assessValueMetricNF = cleanProperties$structuretaxvaluedollarcnt / cleanProperties$landtaxvaluedollarcnt
cleanProperties$livingareapropNF = cleanProperties$calculatedfinishedsquarefeet / cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt

cleanProperties$regionidzip <- as.factor(cleanProperties$regionidzip)
cleanProperties$regionidcounty <- as.factor(cleanProperties$regionidcounty)

cols_drop <- c("building_quality", "property_group")
cleanProperties <- cleanProperties[,!(names(cleanProperties) %in% cols_drop)]

###############################################################
# Make Prediction for Submission 
###############################################################

makePrediction <- function(model, newdata, months, labels) {
  predictions <- cleanProperties[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cleanProperties$month <- months[i]
    predictions[, labels[i]] <- predict(multLinear_train3, newdata = cleanProperties) #change 1st argument to whichever model you want predicitons from
  }
  write.csv(x = predictions, file = "submission.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}
                      # change 1st argument to whichever model you want predictions from
makePrediction(multLinear_train3, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))
      
