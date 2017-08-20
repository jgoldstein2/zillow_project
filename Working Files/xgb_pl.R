library(caretEnsemble)
library(caret)
library(xgboost)
library(plyr)
library(dplyr)

###############################################################
# Feature Engineering and Selection 
###############################################################

cleanTraining$valueratioNF = cleanTraining$taxvaluedollarcnt / cleanTraining$taxamount
cleanTraining$livingareapropNF = cleanTraining$calculatedfinishedsquarefeet / cleanTraining$lotsizesquarefeet 
cleanTraining$totalroomNF = cleanTraining$bathroomcnt + cleanTraining$bedroomcnt
## Julia's new features
cleanTraining$valueproductNF = cleanTraining$taxvaluedollarcnt * cleanTraining$taxamount
cleanTraining$agevalue = cleanTraining$valueproductNF * cleanTraining$age_of_home
cleanTraining$landratio = cleanTraining$structuretaxvaluedollarcnt / cleanTraining$landtaxvaluedollarcnt

taxgroup = cleanTraining %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanTraining = dplyr::left_join(cleanTraining, taxgroup, by='regionidzip')

cols_drop <- c("regionidzip", 'hottubflag', 'deckflag', 'heatflag','fireplaceflag')

cleanTraining <- cleanTraining[ , !(names(cleanTraining) %in% cols_drop)]

###############################################################
# Creation of Training and Test Sets
###############################################################

set.seed(0)
trainIndex <- createDataPartition(cleanTraining$month, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
# training set for 75%
subTrain <- cleanTraining[trainIndex,-1]
### subset for testing code
# subTrain1 <- subTrain[1:5000, ]

subTest = cleanTraining[-trainIndex,-1]
### subset for testing code
### subTest <- subTest[1:2000, ]

###############################################################
# Setting Summary f(x) and trainControl for CV
###############################################################
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

folds=10
repeats=5

myControl <- trainControl(method='cv', 
                          number=folds, 
                          repeats=repeats,
                          summaryFunction=maeSummary, # MUST be called in trainControl
                          returnResamp='none', 
                          returnData=FALSE, 
                          savePredictions="final", 
                          verboseIter=TRUE, 
                          allowParallel=TRUE)

PP <- c('center', 'scale')

###############################################################
# Model Building
###############################################################

### ***IT IS IMPORTANT TO HAVE ALL HYPERPARAMETERS*** ###
xgbGrid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(5, 10, 15),
  eta = c(.1, .01),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)

set.seed(0)
xgbModel <- train(
  logerror ~ ., 
  data=subTrain,
  metric="MAE",
  trControl=myControl,
  method='xgbTree', 
  tuneGrid=xgbGrid)

xgb_preds <- data.frame(predict(xgbModel, newdata=subTest[,-1]))

combMAE <- sort(colMeans(abs(xgb_preds - subTest$logerror)))
combMAE

###############################################################
# Full Model Building on Full Training Set
###############################################################
xgbGrid_final <- expand.grid(
  nrounds = 50, #set as per best tune
  max_depth = 15, #set as per best tine
  eta = .01, #set as per best tune
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = .7)

full_xgbModel <- train(
  logerror ~ ., 
  data=cleanTraining[,-1],
  trControl=trainControl(
      method="boot",
      number=20,
      savePredictions="final",
      summaryFunction=maeSummary,
      returnResamp='none', 
      returnData=FALSE, 
      verboseIter=TRUE, 
      allowParallel=TRUE),
  method='xgbTree', 
  tuneGrid=xgbGrid_final)

preds$xgb <- predict(full_xgbModel, newdata=subTest[,-1])
combMAE <- sort(colMeans(abs(preds - subTest$logerror)))
combMAE

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount
cleanProperties$livingareapropNF = cleanProperties$calculatedfinishedsquarefeet / cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt
## Julia's new features
cleanProperties$valueproductNF = cleanProperties$taxvaluedollarcnt * cleanProperties$taxamount
cleanProperties$agevalue = cleanProperties$valueproductNF * cleanProperties$age_of_home
cleanProperties$landratio = cleanProperties$structuretaxvaluedollarcnt / cleanProperties$landtaxvaluedollarcnt

taxgroup = cleanProperties %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanProperties = dplyr::left_join(cleanProperties, taxgroup, by='regionidzip')

cols_drop <- c("regionidzip", 'hottubflag', 'deckflag', 'heatflag','fireplaceflag')

cleanProperties <- cleanProperties[ , !(names(cleanProperties) %in% cols_drop)]

###############################################################
# Make Prediction for Submission 
###############################################################

makePrediction <- function(model, newdata, months, labels) {
  predictions <- cleanProperties[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cleanProperties$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = cleanProperties)
  }
  write.csv(x = predictions, file = "xgb_full.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(full_xgbModel, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))





