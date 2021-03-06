library(caret)
library(data.table)
library(dplyr)
library(DT)

# load('cleanTraining_final.Rda')

cleanTraining = data.table(cleanTraining)
###############################################################
# Feature Engineering and Selection 
###############################################################

cleanTraining$valueratioNF = cleanTraining$taxvaluedollarcnt / cleanTraining$taxamount
cleanTraining$assessValueMetricNF = cleanTraining$structuretaxvaluedollarcnt / cleanTraining$landtaxvaluedollarcnt
cleanTraining$livingareapropNF = cleanTraining$calculatedfinishedsquarefeet / cleanTraining$lotsizesquarefeet 
cleanTraining$totalroomNF = cleanTraining$bathroomcnt + cleanTraining$bedroomcnt

cols_drop <- c("regionidzip", "taxvaluedollarcnt", "taxamount", "taxdelinquencyflag", 
               "deckflag", "unitcnt", "month", "property_group", "regionidcounty")

cleanTraining <- cleanTraining[ , !(names(cleanTraining) %in% cols_drop)]

###############################################################
# Machine Learning Preparation 
###############################################################

# Partition the training and test data (75% train, 25% test) on month:
set.seed(0)
trainIndex <- createDataPartition(cleanTraining$totalroomNF, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)

# training set
subTrain <- cleanTraining[ trainIndex,-1]

## testing set
subTest  <- cleanTraining[-trainIndex,-1]

# full training set
fullTrain = cleanTraining[,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

###############################################################
# Cross Validation 
###############################################################
gridSearch <- trainControl(method = "cv",
                           number = 5,
                           summaryFunction = maeSummary,
                           verboseIter = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(20, 24, 28), 
                        n.trees = c(2000, 3500, 5000), 
                        shrinkage = c(.0001),
                        n.minobsinnode = 10)

set.seed(0)
gbmFit2 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 tuneGrid = gbmGrid,
                 trControl = gridSearch,
                 verbose = TRUE)

## visualize parameters
plot(gbmFit2)

## best parameters
gbmFit2$bestTune

## variable importance
gbmImp <- varImp(gbmFit2, scale = FALSE)
plot(gbmImp, top = 12)

###############################################################
# Valid Model with SubTest
###############################################################

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(gbmFit2, newdata = subTest))
maeSummary(results)

cor(results)

###############################################################
# Refit Model with Full Training Set 
###############################################################

# Get best parameters for model on full training set
gbmFit2$bestTune

fitBestModel <- trainControl(method = "none",
                             summaryFunction = maeSummary)

gbmFit3 <- train(logerror ~ .,
                 data = fullTrain, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 trControl = fitBestModel,
                 tuneGrid = gbmFit2$bestTune,
                 verbose = TRUE)

#Make sure new data doesn't have parcelid
predict(gbmFit3, newdata = fullTrain)

## visualize parameters
plot(gbmFit3)

## best parameters
gbmFit3$bestTune

## variable importance
gbmImp2 <- varImp(gbmFit3, scale = FALSE)
plot(gbmImp2, top = 8)

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

# load('cleanProperties_final.Rda')

###This must be updated if any new features are added or dropped!

cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount
cleanProperties$assessValueMetricNF = cleanProperties$structuretaxvaluedollarcnt / cleanProperties$landtaxvaluedollarcnt
cleanProperties$livingareapropNF = cleanProperties$calculatedfinishedsquarefeet / cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt

cols_drop <- c("regionidzip", "taxvaluedollarcnt", "taxamount", "taxdelinquencyflag", 
               "deckflag", "unitcnt", "month", "property_group", "regionidcounty")

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
  write.csv(x = predictions, file = "submission.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(gbmFit3, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))

