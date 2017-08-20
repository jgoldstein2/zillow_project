set.seed(0)
library(brnn)
library(caret)
library(caretEnsemble)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)
names(cleanTraining)

## Creating data partitions for training and testing
set.seed(0)
trainIndex <- createDataPartition(cleanTraining$month, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)

subTrain <- cleanTraining[trainIndex,-1]
subTest = cleanTraining[-trainIndex,-1]

## Creating a subset to make sure code works
subTrain_testing <- subTrain[1:1000, ]

## Converting to classes for the model
x=data.matrix(subTrain[,-1])
y=subTrain$logerror

## Setting function for MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

## Setting hyperparameters and running model
hypParams <- list(epochs = 1500, # c(700, 1000, 1300),
                  neurons = 50, #c(5, 10, 15),
                  change = 8,
                  tol=.000001,
                  mu = .0001)
                  #c(.001, .0005, .0001))
             
model_brnn <- brnn(x=x, 
                   y=y, 
                   params=hypParams,
                   metric = maeSummary,
                   verbose=T,
                   seed=0)

## Testing model
brnn_pred <- predict(model_brnn, newdata=data.matrix(subTest[,-1]))
View(cbind(brnn_pred, subTest$logerror))

## Writing csv
makePrediction <- function(model, newdata, months, labels) {
  predictions <- cleanProperties[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cleanProperties$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata)
  }
  write.csv(x = predictions, file = "brnnSubmission.csv",
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(model_brnn, newdata = data.matrix(cleanProperties[,-1]), months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))



