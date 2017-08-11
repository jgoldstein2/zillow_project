getwd()
load('cleanTraining_master (3).Rda')

blank_count = as.data.frame(sapply(cleanTraining, function(x) (sum(x == "")/length(x)) * 100))
# subsetting the train data to test run our model on
# train_data2 <- cleanTraining[1:1000,]

# train_data3 <- cleanTraining[sample(1:nrow(cleanTraining), size =1000, replace = F), ]

# drop transaction date:

cleanTraining$transactiondate <-  NULL

# partition the training and test data (75% train, 25% test) on month:
set.seed(0)
trainIndex <- createDataPartition(cleanTraining$month, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
# training set
subTrain <- cleanTraining[ trainIndex,-1]


## testing set
subTest  <- cleanTraining[-trainIndex,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

# cross validation
## 1. random hyperparameter
rdmSearch <- trainControl(method = "cv",
                          number = 3,
                          summaryFunction = maeSummary,
                          search = "random")
set.seed(0)
gbmFit1 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 tuneLength = 3,
                 trControl = rdmSearch,
                 verbose = TRUE)
plot(gbmFit1)
gbmFit1$bestTune

## 2. grid search
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary)

gbmGrid <-  expand.grid(interaction.depth = c(3,5,7), 
                        n.trees = c(100,200), 
                        shrinkage = c(.1, .01),
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

## cross validation summary
gbmFit2

## visualize parameters
plot(gbmFit2)
#   n.trees interaction.depth shrinkage n.minobsinnode
# 1     100                 3      0.01             10

## variable importance
gbmImp <- varImp(gbmFit2, scale = FALSE)
plot(gbmImp, top = 20)

# Validate model using subTest
results <- data.frame(obs = subTest$logerror, 
                      pred = predict(gbmFit2, newdata = subTest))
maeSummary(results)
#        MAE 

cor(results)


## both MAE and correlation indicate that our prediction is not quite good
## below are the possible reasons and some suggestion for improvement:
##  1. Misinterpreted some features -> need more feature engineering, be creative
##  2. Poor missing data imputation -> need a better way for imputation, e.g., use latitude and longitude to impute censustract etc.
##  3. Accidentally dropped crucial features -> try to do imputation/feature engineering before dopping those features
##  4. Irreduciable error -> if this is the case then there's nothing we can do


# 5. Fitting all training set with Best parameters
## assume gbmFit2 gives you the best parameter
gbmFit2$bestTune

fitBestModel <- trainControl(method = "none",
                             summaryFunction = maeSummary)

set.seed(0)
gbmFit3 <- train(logerror ~ .,
                 data = train_data3[,-1], 
                 method = "gbm", 
                 preProcess = c("center", "scale"),
                 metric = "MAE",
                 maximize = FALSE,
                 trControl = fitBestModel,
                 tuneGrid = gbmFit2$bestTune,
                 verbose = TRUE)

# calculating the prediction of the training data (full 1000 rows):
train_pred <- predict(gbmFit3, newdata = train_data3)

# calculating the prediction of the test data (250 rows):
test_pred <- predict(gbmFit3, newdata = subTest)

# MAE:
sum(abs(train_data3$logerror - train_pred)) / nrow(train_data3)
# 0.06663952

# MAE:
sum(abs(subTest$logerror - test_pred)) / nrow(subTest)
# 0.07816333


#### Did not complete this part!!! ####
# 6. Making prediction for submission
test_data <- properties %>% 
  select(intersect(names(properties), names(train_data))) %>%
  mutate(airconditioningtypeid = as.factor(ifelse(is.na(airconditioningtypeid), 
                                                  "5", airconditioningtypeid)),
         heatingorsystemtypeid = as.factor(ifelse(is.na(heatingorsystemtypeid), 
                                                  "13", heatingorsystemtypeid)),
         buildingqualitytypeid = ifelse(is.na(buildingqualitytypeid), 7, buildingqualitytypeid),
         unitcnt = ifelse(is.na(unitcnt), 1, unitcnt),
         fullbathcnt = ifelse(is.na(fullbathcnt), 2, fullbathcnt),
         calculatedbathnbr = ifelse(is.na(calculatedbathnbr), 2, calculatedbathnbr),
         yearbuilt = ifelse(is.na(yearbuilt), 1955, yearbuilt))

## impute numerical columns using 0
test_data[is.na(test_data)] <- 0

## Note: the factor levels might not match if you test on the entire dataset. 
## Then you should convert those values that were not in the training set


makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  write.csv(x = predictions, file = "submission.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(gbmFit3, newdata = test_data, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))