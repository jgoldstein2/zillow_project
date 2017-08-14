library(caret)
library(data.table)
library(dplyr)
library(DT)
library(randomForest)

load('cleanTraining_final.Rda')

cleanTraining = data.table(cleanTraining)
###############################################################
# Feature Engineering and Selection 
###############################################################

cleanTraining$valueratioNF = cleanTraining$taxvaluedollarcnt / cleanTraining$taxamount
cleanTraining$livingareapropNF = cleanTraining$calculatedfinishedsquarefeet / cleanTraining$lotsizesquarefeet 
cleanTraining$totalroomNF = cleanTraining$bathroomcnt + cleanTraining$bedroomcnt

taxgroup = cleanTraining %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanTraining = dplyr::left_join(cleanTraining, taxgroup, by='regionidzip')

cols_drop <- c("bathroomcnt","bedroomcnt", "regionidzip", "hottubflag", "taxvaluedollarcnt", "taxamount",
               "taxdelinquencyflag", "deckflag", "unitcnt")

cleanTraining <- cleanTraining[ , !(names(cleanTraining) %in% cols_drop)]

###############################################################
# Machine Learning Preparation 
###############################################################

# Partition the training and test data (75% train, 25% test) on month:
set.seed(0)
trainIndex <- createDataPartition(cleanTraining$month, 
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

#control = trainControl(method="repeatedcv", number=10, repeats=3, search="random")

#set.seed(0)
#mtry <- sqrt(ncol(subTrain))
#rf_random <- train(logerror~., data=subTrain, method="rf", metric=maeSummary, tuneLength=3, trControl=control)

set.seed(0)
bestmtry = tuneRF(subTrain[,-1], subTrain$logerror, stepFactor=1, improve=.005, ntreeTry = 500)
print(bestmtry)

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(mtry=bestmtry)
fitvec <- numeric(4)
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(0)
  fit <- train(logerror~., data=subTrain, method="rf", metric=maeSummary, tuneGrid = tunegrid, trControl=control, ntree=ntree)
  fitvec[ntree] = fit
}

plot(c(1000, 1500, 2000, 2500), fitvec, pch = 16, type = "b",
     xlab = "# of Trees",
     ylab = "Fit",
     main = "Fit vs # of Trees")

# CHANGE ACCORDING TO BEST NTREE
set.seed(0)
bestrf = randomForest(logerror ~. , data = subTrain, mtry = bestmtry, ntree = 500, importance=TRUE)

## check best parameters

## check variable importance
importance(bestrf)
varImpPlot(bestrf)

###############################################################
# Valid Model with SubTest
###############################################################

results <- data.frame(obs = subTest$logerror, 
                      rf.pred = predict(bestrf, subTest, type = "response"))
maeSummary(results)

cor(results)

###############################################################
# Refit Model with Full Training Set 
###############################################################

# CHANGE ACCORDING TO BEST NTREE
set.seed(0)
fullrf = randomForest(logerror ~. , data = fullTrain, mtry = bestmtry, ntree = 500, importance=TRUE)

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

load('./cleanProperties_final.Rda')

###This must be updated if any new features are added or dropped!

cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount
cleanProperties$livingareapropNF = cleanProperties$calculatedfinishedsquarefeet / cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt

taxgroup = cleanProperties %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanProperties = dplyr::left_join(cleanProperties, taxgroup, by='regionidzip')

cols_drop <- c("bathroomcnt","bedroomcnt", "regionidzip", "hottubflag", "taxvaluedollarcnt", "taxamount",
               "taxdelinquencyflag", "deckflag", "unitcnt")

cleanProperties <- cleanProperties[ , !(names(cleanProperties) %in% cols_drop)] 

###############################################################
# Make Prediction for Submission 
###############################################################

makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  write.csv(x = predictions, file = "submission2.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(fullrf, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))
