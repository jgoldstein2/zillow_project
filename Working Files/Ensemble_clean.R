###############################################################
# Libraries
###############################################################

library(plyr)
library(splines)
library(parallel)
library(gbm)
library(mlbench)
library(rpart)
library(leaps)
library(brnn) # bayesian regression
library(elasticnet) #enet, ridge regression
library(frbs) #FIR.DM, Fuzzy Inference Rules by Descent Method 
library(bartMachine) #bayesian additive regression trees
library(superpc) # Supervised principal component analysis
library(nnet) # model averaged neural net, bagging concept
library(caretEnsemble)
library(caret)
library(xgboost)
library(dplyr)
library(lattice)
library(bst)

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
### subTrain1 <- subTrain1[1:5000, ]

subTest = cleanTraining[-trainIndex,-1]
### subset for testing code
### subTest <- subTest[1:2000, ]
View(subTrain)

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

### Some models to try
###### rvmPoly, install.packages('kernlab'), regression using RVM (Relavance Vector Machines) with a polynomial kernel
###### pythonKnnReg, install.packages('rPython'), regression using kNN 
###### glmboost, install.packages('mboost'), regression/classification performing linear regression with boosting concept
###### bayesglm, install.packages('arm'), regression/classification using linear regression with Bayesian concept

set.seed(0)
model_list <- caretList(
  logerror ~ ., 
  data=subTrain,
  trControl=myControl,
  tuneList=list(  
      model1 <- caretModelSpec(method='gbm', tuneGrid=expand.grid(n.trees = c(200, 600, 1000), 
                                                                  interaction.depth = c(12, 15, 18), 
                                                                  shrinkage = c(.01, .001), 
                                                                  n.minobsinnode = 10)),
      model2 <- caretModelSpec(method='leapSeq', preProcess=PP),
      model3 <- caretModelSpec(method='brnn', preProcess=PP),
      model4 <- caretModelSpec(method='enet', preProcess=PP),
      model5 <- caretModelSpec(method='avNNet', preProcess=PP),
      # model6 <- caretModelSpec(method='FIR.DM', preProcess=PP),
      model7 <- caretModelSpec(method='superpc', preProcess=PP),
      model8 <- caretModelSpec(method='nnet', preProcess=PP)
      )
)

xyplot(resamples(model_list))
modelCor(model_list)

#########   Not Needed   #####################################
## Looking at results
# model5$results$RMSE

# all.models <- list(model1, model2, model3, model4, model5, model7, model8)
# names(test.models) <- sapply(test.models, function(x) x$method)
# names(all.models) <- sapply(all.models, function(x) x$method)
# sort(sapply(test.models, function(x) min(x$results$RMSE)))
# sort(sapply(all.models, function(x) min(x$results$RMSE)))

###############################################################
# Ensembling
###############################################################

set.seed(0)
ensemModel <-    
  caretEnsemble(
    model_list,
    # tuneList=list(model1,...,modeln), # you may be able to use this parameter if you want to select models to ensemble
    metric="MAE",
    trControl=trainControl(
      summaryFunction=maeSummary,
      savePredictions="final", 
      verboseIter=TRUE, 
      allowParallel=TRUE
    )
  )
    
sort(ensemModel$weights, decreasing=TRUE)
ensemModel$error

###############################################################
# Stacking
###############################################################
set.seed(0)
stackedModel_linear <- caretStack(model_list,
                                  method="BstLm",
                                  metric="MAE",
                                  trControl=trainControl(method="boot",
                                                         number=50,
                                                         savePredictions="final", 
                                                         verboseIter=TRUE,
                                                         summaryFunction=maeSummary,
                                                         allowParallel = TRUE
                                                         )
                                  )
 # trainControl(method='cv'))

summary(stackedModel$ens_model$finalModel)
linear$error

## I tried a more sophisticated stacking method since we have a fairly large data set 
## and the models had similar accuracy.  I was not able to run a correlation  (modelCor())
## on the models but would have if the function worked.  You are supposed to drop highly
## correlated models.  It did not perform better on the test set, so I chose to go with linear.

# set.seed(0)
# stackedModel_boost <- 
#   caretStack(model_list,
#              method="gbm",
#              verbose=TRUE,
#              tuneLength=10,
#              metric="MAE",
#              trControl=trainControl(
#                 method="boot",
#                 number=20,
#                 savePredictions="final",
#                 summaryFunction=maeSummary
#                 )
#   )

###############################################################
# Predicting and Testing - check subTest before re-train
###############################################################

names(subTest)
# preds <- data.frame(sapply(modelList, predict, newdata=subTest[,-1])) # I don't think this line is needed
preds <- data.frame(sapply(model_list, predict, newdata=subTest))
preds$ensemble <- predict(ensemModel, newdata=subTest[,-1])
preds$stacked_linear <- predict(stackedModel_linear, newdata=subTest[,-1])
preds$stacked_boost <- predict(stackedModel_boost, newdata=subTest[,-1])

colnames(preds)
#RSME...I think
combRSME <- sort(sqrt(colMeans((preds - subTest$logerror) ^ 2)))
combRSME
# MAE
combMAE <- sort(colMeans(abs(preds - subTest$logerror)))
combMAE
###############################################################
# Full Model Building on Full Training Set
###############################################################

full_model_list <- caretList(
  logerror ~ ., 
  data=cleanTraining[,-1],
  trControl=myControl,
  tuneList=list(  
    model1 <- caretModelSpec(method='gbm', tuneGrid=expand.grid(n.trees=600, 
                                                                interaction.depth=18, 
                                                                shrinkage = .01, 
                                                                n.minobsinnode=10)),
    model2 <- caretModelSpec(method='leapSeq', preProcess=PP),
    model3 <- caretModelSpec(method='brnn', preProcess=PP),
    model4 <- caretModelSpec(method='enet', preProcess=PP),
    model5 <- caretModelSpec(method='avNNet', preProcess=PP),
    # model6 <- caretModelSpec(method='FIR.DM', preProcess=PP),
    model7 <- caretModelSpec(method='superpc', preProcess=PP),
    model8 <- caretModelSpec(method='nnet', preProcess=PP)
  )
)

###############################################################
# Ensembling full model
###############################################################

full_ensemModel <-    
  caretEnsemble(
    full_model_list,
    metric="MAE",
    trControl=trainControl(
      number=5,
      summaryFunction=maeSummary
    )
  )

sort(full_ensemModel$weights, decreasing=TRUE)
full_ensemModel$models

###############################################################
# Stacking full model
###############################################################

set.seed(0)
full_stackedModel_linear <- 
      caretStack(full_model_list,
                 method="BstLm",
                 metric="MAE",
                 trControl=trainControl(method="boot",
                                        number=20,
                                        savePredictions="final",
                                        summaryFunction=maeSummary,
                                        returnResamp='none', 
                                        returnData=FALSE, 
                                        verboseIter=TRUE, 
                                        allowParallel=TRUE
                                        )
                 )
# trainControl(method='cv'))

summary(full_stackedModel_linear$ens_model$finalModel)
linear$errorsummary(full_stackedModel_linear$ens_model$finalModel)
linear$error

###############################################################
# Predicting and Testing - check subTest before re-train
###############################################################

# preds <- data.frame(sapply(modelList, predict, newdata=subTest[,-1])) # I don't think this line is needed
preds_final <- data.frame(sapply(full_model_list, predict, newdata=subTest))
preds_final$full_ensemble <- predict(full_ensemModel, newdata=subTest[,-1])
preds_final$full_stacked_linear <- predict(full_stackedModel_linear, newdata=subTest[,-1])
# preds_final$stacked_boost <- predict(full_stackedModel_boost, newdata=subTest[,-1])

# RSME...I think
combRSME <- sort(sqrt(colMeans((preds_final - subTest$logerror) ^ 2)))
combRSME
# MAE
combMAE <- sort(colMeans(abs(preds_final - subTest$logerror)))
combMAE

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

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
  predictions <- cleanProperties[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    cleanProperties$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = cleanProperties)
  }
  write.csv(x = predictions, file = "ensem7_full.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(full_ensemModel, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))




