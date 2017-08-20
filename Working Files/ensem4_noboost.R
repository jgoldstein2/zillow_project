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
### subTrain1 <- subTrain1[1:5000, ]

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

set.seed(0)
model_list <- caretList(
  logerror ~ ., 
  data=subTrain,
  trControl=myControl,
  tuneList=list(  
        model_rf <- caretModelSpec(method='rf', tuneGrid=expand.grid(mtry=c(1:3))),
        model_brnn <- caretModelSpec(method='brnn', preProcess=PP),
        model_enet <- caretModelSpec(method='enet', preProcess=PP),
        model_superpc <- caretModelSpec(method='superpc', preProcess=PP)
        )
)

xyplot(resamples(model_list))
modelCor(model_list)

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
      number=5,
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
                                                         number=20,
                                                         savePredictions="final",
                                                         summaryFunction=maeSummary,
                                                         savePredictions="final", 
                                                         verboseIter=TRUE, 
                                                         allowParallel=TRUE
                                                         )
)
# trainControl(method='cv'))

summary(stackedModel_linear$ens_model$finalModel)
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

preds <- data.frame(sapply(model_list, predict, newdata=subTest[,-1]))
preds$ensemble <- predict(ensemModel, newdata=subTest[,-1])
preds$stacked_linear <- predict(stackedModel, newdata=subTest[,-1])
# preds$stacked_boost <- predict(stackedModel, newdata=subTest[,-1])

#RSME...I think
combRSME <- sort(sqrt(colMeans((preds - subTest$logerror) ^ 2)))
combRSME

# MAE
combMAE <- sort(colMeans(abs(preds - subTest$logerror)))
combMAE
###############################################################
# Full Model Building on Full Training Set
###############################################################
set.seed(0)
full_model_list <- caretList(
  logerror ~ ., 
  data=cleanTraining[,-1],
  trControl=myControl,
  tuneList=list(  
    model_rf <- caretModelSpec(method='rf', tuneGrid=expand.grid(mtry=1)), #set mtry based on CV best tune
    model_brnn <- caretModelSpec(method='brnn', preProcess=PP),
    model3_enet <- caretModelSpec(method='enet', preProcess=PP),
    model4_superpc <- caretModelSpec(method='superpc', preProcess=PP)
    )
)

###############################################################
# Ensembling full model
###############################################################
set.seed(0)
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
            caretStack(model_list,
                       method="BstLm",
                       metric="MAE",
                       trControl=trainControl(method="boot",
                                              number=50,
                                              savePredictions="final",
                                              summaryFunction=maeSummary,
                                              returnResamp='none', 
                                              returnData=FALSE, 
                                              verboseIter=TRUE, 
                                              allowParallel=TRUE)
                       )
# trainControl(method='cv'))

summary(stackedModel_linear$ens_model$finalModel)
linear$error

## I tried a more sophisticated stacking method since we have a fairly large data set 
## and the models had similar accuracy.  I was not able to run a correlation  (modelCor())
## on the models but would have if the function worked.  You are supposed to drop highly
## correlated models.
# full_stackedModel_boost <- 
#             caretStack(model_list,
#                        method="gbm",
#                        verbose=TRUE,
#                        tuneLength=10,
#                        metric="MAE",
#                        trControl=trainControl(method="boot",
#                                               number=20,
#                                               savePredictions="final",
#                                               summaryFunction=maeSummary,
#                                               returnResamp='none', 
#                                               returnData=FALSE, 
#                                               verboseIter=TRUE, 
#                                               allowParallel=TRUE
#                                                )
#             )

summary(full_stackedModel_linear$ens_model$finalModel)

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
  write.csv(x = predictions, file = "ensem4_train.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(ensemModel, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))
