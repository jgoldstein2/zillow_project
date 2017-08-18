library(caret)
library(data.table)
library(dplyr)
library(DT)
library(randomForest)

load('cleanTraining_final_yuhan.Rda')

cleanTraining = data.table(cleanTraining)
###############################################################
# Feature Engineering and Selection 
###############################################################

cleanTraining$valueratioNF = cleanTraining$taxvaluedollarcnt / cleanTraining$taxamount
cleanTraining$livingareapropNF = cleanTraining$calculatedfinishedsquarefeet / cleanTraining$lotsizesquarefeet 
cleanTraining$totalroomNF = cleanTraining$bathroomcnt + cleanTraining$bedroomcnt
taxgroup = cleanTraining %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanTraining = dplyr::left_join(cleanTraining, taxgroup, by='regionidzip')

cols_keep <- c("parcelid", "logerror", "structuretaxvaluedollarcnt","landtaxvaluedollarcnt", "calculatedfinishedsquarefeet", 
               "latitude", "longitude", "age_of_home", "lotsizesquarefeet", 
               "valueratioNF", "livingareapropNF", "totalroomNF", "avgtaxamtNF")

cleanTraining <- cleanTraining[ , (names(cleanTraining) %in% cols_keep)]


###############################################################
# Machine Learning Preparation 
###############################################################

# Partition the training and test data (75% train, 25% test) on month:
set.seed(0)
trainIndex <- sample(1:nrow(cleanTraining), nrow(cleanTraining)*0.75)

# training set
subTrain <- cleanTraining[ trainIndex,-1]

## testing set
subTest  <- cleanTraining[-trainIndex,-1]

# full training set
fullTrain = cleanTraining[,-1]

###############################################################
# Cross Validation 
###############################################################
set.seed(0)
oob.err = numeric(6)
for(mtry in 1:6){
  fit = randomForest(logerror ~., data = subTrain, mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

## Visualize the OOB error rates as they change with the number of variables
plot(1:6, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")
print(oob.err) #get the best mtry


## Change according to best mtry (caret)
set.seed(0)
bestrf = randomForest(logerror ~. , 
                      data = subTrain, 
                      mtry = 100000000000000, #change mtry mannually
                      ntree = 100,
                      importance=TRUE,
                      do.trace = TRUE)

## Check variable importance
importance(bestrf)
varImpPlot(bestrf)

###############################################################
# Valid Model with SubTest
###############################################################

rf.pred = predict(bestrf, subTest, type = "response")

sum(abs(subTest$logerror - rf.pred)) / nrow(subTest)

cor(rf.pred)

###############################################################
# Refit Model with Full Training Set 
###############################################################

# CHANGE ACCORDING TO BEST NTREE
set.seed(0)
fullrf = randomForest(logerror ~. , 
                      data = fullTrain, 
                      mtry = 100000000000000000,#change mtry mannually
                      ntree = 100, 
                      importance=TRUE)

###############################################################
# Load Properties File and Add/Drop Features Used in Train Set
###############################################################

load('cleanProperties_final_yuhan.Rda')

cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount
cleanProperties$livingareapropNF = cleanProperties$calculatedfinishedsquarefeet / cleanProperties$lotsizesquarefeet 
cleanProperties$totalroomNF = cleanProperties$bathroomcnt + cleanProperties$bedroomcnt

taxgroup = cleanProperties %>% dplyr::group_by(., regionidzip) %>% dplyr::summarise(., avgtaxamtNF = mean(taxamount))
cleanProperties = dplyr::left_join(cleanProperties, taxgroup, by='regionidzip')

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
  write.csv(x = predictions, file = "submission_rf.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(fullrf, newdata = cleanProperties, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))