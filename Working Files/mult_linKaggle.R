library(car)
library(MASS)
library(glmnet)

# loading in cleanTraining and cleanProperties
# setwd("/Users/mikeghoul/Desktop/Data Science Bootcamp/Projects/Kaggle_Zillow/Rda folders")
load('cleanTraining_final.Rda')
load('cleanProperties_final.Rda')

# cleanTraining:
cleanTraining$valueratioNF = cleanTraining$taxvaluedollarcnt / cleanTraining$taxamount

# cleanProperties:
cleanProperties$valueratioNF = cleanProperties$taxvaluedollarcnt / cleanProperties$taxamount


# subsetting data into test and training:
train = sample(1:nrow(cleanTraining), 7.5*nrow(cleanTraining)/10)
test = (-train)

# check splits:
length(train)/nrow(cleanTraining)
# 75%


# Running a model on training data
set.seed(0)
multi_lin_model = lm(logerror ~ +age_of_home +calculatedfinishedsquarefeet +valueratioNF
                     +structuretaxvaluedollarcnt, cleanTraining[train,])
summary(multi_lin_model)

plot(multi_lin_model)

influencePlot(multi_lin_model)
# observation 68958 is an issue as the CookD is 13

vif(multi_lin_model)
# no violations of multicollinearity


# stepwise AIC/BIC
model.empty = lm(logerror ~ 1, data = cleanTraining[train,]) #The model with an intercept ONLY.
model.full = lm(logerror ~ +age_of_home +calculatedfinishedsquarefeet +valueratioNF
                +structuretaxvaluedollarcnt, data = cleanTraining[train,]) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))


forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2) 
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)


forwardBIC = step(model.empty, scope, direction = "forward", k = log(67680))
backwardBIC = step(model.full, scope, direction = "backward", k = log(67680))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(67680))
bothBIC.full = step(model.full, scope, direction = "both", k = log(67680))

summary(forwardAIC)

# looking at reduced model from BIC (only uses calculatedfinishedsquarefeet)
summary(forwardBIC)
plot(forwardBIC)
influencePlot(forwardBIC)
# CookD no longer violated

avPlots(forwardBIC)
confint(forwardBIC)


grid = 10^seq(5, -2, length = 100)
x = model.matrix(logerror ~ +age_of_home +calculatedfinishedsquarefeet +valueratioNF
                 +structuretaxvaluedollarcnt, cleanTraining)[, -1] #Dropping the intercept column.
y = cleanTraining$logerror 
y.test = y[test]
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge_coef <- coef(ridge.models.train)

plot(ridge.models.train, xvar = "lambda", label = TRUE, main = "Ridge Regression")

set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")

ridge.bestlambdatest = predict(ridge.models.train, s = cv.ridge.out$lambda.min, newx = x[test, ])
mean((ridge.bestlambdatest - y.test)^2)
# 0.02631613

ridge.bestlambdafull = glmnet(x, y, alpha = 0, lambda = cv.ridge.out$lambda.min)
coef(ridge.bestlambdafull)
# (Intercept)                   6.673332e-03
# age_of_home                  -3.861356e-05
# calculatedfinishedsquarefeet  2.736457e-06
# valueratioNF                  1.616925e-05
# structuretaxvaluedollarcnt    3.152325e-09


# Lasso Regression:
grid2 = 10^seq(4, -4, length = 100)
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid2)
lasso_coef <- coef(lasso.models.train)

plot(lasso.models.train, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")

lasso.bestlambdatest = predict(lasso.models.train, s = cv.lasso.out$lambda.min, newx = x[test, ])
mean((lasso.bestlambdatest - y.test)^2)
# 0.02632778

lasso.bestlambdafull = glmnet(x, y, alpha = 1, lambda = cv.lasso.out$lambda.min)
coef(lasso.bestlambdafull)
# all coefficients are 0!

lasso.bestlambdafull = predict(lasso.bestlambdafull, s = cv.lasso.out$lambda.min, newx = x)
mean((lasso.bestlambdafull - y)^2)
# 0.02588188