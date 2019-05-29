#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Ref link : https://machinelearningmastery.com/machine-learning-ensembles-with-r/

#Importing the Train and Test HR Employee Attrition Data file:
training_set = read.csv("data/HR_train_data_cleaned.csv")
test_set = read.csv("data/HR_test_data_cleaned.csv")

training_set$X = NULL
test_set$X = NULL

set.seed(123)

# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

#1. Boosting Algorithms
# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# C5.0

fit.c50 <- train(Attrition~., data=training_set, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
fit.gbm <- train(Attrition~., data=training_set, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

#2. Bagging Algorithms

# Example of Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# Bagged CART
fit.treebag <- train(Attrition~., data=training_set, method="treebag", metric=metric, trControl=control)
# Random Forest
fit.rf <- train(Attrition~., data=training_set, method="rf", metric=metric, trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

#3. Stacking Algorithms
# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
models <- caretList(Attrition~., data=training_set, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# correlation between results
modelCor(results)
splom(results)

# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)