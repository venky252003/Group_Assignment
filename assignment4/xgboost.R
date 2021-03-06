#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Importing the Train and Test HR Employee Attrition Data file:
training_set = read.csv("data/HR_train_data_cleaned.csv")
test_set = read.csv("data/HR_test_data_cleaned.csv")

training_set$X = NULL
test_set$X = NULL

set.seed(123)

library(lattice)
library(caret)
library(readr)
library(stringr)
library(car)
library(xgboost)

classifier<-xgboost(data=as.matrix(training_set[, c(1,3:31)]),
                    label =  as.integer(training_set$Attrition)-1,
                    max_depth=5,
                    objective="binary:logistic",
                    eval_metric="auc",
                    nrounds=10)

#Predicting on test set:
y_xgb<-predict(classifier,as.matrix(test_set[, c(1,3:31)]))

#Confusion Matrix:
predict<-ifelse(y_xgb>0.7,2,1)
test_set$predict = predict
confusionMatrix(as.factor(predict),as.factor(test_set$Attrition))



library(ineq)
gini_en=ineq(y_xgb,type="Gini")
plot(Lc(y_xgb))

