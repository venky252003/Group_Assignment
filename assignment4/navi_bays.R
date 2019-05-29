#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Importing the Train and Test HR Employee Attrition Data file:
training_set = read.csv("data/HR_train_data_cleaned.csv")
test_set = read.csv("data/HR_test_data_cleaned.csv")

training_set$X = NULL
test_set$X = NULL

library(caret)
library(devtools)
#library(GGally)

#ggpairs(training_set, title = "Scatterplot Matrix of the Features of the HR Attrition Data Set")

set.seed(123)

#Scaling of Variables
str(training_set)
training_set[, c(1,3:31)] = scale(training_set[,c(1,3:31)])
test_set[, c(1,3:31)] = scale(test_set[,c(1,3:31)])
head(test_set)
str(training_set)

library(e1071)
classifier = naiveBayes(x = training_set[-2],
                        y = training_set$Attrition)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-2])

#Evalualte Performance using Confusion Matrix on Training Set
confusionMatrix( as.factor(test_set[, 2]),as.factor(y_pred))
