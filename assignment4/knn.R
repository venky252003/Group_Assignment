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

# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -2],
             test = test_set[, -2],
             cl = training_set[, 2],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
#cm = table(test_set[, 2], y_pred)

#Evalualte Performance using Confusion Matrix on Training Set
confusionMatrix( as.factor(test_set[, 2]),as.factor(y_pred))

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 4]) - 1, max(set[, 4]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'DailyRate')
y_grid = knn(train = training_set[, -2], test = grid_set, cl = training_set[, 2], k = 5)
plot(set[, -2],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'DailyRate',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 2] == 1, 'green4', 'red3'))
