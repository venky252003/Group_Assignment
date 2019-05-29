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

# Fitting ANN to the Training set using Deep Learning H20
# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
feature_names<-names(training_set)[c(1,3:31)]
y="Attrition"
train_h2o<-as.h2o(training_set)

train_h2o[,y]<-as.factor(as.h2o(training_set[,y]))

model = h2o.deeplearning(x=feature_names,
                         y=y,
                         training_frame = train_h2o,
                         hidden = c(10,5),
                         epochs = 1000,
                         stopping_metric = "MSE",
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[,c(1,3:31)]))
prob_pred<-as.data.frame(y_pred)

#Evalualte Performance using Confusion Matrix on Training Set
confusionMatrix( as.factor(test_set$Attrition),as.factor(prob_pred$predict))

h2o.shutdown()

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   1   2
# 1 113  29
# 2  23 717
# 
# Accuracy : 0.941           
# 95% CI : (0.9234, 0.9557)
# No Information Rate : 0.8458          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.778           
# 
# Mcnemar's Test P-Value : 0.4881          
#                                           
#             Sensitivity : 0.8309          
#             Specificity : 0.9611          
#          Pos Pred Value : 0.7958          
#          Neg Pred Value : 0.9689          
#              Prevalence : 0.1542          
#          Detection Rate : 0.1281          
#    Detection Prevalence : 0.1610          
#       Balanced Accuracy : 0.8960          
#                                       

#       'Positive' Class : 1     

