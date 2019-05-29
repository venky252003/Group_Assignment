#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Importing the Train and Test HR Employee Attrition Data file:
training_set = read.csv("data/HR_train_data_cleaned.csv")
test_set = read.csv("data/HR_test_data_cleaned.csv")

training_set$X = NULL
test_set$X = NULL

set.seed(123)
#Decision Tree
library(rpart)
library(rpart.plot)
library(caret)

#Create control variable
r.ctrl = rpart.control(minsplit = 50, minbucket = 5, cp=0, xval = 10)

#Cluster using Recursive Partitioning and Regression Tree
DTmodel1 = rpart(formula = Attrition ~ ., data = training_set, method = "class", 
                 control = r.ctrl)
DTmodel1
prp(DTmodel1)

#tree chart
library(rattle)
library(RColorBrewer)

#Create Tree
fancyRpartPlot(DTmodel1)

#find tree perform
printcp(DTmodel1)
plotcp(DTmodel1)

#Pruning the tree at cost price = 0.0060241
ptree = prune(DTmodel1, cp=0.0060241, "CP")
printcp(ptree)
fancyRpartPlot(ptree)

# Baseline Accuracy
table(test_set$Attrition)
print(740/nrow(test_set))
#0.8390023


#Predication on Test dataset
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test_set, type = 'class')
  table_mat <- table(test_set$Attrition, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#Predication on Train dataset
accuracy_tune_train <- function(fit) {
  predict_unseen <- predict(fit, training_set, type = 'class')
  table_mat <- table(training_set$Attrition, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#CART model accuracy
#Check Accuracy of Test Dataset
accuracy_tune(DTmodel1) #0.845805
#Check Accuracy of Train Dataset
accuracy_tune_train(DTmodel1) #0.8901846

######### Change Hyper parameter ##########################
#Create control variable
r.ctrl = rpart.control(minsplit = 10, minbucket = round(10/3), cp=0.0060241, xval = 5, maxdepth = 10)

#Cluster using Recursive Partitioning and Regression Tree
DTModel2 = rpart(formula = Attrition ~ ., data = training_set, method = "class", control = r.ctrl)
#Check Accuracy of Test Dataset
accuracy_tune(DTModel2) #0.88322
#Check Accuracy of Train Dataset
accuracy_tune_train(DTModel2) #0.9285714

## Scoring Test sample
test_set$predict.class <- predict(DTModel2, test_set, type="class")
test_set$predict.score <- predict(DTModel2, test_set)

#Evalualte Performance using Confusion Matrix
conf = confusionMatrix( as.factor(test_set$predict.class),as.factor(test_set$Attrition))
print(conf)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   1   2
# 1  65  26
# 2  77 714
# 
# Accuracy : 0.8832          
# 95% CI : (0.8602, 0.9037)
# No Information Rate : 0.839           
# P-Value [Acc > NIR] : 0.0001243       
# 
# Kappa : 0.4944          
# 
# Mcnemar's Test P-Value : 8.365e-07       
#                                           
#             Sensitivity : 0.4577          
#             Specificity : 0.9649          
#          Pos Pred Value : 0.7143          
#          Neg Pred Value : 0.9027          
#              Prevalence : 0.1610          
#          Detection Rate : 0.0737          
#    Detection Prevalence : 0.1032          
#       Balanced Accuracy : 0.7113          
#                                           
#        'Positive' Class : 1  
       
print(conf$byClass)
# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision 
# 0.45774648           0.96486486           0.71428571           0.90265487           0.71428571 
# Recall                   F1           Prevalence       Detection Rate Detection Prevalence 
# 0.45774648           0.55793991           0.16099773           0.07369615           0.10317460 
# Balanced Accuracy 
# 0.71130567 

summary(test_set)
head(test_set$predict.score[,2])

#ROC Curve
library(ROCR)
pred = ROCR::prediction(test_set$predict.score[,2], test_set$Attrition)
perf = ROCR::performance(pred, "tpr", "fpr")
plot(perf)
perf
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- ROCR::performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc
#0.7865483


