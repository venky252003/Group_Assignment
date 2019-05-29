#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Importing the Train and Test HR Employee Attrition Data file:
training_set = read.csv("data/HR_train_data_cleaned.csv")
test_set = read.csv("data/HR_test_data_cleaned.csv")

training_set$X = NULL
test_set$X = NULL

set.seed(123)
library(caret)
library(randomForest)
rf = randomForest(as.factor(Attrition) ~ ., data=training_set, ntree=501, 
                  mtry=5, nodesize=10, importance=TRUE, replace=TRUE)

print(rf)
summary(rf)

plot(rf, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest HR Employee Attrition")

rf$err.rate
impVar <- round(randomForest::importance(rf), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

# To check important variables
importance(rf)        
varImpPlot(rf)

## Scoring syntax on Train Dataset
training_set$predict.class <- predict(rf, training_set, type="class")
training_set$predict.score <- predict(rf, training_set, type="prob")
head(training_set)
class(training_set$predict.score)

#Evalualte Performance using Confusion Matrix on Training Set
confusionMatrix( as.factor(training_set$predict.class),as.factor(training_set$Attrition))

test_set$predict.class <- predict(rf, test_set, type="class")
#Evalualte Performance using Confusion Matrix
confusionMatrix( as.factor(test_set$predict.class),as.factor(test_set$Attrition))


# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  rfModel <- randomForest(as.factor(Attrition) ~ ., data = training_set[,c(1,2:31)], ntree = 500, mtry = i, 
                          importance = TRUE)
  predValid <- predict(rfModel, test_set, type = "class")
  a[i-2] = mean(predValid == test_set$Attrition)
}

print(a)
plot(3:8,a)

## Tuning Random Forest
str(training_set[,c(1,3:31)])
tRF <- tuneRF(x = training_set[,c(1,3:31)], 
              y=as.factor(training_set$Attrition),
              mtryStart = 3, 
              ntreeTry=101, 
              stepFactor = 0.5, 
              improve = 1e-5, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
)

tRF
# mtry = 3  OOB error = 9.67% 
# Searching left ...
# mtry = 6 	OOB error = 7.82% 
# 0.1909548 0.001 
# mtry = 12 	OOB error = 6.8% 
# 0.1304348 0.001 
# mtry = 24 	OOB error = 7.14% 
# -0.05 0.001 
# Searching right ...
# mtry = 1 	OOB error = 14.67% 
# -1.157143 0.001 

#mtry = 12 has least OOB error
tRF$importance

str(test_set[,c(1,3:31)])

test_set$predict.class <- predict(tRF, test_set[,c(1,3:31)], type="class")

#Evalualte Performance using Confusion Matrix on Test set
conf = confusionMatrix( as.factor(test_set$predict.class),as.factor(test_set$Attrition))
print(conf)
print(conf$byClass)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   1   2
# 1  92   4
# 2  50 736
# 
# Accuracy : 0.9388          
# 95% CI : (0.9209, 0.9537)
# No Information Rate : 0.839           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7392          
# 
# Mcnemar's Test P-Value : 9.141e-10       
#                                           
#             Sensitivity : 0.6479          
#             Specificity : 0.9946          
#          Pos Pred Value : 0.9583          
#          Neg Pred Value : 0.9364          
#              Prevalence : 0.1610          
#          Detection Rate : 0.1043          
#    Detection Prevalence : 0.1088          
#       Balanced Accuracy : 0.8212          
#                                           
#        'Positive' Class : 1       

# Sensitivity          Specificity       Pos Pred Value       Neg Pred Value 
# 0.6478873            0.9945946            0.9583333            0.9363868 
# Precision               Recall                   F1           Prevalence 
# 0.9583333            0.6478873            0.7731092            0.1609977 
# Detection Rate Detection Prevalence    Balanced Accuracy 
# 0.1043084            0.1088435            0.8212410 

########### ROCR Curve ########################

library(ROCR)
prediction_for_roc_curve <- predict(tRF, test_set[,c(1,3:31)],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38")
# Specify the different classes 
classes <- levels(as.factor(test_set$Attrition))

# For each class
for (i in 1:2)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(test_set$Attrition==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- ROCR::prediction(prediction_for_roc_curve[,i],true_values)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  legend("topright", c("1", "2"), text.col=pretty_colours, lty=1:3, col=1:3)
  # Calculate the AUC and print it to screen
  auc.perf <- ROCR::performance(pred, measure = "auc")
  print(auc.perf@y.values)
}


## Classification Error
with(test_set, table(Attrition, predict.class))

