#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

library(caTools)
#Importing the HR Employee Attrition Data file:
dataset=read.csv("data/HR_Employee_Attrition_Data.csv")
table(dataset$Attrition)

set.seed(144)
spl = sample.split(dataset$Attrition, SplitRatio = 0.7)
HRTrain = subset(dataset, spl == TRUE)
HRTest = subset(dataset, spl == FALSE)

head(HRTrain)
setDT(HRTrain)
dim(HRTrain)
str(HRTrain)

#check missing values
table(is.na(HRTrain))
table(is.na(HRTest))

summarizeColumns(HRTrain)
summary(HRTrain)

library(caret)
library(mlr)
library(data.table)

#If there any missing value the use impute function
imp1 <- impute(data = HRTrain,target = "Attrition",classes = list(integer=imputeMedian(), factor=imputeMode()))
imp2 <- impute(data = HRTest,target = "Attrition",classes = list(integer=imputeMedian(), factor=imputeMode()))
HRTrain <- imp1$data
HRTest <- imp2$data

#Move Attriation to first
HRTrain = head(HRTrain[,c(2,1, 3:35)])
head(HRTrain)

#Create Dummy Varibales
HRTrain_Dummy <- createDummyFeatures(HRTrain, target = "Attrition", cols = c("BusinessTravel",
    "Department","EducationField", "Gender", "JobRole", "MaritalStatus", "Over18", "OverTime"))

#Check Data imbalance
setDT(HRTrain)[,.N/nrow(HRTrain),Attrition]
setDT(HRTest)[,.N/nrow(HRTest),Attrition]

#Sort by Attrition
HRTrain <- HRTrain[order(Attrition) , ]

#As factor
HRTrain$Attrition = as.factor(HRTrain$Attrition)
HRTrain$BusinessTravel = as.factor(HRTrain$BusinessTravel)
HRTrain$Department = as.factor(HRTrain$Department)
HRTrain$EducationField = as.factor(HRTrain$EducationField)
HRTrain$Gender = as.factor(HRTrain$Gender)
HRTrain$JobRole = as.factor(HRTrain$JobRole)
HRTrain$MaritalStatus = as.factor(HRTrain$MaritalStatus)
HRTrain$OverTime = as.factor(HRTrain$OverTime)
HRTrain$Over18 = as.factor(HRTrain$Over18)

## Essential Step in mlr package. You have to ensure that response variable is identified correctly
HRTrainTask=makeClassifTask(data=HRTrain, target = "Attrition", positive = "Yes")
HRTestTask=makeClassifTask(data=HRTest, target = "Attrition", positive = "Yes")

HRTrainTaskOriginal=HRTrainTask
paroleTrainTask= smote(paroleTrainTaskOriginal, rate = 6, nn= 5)