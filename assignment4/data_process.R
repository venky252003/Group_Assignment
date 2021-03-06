#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Importing the HR Employee Attrition Data file:
dataset=read.csv("data/HR_Employee_Attrition_Data.csv")

#Exploratory Data Analysis:
str(dataset)
summary(dataset)

#Converting Catgeorical to Numeric
dataset$BusinessTravel <- as.numeric(factor(dataset$BusinessTravel,
                                            levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'),
                                            labels=c(1,2,3)))
dataset$EducationField <- as.numeric(factor(dataset$EducationField,
                                            levels=c('Human Resources','Life Sciences','Medical','Marketing','Technical Degree','Other'),
                                            labels=c(1,2,3,4,5,6)))
dataset$Department <- as.numeric(factor(dataset$Department,
                                        levels=c('Sales','Research & Development','Human Resources'),
                                        labels=c(1,2,3)))
dataset$Gender <- as.numeric(factor(dataset$Gender,
                                    levels=c('Female','Male'),
                                    labels=c(1,2)))
dataset$JobRole <- as.numeric(factor(dataset$JobRole,
                                     levels=c('Healthcare Representative','Human Resources','Laboratory Technician','Manager','Manufacturing Director','Research Director','Research Scientist','Sales Executive','Sales Representative'),
                                     labels=c(1,2,3,4,5,6,7,8,9)))
dataset$MaritalStatus <- as.numeric(factor(dataset$MaritalStatus,
                                           levels=c('Married','Single','Divorced'),
                                           labels=c(1,2,3)))
dataset$OverTime <- as.numeric(factor(dataset$OverTime,
                                      levels=c('Yes','No'),
                                      labels=c(1,2)))

dataset$Attrition <- as.numeric(factor(dataset$Attrition,
                                       levels=c('Yes','No'),
                                       labels=c(1,2)))

# Unwanted variable
dataset$Over18 = NULL
dataset$StandardHours = NULL
dataset$EmployeeCount = NULL
dataset$EmployeeNumber = NULL

summary(dataset)
str(dataset)

#write.csv(dataset, file = 'data/HR_data_cleaned.csv')

#Checking for Correlation between variables:
library(corrplot)
library(psych)
library(ggcorrplot)

cormat = round(cor(dataset),2)
ggcorrplot(cormat)
ggcorrplot(cormat, hc.order = TRUE, type = "lower",
           outline.col = "white")

#YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager , WorkLifeBalance
# are highly correalted 

# #From Correlation table:
# dataset$YearsAtCompany =  NULL
# dataset$YearsInCurrentRole = NULL
# dataset$YearsSinceLastPromotion = NULL
# dataset$MonthlyIncome = NULL
# dataset$PerformanceRating = NULL


#Write Hypothesis and validate the Hypothesis
model = lm(Attrition ~ ., data=dataset)
anova(model)
#res.man <- manova(Attrition ~ ., data = dataset)

# Analysis of Variance Table
# 
# Response: Attrition
#   Df  Sum Sq Mean Sq  F value    Pr(>F)    
#   Age                         1  10.077 10.0771  94.8827 < 2.2e-16 ***
#   BusinessTravel              1   6.226  6.2256  58.6184 2.587e-14 ***
#   DailyRate                   1   1.118  1.1183  10.5294 0.0011882 ** 
#   Department                  1   1.418  1.4178  13.3497 0.0002630 ***
#   DistanceFromHome            1   2.393  2.3935  22.5361 2.162e-06 ***
#   Education                   1   0.000  0.0004   0.0039 0.9501461    
#   EducationField              1   0.315  0.3150   2.9657 0.0851524 .  
#   EmployeeNumber              1   0.020  0.0201   0.1891 0.6636623    
#   EnvironmentSatisfaction     1   3.913  3.9127  36.8405 1.448e-09 ***
#   Gender                      1   0.389  0.3890   3.6627 0.0557426 .  
#   HourlyRate                  1   0.022  0.0222   0.2092 0.6474531    
#   JobInvolvement              1   6.427  6.4273  60.5171 1.005e-14 ***
#   JobLevel                    1   5.189  5.1893  48.8609 3.394e-12 ***
#   JobRole                     1   0.005  0.0046   0.0433 0.8350985    
#   JobSatisfaction             1   4.794  4.7941  45.1394 2.199e-11 ***
#   MaritalStatus               1   0.047  0.0472   0.4447 0.5049291    
#   MonthlyIncome               1   0.144  0.1443   1.3588 0.2438446    
#   MonthlyRate                 1   0.205  0.2049   1.9289 0.1649900    
#   NumCompaniesWorked          1   4.011  4.0112  37.7682 9.055e-10 ***
#   OverTime                    1  25.876 25.8757 243.6363 < 2.2e-16 ***
#   PercentSalaryHike           1   0.116  0.1160   1.0922 0.2960704    
#   PerformanceRating           1   0.050  0.0499   0.4700 0.4930285    
#   RelationshipSatisfaction    1   1.098  1.0979  10.3373 0.0013180 ** 
#   StockOptionLevel            1   6.609  6.6092  62.2301 4.288e-15 ***
#   TotalWorkingYears           1   0.619  0.6189   5.8273 0.0158405 *  
#   TrainingTimesLastYear       1   0.665  0.6649   6.2601 0.0124035 *  
#   WorkLifeBalance             1   1.202  1.2023  11.3200 0.0007767 ***
#   YearsAtCompany              1   0.000  0.0001   0.0008 0.9768861    
#   YearsInCurrentRole          1   2.079  2.0791  19.5760 1.002e-05 ***
#   YearsSinceLastPromotion     1   2.296  2.2960  21.6181 3.475e-06 ***
#   YearsWithCurrManager        1   1.407  1.4068  13.2460 0.0002779 ***
#   Residuals                2908 308.848  0.1062                       
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Check multi collinerity
library(car)
vif(model)
#Job level and MonthlyIncome has more than 10, so we can keep any one variable

library(caTools)
set.seed(123)

#Spliting Dataset in 70/30 ratio
split=sample.split(dataset$Attrition, SplitRatio =0.7)

#Check any NaN values in Dataset
table(is.na(dataset))

#Traing Data set
training_set=subset(dataset,split==TRUE)
#Test Data set
test_set=subset(dataset,split==FALSE)

#Save in File
write.csv(training_set, file = 'data/HR_train_data_cleaned.csv')
write.csv(test_set, file = 'data/HR_test_data_cleaned.csv')

#Check Target variable data
table(dataset$Attrition)

#Base line Accuracy
prop.table(table(training_set$Attrition))
#Train 0.8386
prop.table(table(test_set$Attrition))
#Test 0.8390



