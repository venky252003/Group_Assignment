setwd('F:/Data Science/assignment/2')
library('xlsx')
library(ggplot2)
library(psych)
library(lmtest)
library(zoo)

#Load Data
green_data = read.xlsx("Dataset_All Greens Franchise-1.xls", sheetIndex = 1)

#Rename Columns
names(green_data) <- c("annual_amt", "floor_size", "inventory", "advertising", "sales", "no_stores_competing")

describe(green_data)

#Check NA Values
sum(is.na(green_data))

#Box Plot
boxplot(green_data$annual_amt, horizontal = TRUE, col="blue", xlab = "Annual Sales Box plot")
boxplot(green_data$floor_size, horizontal = TRUE, col="blue", xlab = "Number sq ft  Box plot")
boxplot(green_data$inventory, horizontal = TRUE, col="blue", xlab = "Inventory Box plot")
boxplot(green_data$advertising, horizontal = TRUE, col="blue", xlab = "Advertising Box plot")
boxplot(green_data$sales, horizontal = TRUE, col="blue", xlab = "Size of sales district Box plot")
boxplot(green_data$no_stores_competing, horizontal = TRUE, col="blue", xlab = "Number of competing stores in district Box plot")

#Shapiro test after removing outlier
do.call(rbind, lapply(green_data, function(x) shapiro.test(x)[c("statistic", "p.value")]))

#manova test
manova = aov(annual_amt~floor_size+inventory+advertising+sales+no_stores_competing, data = green_data)
summary(manova)

model1 = lm(annual_amt~., data=green_data)
summary(model1)


actual_fit = green_data$annual_amt - model1$fitted
actual_fit_div = model1$residuals/green_data$annual_amt
actual_fit_div_100 = actual_fit_div*100
mean(abs(actual_fit_div_100))

library(car)
vif(model1)

model2 = lm(annual_amt~.-inventory, data=green_data)
summary(model2)
vif(model2)

plot(green_data$annual_amt, green_data$floor_size+green_data$advertising+green_data$sales+green_data$no_stores_competing, col="red")
abline(a = coefficients(model2), b=0, col = "blue")



library(corrplot)
cor_matrix = cor(green_data)
corrplot(cor_matrix)
corrplot(cor_matrix,method ="number")

#What is Auto Correlation  https://www.cengage.com/resource_uploads/downloads/0030348064_54176.pdf
#Durbin Watson Test to test Auto Correlation
##Null Hypothesis states that there is No auto-correlation 
#Alternate Hypothesis states there is autocorrelation
dwtest(model2)


## Homoscedasticity tested using Goldfelt Quant test
#Null hypothesis : Data satisfies the condiction of homoscedasticity
##Alternate hypothesis states data is not Homoscedastic
gqtest(model2)

res = resid(model2)
plot(green_data$annual_amt, res, ylab="Residuals", xlab="X1") 
abline(0, 0, col="red")                  # the horizon
