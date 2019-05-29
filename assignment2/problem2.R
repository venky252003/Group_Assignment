setwd('F:/Data Science/assignment/2')
library('xlsx')
library(ggplot2)
library(psych)
library(lmtest)
library(zoo)

#Load Data
salt_data = read.xlsx("Dataset_LeslieSalt-1.xlsx", sheetIndex = 1)

#Factor County and Flood
salt_data$County = factor(salt_data$County, levels=c(0,1), labels = c('San Mateo', 'Santa Clara'))
salt_data$Flood = factor(salt_data$Flood, levels=c(1,0), labels = c('Flooding', 'Not Flooding'))
str(salt_data)
describe(salt_data)

#Check NA Values
sum(is.na(salt_data))

#Summary of data
summary(salt_data)

library(GGally)
ggpairs(salt_data)

library(FSA)
Summarize(Price ~ Size, data=salt_data)
Summarize(Price ~ Elevation, data=salt_data)
Summarize(Price ~ Sewer, data=salt_data)
Summarize(Price ~ Distance, data=salt_data)
Summarize(Price ~ Date, data=salt_data)
Summarize(Price ~ County, data=salt_data)
Summarize(Price ~ Flood, data=salt_data)

#Box Plot
boxplot(salt_data$Price, horizontal = TRUE, col="blue", xlab = "Price Box plot")
boxplot(salt_data$Size, horizontal = TRUE, col="blue", xlab = "Size Box plot")
boxplot(salt_data$Elevation, horizontal = TRUE, col="blue", xlab = "Elevation Box plot")
boxplot(salt_data$Sewer, horizontal = TRUE, col="blue", xlab = "Sewer Box plot")
boxplot(salt_data$Distance, horizontal = TRUE, col="blue", xlab = "Distance Box plot")
boxplot(salt_data$Date, horizontal = TRUE, col="blue", xlab = "Date Box plot")
boxplot(Price ~ County + Flood, data=salt_data, ylab="Price", col=c("blue", "yellow", "green"))

#Normal Density Chart
#Price Normality Distrbution chart
ggplot(salt_data, aes(x = salt_data$Price), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Price'))) + ylab(expression(bold('Density')))

#Size Normality Distrbution chart
ggplot(salt_data, aes(x = salt_data$Size), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Size'))) + ylab(expression(bold('Density')))
#Elevation Normality Distrbution chart
ggplot(salt_data, aes(x = salt_data$Elevation), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Elevation'))) + ylab(expression(bold('Density')))
#Sewer Normality Distrbution chart
ggplot(salt_data, aes(x = salt_data$Sewer), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Sewer'))) + ylab(expression(bold('Density')))
#Distance Normality Distrbution chart
ggplot(salt_data, aes(x = salt_data$Distance), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Distance'))) + ylab(expression(bold('Density')))
#Date Normality Distrbution chart
ggplot(salt_data, aes(x = salt_data$Date), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Date'))) + ylab(expression(bold('Density')))

#Normality by QQPlot and QQNorm
qqnorm(salt_data$Price)+qqline(salt_data$Price, col="red")
qqnorm(salt_data$Distance)+qqline(salt_data$Distance, col="red")
qqnorm(salt_data$Date)+qqline(salt_data$Date, col="red")
qqnorm(salt_data$Sewer)+qqline(salt_data$Sewer, col="red")
qqnorm(salt_data$Size)+qqline(salt_data$Size, col="red")
qqnorm(salt_data$Elevation)+qqline(salt_data$Elevation, col="red")

#Shapiro Test
shaprio_data = salt_data
shaprio_data$County= NULL
shaprio_data$Flood= NULL
do.call(rbind, lapply(shaprio_data, function(x) shapiro.test(x)[c("statistic", "p.value")]))

#Correaltion Matrix
data_numeric = salt_data[!names(salt_data) %in% c('County', 'Flood')]
cor(shaprio_data)
library(corrplot)
corrplot(cor(shaprio_data))


#Histogram
histogram(salt_data$County, col=c("red", "blue"), xlab = "County", ylab="Count")
histogram(salt_data$Flood, col=c("red", "blue"), xlab = "Flood", ylab="Count")

#Scatter Plot
plot(Price ~ Size, data=salt_data,  pch = 16,  cex = 1.0, xlab="Price", ylab="Size", col="red")
plot(Price ~ Distance, data=salt_data,  pch = 16,  cex = 1.0, xlab="Price", ylab="Distance", col="red")
plot(Price ~ Elevation, data=salt_data,  pch = 16,  cex = 1.0, xlab="Price", ylab="Elevation", col="red")
plot(Price ~ Sewer, data=salt_data,  pch = 16,  cex = 1.0, xlab="Price", ylab="Sewer", col="red")
plot(Price ~ Date, data=salt_data,  pch = 16,  cex = 1.0, xlab="Price", ylab="Date", col="red")

#Studying County category by Flood
crosstab = table(salt_data$County, salt_data$Flood)
crosstab
addmargins(crosstab)
chisq.test(crosstab)

#Remove Outlier in Size
size = salt_data[which(salt_data$Size < 300),]
boxplot(size$Size, col="blue")

#Remove Outlier in Price
price = size[which(size$Price < 30),]
boxplot(price$Price, col="blue")

#Remove Outlier in Elevation
elevation = price[which(price$Elevation < 15),]
boxplot(elevation$Elevation, col="blue")

#Remove Outlier in Elevation
sewer = elevation[which(elevation$Sewer < 7000),]
boxplot(sewer$Sewer, col="blue")

#Remove Outlier in Elevation
describe(sewer)
Data1= sewer
Data1$County= NULL
Data1$Flood= NULL

#kruskal test after removing outlier
kruskal.test(Data1)

#Z-Score Standardization
dfNormZ <- as.data.frame(scale(Data1))

#Shapiro test after removing outlier
do.call(rbind, lapply(dfNormZ, function(x) shapiro.test(x)[c("statistic", "p.value")]))

#Data Transformation to Normalize variables
shapiro.test(abs(dfNormZ$Size))
shapiro.test(log(dfNormZ$Distance))
shapiro.test(log(dfNormZ$Sewer))
shapiro.test(log(dfNormZ$Date))



Model_data = sewer
Model_data$Normal_Size = abs(dfNormZ$Size)
Model_data$Normal_Distance = dfNormZ$Distance
Model_data$Normal_Sewer =  dfNormZ$Sewer
Model_data$Normal_Date =  dfNormZ$Date
Model_data$Normal_Price =  dfNormZ$Price
Model_data$Normal_Elevation =  dfNormZ$Elevation

#Shapiro test after removing outlier
do.call(rbind, lapply(Model_data, function(x) shapiro.test(x)[c("statistic", "p.value")]))


#Normality by QQPlot and QQNorm after Data Transformation
qqnorm(Model_data$Price)+qqline(Model_data$Price, col="red")
qqnorm(Model_data$Normal_Distance)+qqline(Model_data$Normal_Distance, col="red")
qqnorm(Model_data$Normal_Date)+qqline(Model_data$Normal_Date, col="red")
qqnorm(Model_data$Normal_Sewer)+qqline(Model_data$Normal_Sewer, col="red")
qqnorm(Model_data$Normal_Size)+qqline(Model_data$Normal_Size, col="red")
qqnorm(Model_data$Normal_Elevation)+qqline(Model_data$Normal_Elevation, col="red")

#manova test
manova = aov(Price~Elevation+Date+Flood+Size+County+Sewer+Distance, data = Model_data)
summary(manova)

#Create Model as Price (DV) other ID
model1= lm(Price~Elevation+Date+Flood+Size+County+Sewer+Distance, data=Model_data)
summary(model1)
anova(model1)

model1_MAPE<-model1$residual/Model_data$Price
mean(abs(model1_MAPE))


#Create Model as Price (DV) and remove County (0.25351)
model2= lm(Price~Elevation+Date+Flood+Size+Sewer+Distance, data=Model_data)
summary(model2)
anova(model2)

model2_MAPE<-model2$residual/Model_data$Price
mean(abs(model2_MAPE))

#Create Model as Price (DV) and remove Size ( 0.30865)
model3 = lm(Price~Elevation+Date+Flood+Sewer+Distance, data=Model_data)
summary(model3)
anova(model3)

model3_MAPE<-model3$residual/Model_data$Price
mean(abs(model3_MAPE))

#Create Model as Price (DV) and remove Sewer (0.206502)
model4 = lm(Price~Elevation+Date+Flood+Distance, data=Model_data)
summary(model4)
anova(model4)
model4_MAPE<-model4$residual/Model_data$Price
mean(abs(model4_MAPE))

#Create Model as Price (DV) and remove Elevation (0.03720)
model5 = lm(Price~Date+Flood+Distance, data=Model_data)
summary(model5)
model5_MAPE<-model5$residual/Model_data$Price
mean(abs(model5_MAPE))

#Create Model as Price (DV) and with normal data set
model5 = lm(Normal_Price~Normal_Distance+Normal_Date+Normal_Sewer+Normal_Elevation+County+Flood, data=Model_data)
summary(model5)

# Residual Vs Fitted #
Regresi4 = residuals(model4)
fit4 = fitted(model4)


library("ggplot2")
plot(fit4,Regresi4, main="Residuals Vs Fit Model", xlab = "Price", ylab = "Residual", col="Blue")
abline(h=0,col="Red")

shapiro.test(Regresi4)

#Plot 
par(mfrow = c(2, 2))
plot(model4)

#Predication
Prediction = predict(model4)

Pred.newdata <- data.frame (0,"Santa Clara", 246.8, 0, 0, 0, "Not Flooding", 5)
colnames(Pred.newdata) <- c("Price", "County", "Size", "Elevation", "Sewer", "Date", "Flood", "Distance")
predict(model4, newdata = Pred.newdata)

#Backtrack 
BackTrack = data.frame(Model_data$Price, Prediction)

library(lattice)
plot(Model_data$Price, col="Red", xlab = "Data Point")
lines(Model_data$Price, col="Red")
plot(Prediction, col="Blue")
lines(Prediction, col="Blue")


#VIF - Variation inflation factor -as a thumb rule if the VIF is greater than 10 it suggests there is multi collinerity
# If the VIF is greater than 100 it certainly demands us to fix this as it could greatly affect results in regression
library(car)
vif(model4)

#What is Auto Correlation  https://www.cengage.com/resource_uploads/downloads/0030348064_54176.pdf
#Durbin Watson Test to test Auto Correlation
##Null Hypothesis states that there is No auto-correlation 
#Alternate Hypothesis states there is autocorrelation
dwtest(model4)


## Homoscedasticity tested using Goldfelt Quant test
#Null hypothesis : Data satisfies the condiction of homoscedasticity
##Alternate hypothesis states data is not Homoscedastic
gqtest(model4)

