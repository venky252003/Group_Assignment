#Setting the right working directory:
setwd("F:/Data Science/assignment/4")

#Importing the HR Employee Attrition Data file:
dataset=read.csv("data/HR_Employee_Attrition_Data.csv")

#Exploratory Data Analysis:
str(dataset)
summary(dataset)

#Plots:
library(ggplot2)
g <- ggplot(dataset)

#Attrition:
g+geom_bar(aes(Attrition,fill=Attrition))
#People in the company are 5 times more than the people leaving the company

#Education
g+geom_count(aes(x=Education ,y=Attrition))

#Monthly Income by Attrition with box plot
g+geom_boxplot(aes(y=MonthlyIncome, x=Attrition),width=0.5, outlier.colour = "dodgerblue", outlier.size = 4, outlier.shape = 16, outlier.stroke = 2, notch=T) + labs(title="Monthly Income by Attrition with box plot")

#Yearssincelastpromotion vs Attrition
g+geom_bar(aes(YearsSinceLastPromotion,fill=Attrition))
#People recently promoted quit the company more than the ones not promoted

#YearsWithCurrentManager vs Attrition:
g+geom_bar(aes(YearsWithCurrManager,fill=Attrition))
#As the number of years with Current Manager increases, Attrition decreases

#TrainingTimeLastYear vs Attrition:
g+geom_bar(aes(TrainingTimesLastYear,fill=Attrition))
#Attrition seen in employees trained betweeb 2-4times last year.

#YearsatCompany vs Attrition
g+geom_point(aes(YearsAtCompany,Attrition,size=YearsAtCompany))
#People with less no of years tend to quit the company more.

#TotalWorking Years vs Attrition
g+geom_bar(aes(TotalWorkingYears,fill=Attrition))
#People with less Experience are leaving the job more.

#Present Salary Hike vs Attrition
g+(aes(PercentSalaryHike,Attrition))+geom_point(alpha=0.01)
#People with Less Percent Hike leave the company.

#OverTime vs Attrition
g+geom_bar(aes(OverTime,fill=Gender,colour=Attrition))
#Male employees working overtime leave the company more

#WorkLifeBalance vs Attrition
g+geom_bar(aes(WorkLifeBalance,fill=Attrition))
#People with better work life balance may tend to quit more.

#Marital Status vs Attrition
g+geom_bar(aes(MaritalStatus,fill=Attrition))
#Attrtion higest in Employees who are single,medium in Employees who are married and least in Divorced Employees.

#JObRole vs Attrition
g+(aes(JobRole))+geom_bar(aes(fill=Attrition))
#Job ROle of Sales Representative has the most attrition in various job roles present.

#JobInvolvement vs Attrition
g+(aes(JobInvolvement))+geom_bar(aes(fill=Attrition))
#People leaving the company are highly involved in their jobs

#JobSatisfaction vs Attrition:
g+(aes(JobSatisfaction))+geom_bar(aes(fill=Attrition))
#Low JobSatisfaction results in people leaving the company.

#StockOptionLevels vs Attrition:
g+(aes(StockOptionLevel))+geom_bar(aes(fill=Attrition))
#Attrition high in people with No or Less Stock Options 

#Gender vs Attrition
g+geom_bar(aes(Attrition,fill=Gender))
#More in Male employees

#DistanceFromHome vs Attrition
g+geom_histogram(binwidth=40,aes(DistanceFromHome,fill=Attrition))
#People living in shorter distances from office leave more.

#Hourly,Daily & Monthly Rates vs Attrition:
g+geom_point(aes(DailyRate,Attrition),alpha = 0.05)
g+geom_point(aes(HourlyRate,Attrition),alpha = 0.05)
g+geom_point(aes(MonthlyRate,Attrition),alpha = 0.05)

#Education Field, Education vs Attrition
g+geom_bar(aes(Education,fill=Attrition))
g+geom_bar(aes(EducationField,fill=Attrition))
#Attrtion seems to be higher in Bachelors, Life Sciences and Medical

#Department vs Attrition
g+aes(Department,fill=Attrition)+geom_density(position = "stack")
#People from Reasearch&Development and Sales tend to quit more compared to HR

#Business Travel vs Attrition
g+geom_bar(aes(BusinessTravel,fill=Attrition))
#Attrition is directly proportional to Travel among the employees.

#Age vs Attrition
g+geom_histogram(binwidth = 10,aes(Age,fill=Attrition),colour="Black")
#Employees around 30years leave the company more

#Others:
g+geom_bar(aes(OverTime,fill=Attrition))+facet_grid(.~JobRole,scales = "free")
#High Attrition among Sales Representatives and Lab Technicians who work Overtime.

