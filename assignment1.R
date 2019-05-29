setwd('F:/Data Science/Code/R/resendency2')
data = read.csv('data/titain_insurance.csv')

attach(data)
summary(data)

hist(data$Old_Scheme)
hist(data$New_Scheme)

qqnorm(data$Old_Scheme)
qqline(Old_Scheme)

qqnorm(data$New_Scheme)
qqline(New_Scheme)

old = data$Old_Scheme
new = data$New_Scheme
new_5 = data$New_Scheme+5

#Data is normaly distrbutted
t.test(Old_Scheme, New_Scheme)
t.test(Old_Scheme, New_Scheme, var.equal = FALSE)
t.test(Old_Scheme, New_Scheme, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
t.test(Old_Scheme, New_Scheme, paired = TRUE, var.equal = TRUE, alternative = "two.sided")

t.test(Old_Scheme, New_Scheme, var.equal = TRUE, alternative = "greater", conf.level = 0.95)

t.test(New_Scheme, Old_Scheme, paired = TRUE, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
t.test(New_Scheme, Old_Scheme, paired = TRUE, alternative = "greater", conf.level = 0.95)
'No Match -> NULL
Imporment -> Alternate'

var(Old_Scheme, new_5)

t.test(old)
#Two-Sample u-test in R
wilcox.test(old, new)

mean(New_Scheme*1000)
sd(Old_Scheme*1000)

n = 30
difference = New_Scheme-Old_Scheme
SE = sd(difference)/sqrt(n)
alpha = 0.05
t_cirtcal = qt(alpha, df=n-1, lower.tail = F)
Mu = 0
X_bar = Mu + t_cirtcal * SE
t_stat = (X_bar-5)/SE
pt(t_stat, df=n-1, lower.tail = F)

#Correlation
cor(old, new) 
#0.811802
cov(old, new)
#399.5851
cov2cor(old)
var(old, new)

test(old, new)
#Significance Testing in Correlation Tests
cor.test(old, new)

shapiro.test(New_Scheme)
shapiro.test(Old_Scheme)

dev.off()
boxplot(Old_Scheme, horizontal = F)
boxplot(New_Scheme, horizontal = F)

n=30
Difference<-(New_Scheme*1000-Old_Scheme*1000)

#Standarad Error
SE<-(sd(Difference)/sqrt(n))
alpha<-0.05
t_critical<-qt(alpha,df=n-1, lower.tail = F)
mu0<-0
X_bar<-mu0+t_critical*SE

t_stat<-(X_bar-5000)/SE
pt(t_stat,df=n-1, lower.tail = F)

t_stat<-(X_bar)/SE
pt(t_stat,df=n-1, lower.tail = F)

library(ggplot2)
boxplot(bfi$A1)
qplot(bfi$A1, geom = 'histogram', binwidth = 2) + xlab('Time')
datasim <- data.frame(New_Scheme)
ggplot(datasim, aes(x = New_Scheme), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('New Schema'))) + 
  ylab(expression(bold('Density')))

library(moments)
skewness(Old_Scheme)
kurtosis(Old_Scheme)

ggplot(data, aes(x="", y=Old_Scheme)) + 
  geom_boxplot()
