setwd('F:/Data Science/assignment/2')
library('xlsx')
library('ggplot2')
library(psych)
library(lmtest)
library(zoo)
library(nFactors)

cereal_data = read.csv('Dataset_Cereal.csv')
#Converting to 6 to 5
cereal_data[cereal_data==6]=5
summary(cereal_data)
str(cereal_data)
describe(cereal_data)

#chisq.test(cereal_data[2:26])
cor(cereal_data[2:26])
corr.test(cereal_data[2:26])
cortest.bartlett(cereal_data[2:26]) #0

#Correlation Plot
library(corrplot)
corrplot(cor(cereal_data[2:26]), method = "circle",bg = "grey")


#KMO Test
KMO(cereal_data[2:26])
#Barett Test
cortest.bartlett(cereal_data[2:26])
#Scree Plot
scree(cereal_data[2:26])
##Checking for Null values
sum(is.na(cereal_data))

#Loading
fa.out<-fa(easy_drop_cereal[2:25], nfactors = 4,fm="pa", rotate = "varimax")
fa.out$loadings
fa.diagram(fa.out)
print(fa.out$loadings, cutoff = 0.4)

## Plot loadings against one another
load = fa.out$loadings[,1:2]
plot(load, type="n") # set up plot 
text(load,labels=names(cereal_data),cex=.7) # add variable names


############################### FACTOR Analysis ######################################
process_factor = function(data, factors){
  print(KMO(data))
  cat(sprintf('Bartlett - %s\n', cortest.bartlett(data)$p.value))
  for (nfactor in factors) {
    print(nfactor)
    #cat(sprintf('KMO - %s\n', KMO(data)))
    fa.out<-fa(data, nfactors = nfactor,fm="pa", rotate = "none")
    # "varimax", "quartimax", "equamax", 
    cat(sprintf('Variance - %s\n', sum(100*fa.out$e.values[1:nfactor]/length(fa.out$e.values))))
    print(fa.out$communality[fa.out$communality == min(fa.out$communality)])
    print(fa.out$communality[fa.out$communality == max(fa.out$communality)])
  }
}


factors = c(3:6)
data = cereal_data[2:26]
KMO(data)
scree(data)
process_factor(data, factors)

drops <- c("Easy")
easy_drop_cereal = cereal_data[ , !(names(cereal_data) %in% drops)]
KMO(easy_drop_cereal[2:25])
scree(easy_drop_cereal[2:25])
factors = c(3:5)
process_factor(easy_drop_cereal[2:25], factors)
fa.diagram(fa.out)

drops <- c("Easy", "Process")
process_drop_cereal = cereal_data[ , !(names(cereal_data) %in% drops)]
KMO(process_drop_cereal[2:24])
scree(process_drop_cereal[2:24])
process_factor(process_drop_cereal[2:24], factors)

drops <- c("Easy", "Process", "Soggy")
soggy_drop_cereal = cereal_data[ , !(names(cereal_data) %in% drops)]
KMO(soggy_drop_cereal[2:23])
scree(soggy_drop_cereal[2:23])
process_factor(soggy_drop_cereal[2:23], factors)

drops <- c("Easy", "Kids", "Soggy")
kids_drop_cereal = cereal_data[ , !(names(cereal_data) %in% drops)]
KMO(kids_drop_cereal[2:23])
scree(kids_drop_cereal[2:23])
factors = c(3:5)
process_factor(kids_drop_cereal[2:23], factors)

fa_drop.out<-fa(kids_drop_cereal[2:23], nfactors = 5, fm="pa")
fa_drop.out$loadings
fa.diagram(fa_drop.out)
print(fa_drop.out$loadings, cutoff = 0.3)
fa_drop.out


########################## PCA ######################################
corrmatrix = cor(cereal_data[2:26])
cortest.bartlett(cereal_data[2:26], nrow(cereal_data)) #0
a = eigen(corrmatrix)
eigenvectors = a$vectors
eigenvalues = a$values
d = diag(eigenvalues)
pcaloading = as.matrix(eigenvectors%*%d)
#Variance
print(eigenvalues/sum(eigenvalues)*100)
print(cumsum(eigenvalues/sum(eigenvalues)*100))

Factor=c(1:25)
Scree=data.frame(Factor,eigenvalues)
plot(Scree,main="Scree Plot", col="Blue",ylim=c(0,4))
lines(Scree,col="Red")

Unrotate=principal(cereal_data[2:26], nfactors=3, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(cereal_data[2:26],nfactors=3,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)
####################################################################