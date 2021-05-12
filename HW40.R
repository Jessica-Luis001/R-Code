library(matlib)
library(smallstuff)
library(boot)
library(ISLR)
library(broom)
library(class)
library(MASS)
library(leaps)
source("smallstuff2.R")

#NOTE: Whenever you need to set a seed, use number 120.

# 1. We will be working with the Auto data set in the ISLR package, with the 
#    variable mpg as its response variable. However, load the version we 
#    created during HW5 instead. Do the following.
load("../data/Auto.RData")
head(Auto)
?Auto
dim(Auto)

# a) Using best subset selection but making sure to exclude the name variable, 
#    find the best simple linear regression model for this dataset. Then find 
#    the 90% CI for the parameter of the predictor.
subs=regsubsets(mpg~.-name,Auto)
lmod1=lmSub(subs,1)
summary(lmod1)
confint(lmod1,"weight",level=.9)

# b) Plot the response versus the predictor for the best simple linear 
#    regression model and add the regression line in a different color.
plot(mpg~weight,Auto)
abline(lmod1,col=2,lwd=2)

# c) Plot the BIC values for all d. According to the BIC, how many predictors 
#    does the best model have? State this model and indicate it on the graph. 
#    What is its BIC? How about its R^2, and adjusted R^2?
subsum=summary(subs)
plot(subsum$bic,xlab="d",ylab="BIC",type='b')
(idx=which.min(subsum$bic))      #4
#According to the BIC, the model with = 4+1 predictors is best.
points(idx,subsum$bic[idx],col=2,cex=2,pch='X')
round2(coef(subs,4),3)
#mpg=-18.3-.006weight+.77year+1.98originEuropean+2.22originJapanese+eps
subsum$bic[idx]
subsum$rsq[idx]
subsum$adjr2[idx]
#BIC=-640
#R2=81.9%
#Adjusted R2=81.7%

# d) Using backward stepwise selection, find the best linear regression 
#    model according to the BIC, and show its summary. Is it the same model 
#    as we got using best subset selection?
subs2=regsubsets(mpg~.-name,Auto,method="backward")
subsum2=summary(subs2)
(idx2=which.min(subsum2$bic))
summary(lmSub(subs2,idx2))
#Compare
summary(lmSub(subs,idx))
#Yes, this is the same model

# e) Plot the variables by decreasing BIC values (using the black boxes), 
#    for both best subset selection and R’s hybrid stepwise selection, on the 
#    same screen. Is there a difference?
subs3=regsubsets(mpg~.-name,Auto,method="seqrep")
subsum3=summary(subs3)
par(mfrow=c(2,1))
plot(subs)
plot(subs3)
par(parSave)
#Yes, they're different.
(idx=order(subsum$bic)[4]);subsum$bic[idx]
(idx3=order(subsum3$bic)[4]);subsum3$bic[idx3]
coef(subs,idx);coef(subs,idx3)
#The fourth lowest BIC value has 7+1 predictors for best subset selection,
#but 3+1 predictors for hybrid stepwise selection.

# f) Using best subset selection, plot the R^2, adjusted R^2, C_p, and BIC by d. 
#    Indicate the best values for the latter three. Find out if they all point 
#    to the same best subset. 
par(mfrow=c(2,2))
plot(subsum$rsq,xlab="Number of Variables",ylab="Rsq",type='b')
plot(subsum$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type='b')
(idx=which.max(subsum$adjr2))
points(idx,subsum$adjr2[idx],col=2,cex=2,pch='X')
plot(subsum$cp,xlab="Number of Variables",ylab="Cp",type='b')
(idx=which.min(subsum$cp))
points(idx,subsum$cp[idx],col=2,cex=2,pch='X')
plot(subsum$bic,xlab="Number of Variables",ylab="BIC",type='b')
(idx=which.min(subsum$bic))
points(idx,subsum$bic[idx],col=2,cex=2,pch='X')
par(parSave)
#According to BIC, the model with 4+1 predictors is best, while 
#according to the other statistics, the model with 7+1 predictors
#is best.

# g) We already know the best subset according to BIC, but if the others are 
#    different, show their summary. Are all predictors significant? If not, 
#    remove the insignificant predictor(s). Is the difference in adjusted 
#    R-squared large? Compare to the best subset with the same number of 
#    predictors. Any difference?
lmod7=lmSub(subs,7)
summary(lmod7)
#Cylinders is not significant
summary(lm(mpg~displacement+horsepower+weight+year+origin,Auto))
#Barely different; adjusted R2 goes from .8207 to .82.
#But now all predictors are significant
summary(lmSub(subs,6))
#The same model

# h) If you had to pick the best subset, which would you pick? 
#    Show the summary of your favorite model. Do you think this is it, or is 
#    there something else you can try to improve the model?
#I would prefer the model with 4 variables; it has the smallest p-value
#and the difference in adj. R2 doesn't appear to be very large after that.
summary(lmSub(subs,4))
glance(lmSub(subs,4))$p.value   #Smallest p-value
summary(lmSub(subs,5))
glance(lmSub(subs,5))$p.value
subsum$adjr2
#We may be able to improve the model by trying to add a quadratic term
#for weight.

#Extra; try adding a quadratic term for weight ####
subs4=regsubsets(mpg~.+I(weight^2)-name,Auto)
subsum4=summary(subs4)

plot(subsum4$bic,xlab="d",ylab="BIC",type='b')
(idx=which.min(subsum4$bic))
points(idx,subsum4$bic[idx],col=2,cex=2,pch='X')
summary(lmSub(subs4,idx))
#The best model according to BIC does include the quadratic term
#and has a better adjusted R2.
