library(matlib)
library(smallstuff)
library(ISLR)
library(MASS)
library(broom)
library(ROCR)
library(sur)
library(class)
library(faraway)
source('smallstuff2.R')

# NOTE: Whenever you need to set a seed, use number 120.

#1.	Load the Auto data set from your data directory, and once again let the 
#   variable mpg be the response variable. Get some information about the dataset. 
#   We will continue the problem in HW34 where we aim to perform a linear 
#   regression of mpg versus some polynomial of horsepower, and we want to find 
#   out which polynomial would be best. Do the following.
data(Auto)
load("../data/Auto.RData")
?Auto
head(Auto)
dim(Auto)
(n=nrow(Auto))

#a)	Like before, use the function sample to split the data equally into a training set 
#   and a testing set. Call the training set AutoTrain and the testing set AutoTest. 
#   Except for the last part of the problem, we will be working with AutoTrain.
set.seed(120)
tr=sort(sample(nrow(Auto),round2(nrow(Auto)/2)))
AutoTrain=Auto[tr,]
AutoTest=Auto[-tr,]
rm(tr)

#b)	Obtain the 5-Fold CV MSE for polynomials from 1 to 6. Plot them. Which one seems best?
#5-fold CV
MSEcv5=NULL
set.seed(120)
for (i in 1:6) {
  gmod=glm(mpg~poly(horsepower,i),data=AutoTrain)
  MSEcv5[i]=cv.glm(AutoTrain,gmod,K=5)$delta[1]
}
MSEcv5
plot(MSEcv5,type='b',main="5-Fold Cross-Validation")
which.min(MSEcv5)
#The Quartic polynomial seems best.

#c)	Obtain the 10-Fold CV for polynomials from 1 to 6. Plot them. Which one seems best?
#10-fold CV
MSEcv10=NULL
set.seed(120)
for (i in 1:6) {
  gmod=glm(mpg~poly(horsepower,i),data=AutoTrain)
  MSEcv10[i]=cv.glm(AutoTrain,gmod,K=10)$delta[1]
}
MSEcv10
plot(MSEcv10,type='b',main="10-Fold Cross-Validation")
which.min(MSEcv10)
#The quadratic polynomial seems best; cubic has the lowest MSE
#but it is really close to quadratic.

#d)	Obtain the LOOCV MSE for polynomials from 1 to 6. Plot them. Which one seems best?
#LOOCV
MSEcvn=NULL
set.seed(120)
for (i in 1:6) {
  gmod=glm(mpg~poly(horsepower,i),data=AutoTrain)
  MSEcvn[i]=cv.glm(AutoTrain,gmod)$delta[1]
}
MSEcvn
plot(MSEcvn,type='b',main="LOOCV")
which.min(MSEcvn)
#The quadratic polynomial seems best.

#e)	Choose a final polynomial. Create a model on the full training set using that 
#   polynomial. State the model and calculate the training MSE. Hint: If you need to 
#   state the model, you cannot use the poly function.
#I will choose the quadratic since the MSE seems low enough
#with every CV method
lmod=lm(mpg~horsepower+I(horsepower^2),AutoTrain)
round2(coef(lmod),3)
#mpg=59.155-.51horsepower+.001horsepower^2+eps
summary(lmod)
#Training MSE
(MSE=deviance(lmod)/n)

#f)	Plot mpg versus horsepower again. Using the best model created in e), 
#   plot the regression line. Make sure to order the fitted values and horsepower 
#   correctly and plot the line in a different color. 
o=order(AutoTrain$horsepower)
plot(mpg~horsepower,AutoTrain,main="Cars")
lines(fitted(lmod)[o]~horsepower[o],AutoTrain, col=2)

#g)	Use AutoTest to obtain the testing MSE.
mean((AutoTest$mpg-predict(lmod,AutoTest))^2)
