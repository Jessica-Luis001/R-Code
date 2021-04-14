library(matlib)
library(smallstuff)
library(ISLR)
library(broom)
library(class)
library(MASS)
source("smallstuff2.R")

#1 We will be working with the College data set in the ISLR package, with the 
#  variable Private as its response variable versus the three predictors F. Undergrad, 
#  Outstate and PhD. Get some information on the dataset, then do the following:
data(College)
names(College)
head(College$Private)
head(College[c(1,7,9,13,18)])
summary(College[c(1,7,9,13,18)])

#a)	Set a seed to 123, then create the variable train containing random indices 
#   for 500 observations in the data set; do this via the R function sample. 
#   The rows in the data set subset with train, form the training set. 
#   The remaining rows form the testing set. Find the percentage of private 
#   schools in each set.
(n=nrow(College))
set.seed(123)
train=sort(sample(1:n,500))
table(College$Private[train])/length(College$Private[train])*100
table(College$Private[-train])/length(College$Private[-train])*100
#72.6% in the training set
#72.9% in the testing set

#b)	Create a logistic regression object on the training set. Call it gmod.
gmod=glm(Private~F.Undergrad+Outstate+PhD,binomial,College,subset=train)
summary(gmod)

#c)	Create an lda object on the training set. Call it ldamod.
ldamod=lda(Private~F.Undergrad+Outstate+PhD,College,subset=train)

#d)	Create a qda object on the training set. Call it qmod.
qmod=qda(Private~F.Undergrad+Outstate+PhD,College,subset=train)

#e)	Find the error rate for the training set on gmod, ldamod, qmod, and KNN with 
#   K=1 and K=10. Which method has the lowest error rate? Second lowest?
errorRate(gmod)$errorRate                     #5.2%
predl=predict(ldamod)
mean(predl$class!=College$Private[train])     #6%
predq=predict(qmod)
mean(predq$class!=College$Private[train])     #6.4%
set.seed(123)
yhat1=knn(College[train,c(7,9,13)],College[train,c(7,9,13)],
          College$Private[train],prob=T)
mean(yhat1!=College$Private[train])           #0%
set.seed(123)
yhat10=knn(College[train,c(7,9,13)],College[train,c(7,9,13)],
           College$Private[train],10,prob=T)
mean(yhat10!=College$Private[train])          #4.4%
#Lowest is KNN with K=1, second lowest KNN with K=10.

#f)	Find the error rate for the testing set on lmod, ldamod, qmod, and KNN 
#   with K=1 and K=10. Which method appears to be better? Which methods overfit?
errorRate(gmod,College[-train,])$errorRate    #6.1%
predl=predict(ldamod,College[-train,])
mean(predl$class!=College$Private[-train])    #6.9%
predq=predict(qmod,College[-train,])
mean(predq$class!=College$Private[-train])    #7.2%
set.seed(123)
yhat1=knn(College[train,c(7,9,13)],College[-train,c(7,9,13)],
          College$Private[train],prob=T)
mean(yhat1!=College$Private[-train])          #10.1%
set.seed(123)
yhat10=knn(College[train,c(7,9,13)],College[-train,c(7,9,13)],
           College$Private[train],10,prob=T)
mean(yhat10!=College$Private[-train])         #8.3%
#Logistic Regression is the best one, then lda
#Both the KNN methods overfit.

#g)	Find the sensitivity and specificity for gmod, ldamod, qmod, and KNN with K=10. 
#   Which method has the best sensitivity? Which one has the best specificity? 
#   And which one has the second best specificity?
(er=errorRate(gmod,College[-train,]))
er$m[2,2]/(er$m[1,2]+er$m[2,2])        #sensitivity=98.0%
er$m[1,1]/(er$m[1,1]+er$m[2,1])        #specificity=82.7%

(ct=addmargins(table(yhat=predl$class,response=College$Private[-train])))
ct[2,2]/ct[3,2]    #sensitivity=98.0%
ct[1,1]/ct[3,1]    #specificity=80%

(ct=addmargins(table(yhat=predq$class,response=College$Private[-train])))
ct[2,2]/ct[3,2]    #sensitivity=96.5%
ct[1,1]/ct[3,1]    #specificity=82.7%

(ct=addmargins(table(yhat=yhat10,response=College$Private[-train])))
ct[2,2]/ct[3,2]    #sensitivity=95.5%
ct[1,1]/ct[3,1]    #specificity=81.3%
#LDA and logistic have the best sensitivity, logistic and QDA the best specificity, and 
#KNN with K=10 the next best specificity.

#h)	Create ROC plots for all the models in f). 
#   If possible, put them all on the same screen. NOTE: Use the testing set!
par(mfrow=c(2,3))
ROC(gmod,College[-train,c(1,7,9,13)])
ROC(ldamod,College[-train,c(1,7,9,13)])
ROC(qmod,College[-train,c(1,7,9,13)])
ROCknn(yhat1,College$Private[-train])
ROCknn(yhat10,College$Private[-train])
par(parSave)

#i)	What can you do to try and improve the KNN result?
#Try a higher K to stop the overfitting.
