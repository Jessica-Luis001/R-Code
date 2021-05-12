library(matlib)
library(smallstuff)
library(boot)
library(ISLR)
library(broom)
library(class)
library(MASS)
source("smallstuff2.R")

#NOTE: Whenever you need to set a seed, use number 120.

#1.	We will be working with the College data set in the ISLR package, with the 
#   variable Private as its response variable versus the three predictors 
#   Private, F.Undergrad, Outstate and PhD. Do the following.
data(College)
head(College)
?College
dim(College)

#a)	Create a numeric vector called Ks, containing the number 1, 5 through 20 by 
#   5, and 40 through the number of rows in College divided by 4 rounded to a 
#   whole number, by 20. So, we have Ks=(1,5,10,15,20,40,60,…).
(Ks=c(1,seq(5,20,by=5),seq(40,round2(nrow(College)/4),by=20)))

#b)	Find the training error rates (over the whole dataset), the 5-Fold CV, 
#   10-Fold CV, and LOOCV error rates for KNN for each K in the vector Ks.
#CV Error rates for KNN for K in Ks
knntrain=NULL
knn5err=NULL
knn10err=NULL
knnnerr=NULL
vars=c(7,9,13)
set.seed(120)
for (i in (1:length(Ks))) {
  yhat=knn(College[,vars,drop=F],College[,vars,drop=F],College$Private,k=Ks[i])
  knntrain[i]=mean(yhat!=College$Private)*100
  knn5err[i]=mean(CVerrorknn(College[,vars,drop=F],College$Private,K=Ks[i],k=5))
  knn10err[i]=mean(CVerrorknn(College[,vars,drop=F],College$Private,K=Ks[i],k=10))
  knnnerr[i]=mean(CVerrorknn(College[,vars,drop=F],College$Private,K=Ks[i]))
}

#c)	Plot the error rates versus their K for all four CV methods in the same window.
par(mfrow=c(2,2))
plot(knntrain~Ks,type='b',main="Training",xlab="K",ylab="% Error Rate")
plot(knn5err~Ks,type='b',main="5-CV",xlab="K",ylab="% Error Rate")
plot(knn10err~Ks,type='b',main="10-CV",xlab="K",ylab="% Error Rate")
plot(knnnerr~Ks,type='b',main="LOOCV",xlab="K",ylab="% Error Rate")

#d) Find the K with the lowest error rate for each. We must pick the “best” K;
#   this would be the highest K that still gives a low error rate. 
#   Which K would you pick?
Ks[c(which.min(knntrain),which.min(knn5err),which.min(knn10err),
     which.min(knnnerr))]
round2(c(min(knntrain),min(knn5err),min(knn10err),min(knnnerr)),2)
#Probably K=15, since that one gives a low error rate while still not being too
#flexible.

#e)	Calculate the LDA training error rates, 5-Fold CV, 10-Fold CV, and LOOCV.
#LDA
set.seed(120)
ldamod=lda(Private~F.Undergrad+Outstate+PhD,College)
ldatrain=mean(predict(ldamod)$class!=College$Private)*100
ldaerr5=CVerror(ldamod,5)
ldaerr10=CVerror(ldamod,10)
ldaerrn=CVerror(ldamod)
ldatrain;mean(ldaerr5);mean(ldaerr10);mean(ldaerrn)

#f)	Calculate the QDA training error rates, 5-Fold CV, 10-Fold CV, and LOOCV.
#QDA
set.seed(120)
qmod=qda(Private~F.Undergrad+Outstate+PhD,College)
qdatrain=mean(predict(qmod)$class!=College$Private)*100
qdaerr5=CVerror(qmod,5)
qdaerr10=CVerror(qmod,10)
qdaerrn=CVerror(qmod)
qdatrain;mean(qdaerr5);mean(qdaerr10);mean(qdaerrn)

#g)	Over all the results, KNN, LDA, and QDA, which model would you pick for your data?
i=which(Ks==15)
knn5err[i];knn10err[i];knnnerr[i]
#We pick KNN with K=15 since that gives us the lowest 10-CV error rate (6.18%)
#and the lowest LOOCV error rate (6.18%).

###################################################################################
#Logistic Regression
gmod=glm(Private~F.Undergrad+Outstate+PhD,binomial,College)
gtrain=errorRate(gmod)$errorRate
set.seed(120)
gerr5=CVerror(gmod,5)
gerr10=CVerror(gmod,10)
gerrn=CVerror(gmod)
gtrain;mean(gerr5);mean(gerr10);mean(gerrn)
