library(matlib)
library(smallstuff)
library(ISLR)
library(broom)
library(sur)
library(MASS)
source('smallstuff2.R')

#1.	We will be working with the NELS data set in the sur package, 
#   with the variable advmath8 as its response variable. Get some 
#   information on the dataset, then do the following:
data(NELS)
?NELS
head(NELS)
dim(NELS)
head(NELS$advmath8)

#a)	We will be working with the variables slfcnc08 and achmat08 in addition to 
#   the response. Find the indices of the 3 variables of interest using the names 
#   function. Then obtain summaries for those 3 variables using those indices.
names(NELS)
summary(NELS[,c(2,9,34)])

#b)	We are going to create a new dataset called NELS2, which must contain only 
#   the three variables of interest, and only those rows that do not have missing 
#   values. The function complete.cases takes a data frame as input, and returns 
#   a logical value for each row; TRUE if the row contains no missing values, 
#   FALSE if it does. So, we subset the rows of NELS using the output of this 
#   function on the variables of interest, and we subset the columns using the 
#   indices of the variables of interest. Example: If the variables we are interested 
#   in have indices 3 and 6 (NOTE that those are not the correct indices), we would 
#   create NELS2 as follows: NELS2=NELS[complete.cases(NELS[,c(3,6)]),c(3,6)].Here 
#   we have used complete.cases(NELS[,c(3,6)]) to subset the rows, and c(3,6) to 
#   subset the columns. Show the first few rows of NELS2 to see if you did it right. 
#   Set n equal to the number of rows in NELS2.
NELS2=NELS[complete.cases(NELS[,c(2,9,34)]),c(2,9,34)]
head(NELS2)
(n=nrow(NELS2))

#c)	Create a vector that consists of the numbers 250 to the number of 
#   observations in the NELS2 dataset and call it train. This will represent 
#   the indices for the observations in the training set; the remaining 
#   observations will be the testing set. Create an LDA model on the response 
#   versus slfcnc08 and achmat08 for the training set (set the argument subset 
#   equal to train). 
train=(250:n)
(ldamod=lda(advmath8~.,NELS2,subset=train))

#d)	Find the error rate for the training set.
pred=predict(ldamod)
mean(pred$class!=NELS2$advmath8[train])  #31.4%

#e)	Find the error rate for the testing set.
pred=predict(ldamod,NELS2[-train,])
mean(pred$class!=NELS2$advmath8[-train]) #34.1%

#f)	Find the sensitivity for the testing set.
addmargins(table(yhat=pred$class,response=NELS2$advmath8[-train]))
56/115
#Sensitivity is 48.7%

#g)	Create a logistic regression model for the training set and give the 
#   training error rate.
gmod=glm(advmath8~.,binomial,NELS2,subset=train)
errorRate(gmod)$errorRate                 #31.4%

#h)	Find the testing error rate for the logistic regression model.
errorRate(gmod,NELS2[-train,])$errorRate  #33.7%

#i)	For observation 10 in the testing set, find the probability that 
#   this student took advanced math in 8th grade using both the LDA model 
#   and the logistic regression model.
#LDA
pred$posterior[10,2]
#Logistic regression
predict(gmod,NELS2[-train,],type="r")[10]

#j)	Create an ROC plot for the testing set for each method and put them on the same page.
par(mfrow=c(2,1))
ROC(ldamod,NELS2[-train,])
ROC(gmod,NELS2[-train,])
par(parSave)

