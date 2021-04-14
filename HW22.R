library(matlib)
library(smallstuff)
library(ISLR)
library(broom)
source('smallstuff2.R')

#1.	We will be working with the College data set in the ISLR package, 
#   with the variable Private as its response variable. Answer the following questions.
data(College)

#a)	First get some general information about College and display the first 6 rows. 
#   Then create a summary on the following 5 variables of interest: Private, F.Undergrad, 
#   Outstate, PhD, Grad.Rate. Make sure to check what each of those variables means! 
#   Store the number of rows in the variable n.
?College
head(College)
dim(College)
head(College$Private)
names(College)
summary(College[c(1,7,9,13,18)])
(n=nrow(College))

#b)	Get some more information about the variables of interest. Produce two plots, 
#   either next to each other, or one below the other (you will need to change the 
#   plot window for that). First, plot F.undergrad versus Outstate, giving public 
#   colleges a different color from private colleges, and produce a legend for the 
#   two different types of colleges. Add a linear regression line for F.undergrad 
#   versus Outstate. Then show the correlation between the two variables. For the 
#   second plot, do the same, but use PhD versus Grad.Rate instead. Finally, 
#   change the plot window setting back to its default.
par(mfrow=c(2,1))
plot(F.Undergrad~Outstate,College,col=Private,pch=20,cex=.8)
legend("topright",c("Public","Private"),pch=20,pt.cex=.8,col=1:2)
abline(lm(F.Undergrad~Outstate,College))
cor(College$F.Undergrad,College$Outstate)
plot(PhD~Grad.Rate,College,col=Private,pch=20,cex=.8)
legend("bottomright",c("Public","Private"),pch=20,pt.cex=.8,col=1:2)
abline(lm(PhD~Grad.Rate,College))
cor(College$PhD,College$Grad.Rate)
par(parSave)

#c)	Using the information obtained in b), state some interesting observations.
    #It appears most Private colleges have a lower number of full-time undergraduates,
    #especially those with a lower out of state tuition.
    #Colleges with higher out of state tuition tend to have less full-time
    #undergraduates.
    #Colleges with a higher percentage of faculty with PhDs tend to have
    #higher graduation rates.
    #Public schools appear to have more faculty with PhDs, but tend to have
    #lower graduation rates.

#d)	Create a logistic regression model on Private versus the 4 other variables as predictors. 
#   Create a summary on the model. Are all predictors useful? Would you be inclined to remove one, and if so, which one?
gmod=glm(Private~F.Undergrad+Outstate+PhD+Grad.Rate,
         College,family=binomial)
summary(gmod)
#Remove Grad.Rate since its p-value is greater than .05.

#e)	Assume a college has the same out of state tuition, number of faculty with PhDs, 
#   and graduation rate as another, but more full-time undergraduates. Does its 
#   probability of being a private school increase or decrease?
coef(gmod)
head(College$Private)
#Decrease since the coefficient of F.Undergrad is negative.

#f)	What is the 95% CI for the coefficient of the number of faculty with PhDs?
confint(gmod)[4,]
#(-.097,-.040)

#g)	Calculate the error rate, the sensitivity, the specificity, the false 
#   positive and the false negative rates of the model. Explain what each 
#   of them means in the context of this problem.
(er=errorRate(gmod))
#Error rate is 5.28%, this is the percentage of colleges misclassified
#in the training data set.
er$m[2,2]/(er$m[1,2]+er$m[2,2])
#549/(549+16)
#Sensitivity is 97.2%; this means that we correctly identified 97.2%
#of private schools in the training data set.
er$m[1,1]/(er$m[1,1]+er$m[2,1])
#187/(187+25)
#specificity is 88.2%; this means that we correctly identified 88.2%
#of public schools in the training data set.
er$m[2,1]/(er$m[1,1]+er$m[2,1])
#25/(187+25)
#False positive rate is 11.8%; this means that 11.8% of public schools
#are misclassified as private schools in the training data set.
er$m[1,2]/(er$m[1,2]+er$m[2,2])
#16/(549+16)
#False negative rate is 2.8%; this means that 2.8% of private schools
#are misclassified as public schools in the training data set.

#h)	Create another model like the first, but without the PhD and Grad.Rate 
#   predictors. Give it a different name. Show its summary. 
#   Compare the error rates; which one has a better error rate?
gmod2=update(gmod,~.-PhD-Grad.Rate)
summary(gmod2)
errorRate(gmod2)
errorRate(gmod)
#The original model has a better error rate as it is 5.28% rather than 6.3%.

#i)	Create side-by-side (or one above the other) ROC plots for each model. 
#   Which one is better? What does that mean in the context of the current situation? 
#   Does it make a big difference? Change the plot window setting back to its default.
par(mfrow=c(2,1))
ROC(gmod)
ROC(gmod2)
par(parSave)
#The first ROC plot is better since it has a larger AUC, but it is not
#a huge difference.
#A larger AUC means it is better at identifying what type of college
#we are dealing with. Note that these are ROC plots for the training data
#so this may not hold in the population.

#j) For the second model, plot the decision boundary between the two predictors. 
#   Create predictions for a spot on either side of the line to see where the probability 
#   is less and where it is greater than .5. Then add some text to the plot to show that. 
#   What does this plot tell us? Finally, add the points, colored by type of college as in part b), and a legend.
(beta=coef(gmod2))
#Note that undergrad is x2 and Outstate is x3
summary(College[c(7,9)])

undergrad=100:32000
outstate=-beta[1]/beta[3]-beta[2]*undergrad/beta[3]
#Bayes Decision Boundary
plot(outstate~undergrad,type='l')  
nw=data.frame(F.Undergrad=c(10000,20000),Outstate=c(25000,10000))
nw;predict(gmod2,nw,type="r")
text(c(10000,27000),c(25000,20000),c("P(private)>.5","P(private)<.5"))
#The probability of a college being private is higher for colleges
#with a higher out of state tuition for the schools with the
#same number of full-time undergraduates.
#Also, the probability of a college being private is lower for colleges
#with a higher number of undergraduates when they have the same
#out of state tuition.
points(Outstate~F.Undergrad,College,col=(2:3)[Private],pch=20,cex=.8)
legend("top",c("Public","Private"),pch=20,pt.cex=.8,col=2:3,title="College")

#k)	Note that we have worked with training data only. 
#   If we wanted to see how the models do on data that was not used to create the models, 
#   but we only have the current data set, what could we do?
        #We could split the data in two, creating a training set and a testing set. Use
        #the training set to create the model, and the testing set to assess it.
