library(matlib)
library(smallstuff)
library(tidyverse)
library(ggpubr)
library(ISLR)
library(broom)
source("smallstuff2.R")

data(Theoph)
?Theoph
summary(Theoph)
head(Theoph)
#dim(Theoph)      #132 rows of 5 columns
nrow(Theoph)     #number of rows(observations): 132
ncol(Theoph)     #number of columns(variables): 5
colnames(Theoph)

#linear model with all predictors in the data set (4 plots)
plot(conc~.,Theoph,pch=".",col=2,main="Pharmacokinetics of Theophyllines",cex=2) 
class(Theoph$conc)          #"numeric"
class(Theoph$Time)          #"numeric"
class(Theoph$Subject)       #"ordered"  "factor"

plot(conc~Time+Subject,Theoph,pch=".",col=2,main="Pharmacokinetics of Theophyllines",cex=2)
plot(conc~Time+Wt,Theoph,pch=".",col=2,main="Pharmacokinetics of Theophyllines",cex=2)
plot(conc~Time+Dose,Theoph,pch=".",col=2,main="Pharmacokinetics of Theophyllines",cex=2)
plot(conc~Dose,Theoph,pch=".",col=2,main="Pharmacokinetics of Theophyllines",cex=2)

lmod=lm(conc~Dose,Theoph)         #Linear model
abline(lmod)                      #Add the fitted values versus the predictor
(beta=coef(lmod))                 #beta-hat: (intercept)=5.7013472  Time -0.1256896 
#f^(x)=f^(X2)=5.70-0.13X2

#create a new data set with only two variables and
summary(Theoph[,c(1,4,5)])
Theoph2=Theoph[complete.cases(Theoph[,c(1,4,5)]),c(1,4,5)]
head(Theoph2)
(n=nrow(Theoph2))     #keeping the remainder of the observations: 132

summary(Theoph[,c(4,5)])
Theoph3=Theoph[complete.cases(Theoph[,c(4,5)]),c(4,5)]
head(Theoph3)
(n=nrow(Theoph3)) 

set.seed(1)
time.c=scale(Theoph2$Time,center=TRUE,scale=FALSE)
time.c=as.data.frame(time.c)
names(time.c)[1]="time.c"
Theoph2=cbind(Theoph2,time.c)
mod1=lm(conc~time.c, data=Theoph2)
summary(mod)  #recommend this model? look at the p-value: 0.0004019 lower than 0.05
               # Also multiple R^2 is 0.09217 (9.2%) better than the one-model
               #intercept=4.9604  Time=-0.1257
               #Residual Standard Error (RSE): 2.742
summary(lmod)
(MSE=deviance(lmod)/n)
(MSE=deviance(mod)/n)
plot(conc~time.c,Theoph2,xlab="Time Since Administered (hrs)",ylab="Theophylline Concentration (mg/L)", main="Pharmacokinetics of Theophylline Dataset")
abline(lmod2,col=2)
coplot(conc~Time|Subject,Theoph2,main="Pharmacokinetics of Theophyllines",show.given = FALSE)

(MSE=anova(lmod)/n)
(MSE=anova(mod)/n)

par(mfrow = c(2,2))  #diagnostic
plot(mod)

cor(Theoph2$conc,Theoph2$Time)     #correlation coefficient: -0.3036008
(MSE=deviance(lmod)/n)             #estimated MSE(mean of square residuals): 7.407171
mod=lm(conc~Time,data=Theoph2)
summary(mod)
summary(mod)$sigma                 #2.742467=RSE
plot(resid(lmod)~fitted(lmod),main="Pharmacokinetics of Theophyllines")     #residual plot
abline(h=0,col="blue")
