library(matlib)
library(smallstuff)
library(ISLR)
library(MASS)
library(ROCR)
library(class)
source("smallstuff2.R")

###################################################################################
#
# smallstuff2.R
#
###################################################################################
# Internal Functions
###################################################################################
errorRate<-function(gmod,nw=NULL,p=.5) {
  if (class(gmod)[1]!="glm") stop("Argument must be a logistic regression model")
  if (as.character(gmod$call)[3]!="binomial") stop("Model must be binomial")
  #Testing error rate
  if (!is.null(nw)) {
    if (class(nw)[1]!="data.frame") stop("nw must be a data frame")
    response=strsplit(as.character(gmod$call)[2]," ")[[1]][1]
    if (!(response %in% colnames(nw))) stop("nw must contain the response variable")
    prob=predict(gmod,nw,"r")   #Responses from the test set
    m=nrow(nw)
    yhat=rep(levels(gmod$model[,1])[1],m)
    yhat[prob>p]=levels(gmod$model[,1])[2]
    #    return(list(errorRate=eval(parse(text=paste0("mean(yhat!=nw$",response,")*100"))),
    #                m=eval(parse(text=paste0("table(yhat,response=nw$",response,")*100/m")))))
    return(list(errorRate=eval(parse(text=paste0("mean(yhat!=nw$",response,")*100"))),
                m=eval(parse(text=paste0("table(yhat,response=nw$",response,")")))))
  }
  #Training error rate
  n=nrow(gmod$model)
  yhat=rep(levels(gmod$model[,1])[1],n)
  yhat[fitted(gmod)>p]=levels(gmod$model[,1])[2]
  #  list(errorRate=mean(yhat!=gmod$model[,1])*100,
  #       m=table(yhat,response=gmod$model[,1])*100/n)
  list(errorRate=mean(yhat!=gmod$model[,1])*100,
       m=table(yhat,response=gmod$model[,1]))
}
ROC<-function(mod,nw=NULL) {
  if (!inherits(mod,"glm")&&!inherits(mod,"lda")&&!inherits(mod,"qda")) {
    stop("Not a supported model")
  }
  if (!is.null(nw)) {
    if (class(nw)[1]!="data.frame") stop("nw must be a data frame")
    resp=strsplit(as.character(mod$call)[2]," ")[[1]][1]
    if (!(resp %in% colnames(nw))) stop("nw must contain the response variable")
    response=eval(parse(text=paste0("nw$",resp)))
  }
  require("ROCR")
  if (inherits(mod,"glm")) {
    if (is.null(nw)) {
      prob=fitted(mod)
      response=factor(model.frame(mod)[,1],ordered=T)
    } else prob=predict(mod,nw,"r")
  } else {
    if (is.null(nw)) {
      prob=predict(mod)$posterior[,2]
      response=factor(model.frame(mod)[,1],ordered=T)
    } else prob=predict(mod,nw)$posterior[,2]
  }
  pr=prediction(prob,response)
  plot(performance(pr,"tpr","fpr"),
       main=c(paste("ROC curve for",class(mod)[1],"on",mod$call$data),
              deparse(mod$call[[2]])))
  auc=performance(pr,"auc")@y.values[[1]]
  text(.5,.5,paste0("AUC = ",round2(auc,3)))
}
ROCknn<-function(mod,response) {
  if (!inherits(mod,"factor")) stop("Not a supported model")
  if (is.null(attributes(mod)$prob)) stop("No probabilities")
  if (length(levels(mod))!=2) stop("Must have 2 levels")
  require("ROCR")
  prob=attributes(mod)$prob
  prob[mod==levels(mod)[1]]=1-prob[mod==levels(mod)[1]]
  pr=prediction(prob,factor(response,ordered=T))
  plot(performance(pr,"tpr","fpr"),main="ROC curve for KNN")
  auc=performance(pr,"auc")@y.values[[1]]
  text(.5,.5,paste0("AUC = ",round2(auc,3)))
}
CVerror<-function(mod, k=nrow(model.frame(mod))) {
  if (!inherits(mod,"glm")&&!inherits(mod,"lda")&&!inherits(mod,"qda")) {
    stop("Not a supported model")
  }
  require(pryr)
  dat=model.frame(mod)
  n=nrow(dat)
  split=sample(rep(1:k,length.out=n))
  if (!inherits(mod,"glm")) {
    resp=eval(parse(text=paste0(as.character(mod$call[3]),"$",
                                strsplit(as.character(mod$call)[2]," ")[[1]][1])))
  }
  errk=NULL
  for (i in 1:k) {
    mod2=eval(modify_call(mod$call,list(subset=which(split!=i))))
    if (inherits(mod,"glm")) {
      errk[i]=errorRate(mod2,dat[split==i,])$errorRate 
    } else {
      errk[i]=mean(predict(mod2,dat[split==i,])$class!=resp[split==i])*100
    }
  }
  errk
}
CVerrorknn<-function(pred,resp,K=1,k=nrow(pred)) {
  n=nrow(pred)
  split=sample(rep(1:k,length.out=n))
  errk=NULL
  for (i in 1:k) {
    kk=knn(pred[split!=i,,drop=F],pred[split==i,,drop=F],resp[split!=i],K)
    errk[i]=mean(kk!=resp[split==i])*100
  }
  errk
}
lmSub<-function(x,...) {
  UseMethod("lmSub",x)
}
lmSub.regsubsets<-function(object,d) {
  response=as.character(object$call[[2]][[2]])
  fr=as.formula(object$call[[2]])
  dat=eval(object$call[[3]])
  subsum=summary(object)
  dataSet=cbind(dat[,response,drop=F], model.matrix(fr,dat)[,-1][,subsum$which[d,-1],drop=F])
  form=formula(paste(response,"~."))
  lm(form,dataSet)
}
predict.regsubsets<-function(object,d,newdata) {
  fr=as.formula(object$call[[2]])
  m=model.matrix(fr,newdata)
  beta=coef(object,d)
  m[,names(beta)]%*%beta
}
dataSet<-function(formula,data,subset=NULL) {
  if (!inherits(formula,"formula")) stop("Formula must be a formula")
  if (!inherits(data,"data.frame")) stop("Data must be a data frame")
  response=as.character(formula[[2]])
  cols=c(response,attributes(terms(formula,data=data))$term.labels)
  if (is.null(subset)) data[,cols] else data[subset,cols]
}

