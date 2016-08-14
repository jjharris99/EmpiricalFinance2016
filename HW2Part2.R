#rm(list=ls())
#basic setup
library(openxlsx)
options(stringsAsFactors=FALSE)

#set the directory to read from
workdir="/Users/jonharris/Google Drive2/EDHEC/Courses/ReneEmpirical/Assign2/"

fof=read.xlsx(paste(c(workdir,"FoF.xlsx"),collapse=""),detectDates=T)
factors=read.xlsx(paste(c(workdir,"Factors(1).xlsx"),collapse=""),startRow=5,detectDates=T)
crsp=read.xlsx(paste(c(workdir,"CRSPzero_yields.xlsx"),collapse=""),detectDates=T)

#align factors and tbill to fof
fof2factorsmatch=match(fof$Date,factors$Date)
fof2crspmatch=match(fof$Date,crsp$qdate)
fof2crspmatch[which(fof2crspmatch==min(fof2crspmatch,na.rm=T)):which(fof2crspmatch==max(fof2crspmatch,na.rm=T))]=min(fof2crspmatch,na.rm=T):max(fof2crspmatch,na.rm=T) #repair match errors due to slightly different month end format
foffactors=factors[fof2factorsmatch,2:ncol(factors)]
fofcrsp=crsp[fof2crspmatch,2:ncol(crsp)]

#select funds with 5 years of performance: 1) in line with BSW, 2) to review funds exposed to a variety of market environments

coefs=as.data.frame(NULL)
tstats=as.data.frame(NULL)
pvals=as.data.frame(NULL)
# fcol=15
for (fcol in 2:ncol(fof)) {
  rexc=fof[,fcol]-fofcrsp$yldave1mth
  validt=which(!is.na(rexc))
  if (length(validt)>=36) {
    fit=lm(rexc[validt] ~ foffactors[validt,1]+foffactors[validt,2]+foffactors[validt,8]+foffactors[validt,3]+foffactors[validt,4]+foffactors[validt,5]+foffactors[validt,6]+foffactors[validt,7])
    coefs=rbind(coefs,fit$coefficients)
    tstats=rbind(tstats,summary(fit)$coefficients[,3])
    pvals=rbind(pvals,summary(fit)$coefficients[,4])
  }
}
names(coefs)=names(fit$coefficients)
names(tstats)=names(fit$coefficients)
names(pvals)=names(fit$coefficients)
mean(coefs$'(Intercept)')
min(coefs$'(Intercept)')
max(coefs$'(Intercept)')
hist(coefs$'(Intercept)',50)

summary(fit)$coefficients[,4]
names(summary(fit))

WhatM <-function(lambda,values) {
  return(length(which(values>lambda))/length(values))
}
getpihat <-function(lambda,values) {
  return(WhatM(lambda,values)/(1-lambda))
}

minlambda=0.1
maxlambda=0.9
testlambdas=seq(from=minlambda,to=maxlambda,by=0.05)
pihat0=sapply(testlambdas,getpihat,pvals[,1])
pihat0b=NULL
for (b in 1:10000) {
  pihat0b=rbind(pihat0b,sapply(testlambdas,getpihat,sample(x=pvals[,1],size=length(pvals[,1]),replace=T)))
}
MSElambda=colSums((pihat0b-pihat0)^2)/nrow(pihat0b)
plot(MSElambda)
lambdastar=testlambdas[which(MSElambda==min(MSElambda))]
pihat=getpihat(lambdastar,pvals[,1])
  
# Calculating gamma * via the bootstrapping methodology for pi_negative
getSgamma<-function(gamma,dir,values){
  thres=qt(gamma/2,9999)
  return(length(which(values< -dir*thres))/length(values))
}
getTgamma<-function(gamma,pihat,dir,values){
  return(getSgamma(gamma,dir,values)-pihat*gamma/2)
}

testgammas=seq(from=0.3,to=0.5,by=0.05)
sapply(testgammas,getTgamma,pihat,tstats[,1])

mingamma=0.3
maxgamma=0.5
testgammas=seq(from=mingamma,to=maxgamma,by=0.05)
dir=-1
piA0=sapply(testgammas,getTgamma,pihat,dir,tstats[,1])
piA0b=NULL
for (b in 1:1000) {
  piA0b=rbind(piA0b,sapply(testgammas,getTgamma,pihat,dir,sample(x=tstats[,1],size=length(tstats[,1]),replace=T)))
}
MSEgamma=colSums((piA0b-piA0)^2)/nrow(piA0b)
plot(MSEgamma)
gammastar=testgammas[which(MSEgamma==min(MSEgamma))]
piAneg=getTgamma(gammastar,pihat,dir,pvals[,1])

dir=1
piA0=sapply(testgammas,getTgamma,pihat,dir,tstats[,1])
piA0b=NULL
for (b in 1:1000) {
  piA0b=rbind(piA0b,sapply(testgammas,getTgamma,pihat,dir,sample(x=tstats[,1],size=length(tstats[,1]),replace=T)))
}
MSEgamma=colSums((piA0b-piA0)^2)/nrow(piA0b)
plot(MSEgamma)
gammastar=testgammas[which(MSEgamma==min(MSEgamma))]
piApos=getTgamma(gammastar,pihat,dir,pvals[,1])


#Compile a table for multiple points of gamma with MSE numbers Sensitivity to certain parameters Ferson and Chan need to understand method and code it up Concluding thoughts: Are factors really betas? Is alpha really alpha? Tstat of alpha says more about consistances than actual outperformance x--AHL is listed multiple times for the same fund but different currency share classes, I suspect the original data is not as clean as it could be FC use only 12 month window for HF which is far too short as it doesn't cover the business cycle Best info is how much type you should be spending on DD. If there are not many bad funds but some zero alpha funds. x--Multiple share classes of same funds. Different currency classes can have different alpha if they don't hedge This is only slightly relevant for FoF and not in the spirit of EDHEC RI which calls for customized investment solutions for each investor which I would agree is more inline in traditional investment theory (endogenous utility functions). Parsimony question