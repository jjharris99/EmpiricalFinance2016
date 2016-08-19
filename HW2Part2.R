#rm(list=ls())
#basic setup
library(openxlsx)
library(quadprog)
library(sandwich)
library(lmtest)
options(stringsAsFactors=FALSE)

#set the directory to read from
workdir="/Users/jonharris/Google Drive2/EDHEC/Courses/ReneEmpirical/Assign2/EmpiricalFinance2016/"

fof=read.xlsx(paste(c(workdir,"FoF.xlsx"),collapse=""),detectDates=T)
factors=read.xlsx(paste(c(workdir,"Factors(1).xlsx"),collapse=""),startRow=5,detectDates=T)
crsp=read.xlsx(paste(c(workdir,"CRSPzero_yields.xlsx"),collapse=""),detectDates=T)
rf=read.xlsx(paste(c(workdir,"RFR_Homework2.xlsx"),collapse=""),detectDates=T)

#align factors and tbill to fof
fof2factorsmatch=match(fof$Date,factors$Date)
fof2rfmatch=match(fof$Date,rf$Date)
fof2crspmatch=match(fof$Date,crsp$qdate)
fof2crspmatch[which(fof2crspmatch==min(fof2crspmatch,na.rm=T)):which(fof2crspmatch==max(fof2crspmatch,na.rm=T))]=min(fof2crspmatch,na.rm=T):max(fof2crspmatch,na.rm=T) #repair match errors due to slightly different month end format
foffactors=factors[fof2factorsmatch,2:ncol(factors)]
fofcrsp=crsp[fof2crspmatch,2:ncol(crsp)]
fofrf=rf[fof2rfmatch,2:ncol(rf)]

plot(rowMeans(fof[,2:ncol(fof)],na.rm=T))

View(cbind(fofrf,fofcrsp$yldave1mth/100/12))
#select funds with 5 years of performance: 1) in line with BSW, 2) to review funds exposed to a variety of market environments

resids=as.data.frame(NULL)
coefs=as.data.frame(NULL)
tstats=as.data.frame(NULL)
serrs=as.data.frame(NULL)
pvals=as.data.frame(NULL)
pvals2=as.data.frame(NULL)
tis=rep(NA,1000)
# fcol=15
for (fcol in 2:6393) {#ncol(fof)) {
  rexc=fof[,fcol]-fofrf
  validt=which(!is.na(rexc))
  if (length(validt)>=36) {
    fit=lm(rexc[validt] ~ foffactors[validt,1]+foffactors[validt,2]+foffactors[validt,8]+foffactors[validt,3]+foffactors[validt,4]+foffactors[validt,5]+foffactors[validt,6]+foffactors[validt,7])

    for (ii in 1:1000) {
#       validti=sample(validt,length(validt),replace=T)
#       fiti=lm(rexc[validti] ~ foffactors[validti,1]+foffactors[validti,2]+foffactors[validti,8]+foffactors[validti,3]+foffactors[validti,4]+foffactors[validti,5]+foffactors[validti,6]+foffactors[validti,7])
#       tis[ii]=unclass(coeftest(fiti, vcov. = NeweyWest))[1,3] #Newey-West adjustment
      validti=sample(residf,length(residf),replace=T)
      tis[ii]=mean(validti)/sd(validti) #Newey-West adjustment      
    }

    residf=fit$residuals
    resids=rbind(resids,residf)
    fitstats=unclass(coeftest(fit, vcov. = NeweyWest)) #Newey-West adjustment
    coefs=rbind(coefs,fit$coefficients)
    tstats=rbind(tstats,fitstats[,3])
    serrs=rbind(serrs,fitstats[,2])
    pvalf=fitstats[,4]
    simresid=sample(residf,1000,replace=T)
    pvalf[1]=2*min(length(which(tis<fitstats[1,3])),length(which(tis>fitstats[1,3])))/length(tis)
#       2*min(length(which(simresid/fitstats[1,2]<fitstats[1,3])),length(which(simresid/fitstats[1,2]>fitstats[1,3])))/length(simresid)
    pvals=rbind(pvals,pvalf)
#     coefs=rbind(coefs,fit$coefficients)
#     tstats=rbind(tstats,summary(fit)$coefficients[,3])
#     pvals2=rbind(pvals2,summary(fit)$coefficients[,4])
  }
}
hist(pvals[,1],50,)
# hist(pvals2[,1],50)
# pvals=pvals2
  
names(coefs)=names(fit$coefficients)
names(tstats)=names(fit$coefficients)
names(pvals)=names(fit$coefficients)
mean(coefs$'(Intercept)')
min(coefs$'(Intercept)')
max(coefs$'(Intercept)')
hist(coefs$'(Intercept)',50)

# View(cbind(coefs[,1],apply(resids,2,min),apply(resids,2,mean),apply(resids,2,max),tstats[,1],serrs[,1],pvals[,1],pvals2[,1]))

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
for (b in 1:1000) {
  pihat0b=rbind(pihat0b,sapply(testlambdas,getpihat,sample(x=pvals[,1],size=length(pvals[,1]),replace=T)))
}
MSElambda=colSums((pihat0b-min(pihat0))^2)/nrow(pihat0b) #NOT SURE why min() needed here, but it is in the paper. any idea?
plot(testlambdas,MSElambda)
lambdastar=testlambdas[which(MSElambda==min(MSElambda))][1]
pihat=getpihat(lambdastar,pvals[,1])
pihat

# Calculating gamma * via the bootstrapping methodology for pi_negative
getSgamma<-function(gamma,dir,tvals,pvals){
  thres=gamma/2
  return(length(which(pvals<thres & dir*tvals>0))/length(tvals))
}
getTgamma<-function(gamma,pihat,dir,svalues){
  tvals=tstats[svalues,1]
  pvals=pvals[svalues,1]
  return(getSgamma(gamma,dir,tvals,pvals)-pihat*gamma/2)
}

testgammas=seq(from=0.3,to=0.5,by=0.05)
sapply(testgammas,getTgamma,pihat,1,1:length(tstats[,1]))

mingamma=0.3
maxgamma=0.5
testgammas=seq(from=mingamma,to=maxgamma,by=0.05)
dir=-1
piA0=sapply(testgammas,getTgamma,pihat,dir,1:length(tstats[,1]))
piA0b=NULL
for (b in 1:1000) {
  piA0b=rbind(piA0b,sapply(testgammas,getTgamma,pihat,dir,sample(x=1:length(pvals[,1]),size=length(pvals[,1]),replace=T)))
}
MSEgamma=colSums((piA0b-min(piA0))^2)/nrow(piA0b) #NOT SURE why min() needed here, but it is in the paper. any idea?
plot(MSEgamma)
minMSEneg=min(MSEgamma)
gammastarneg=testgammas[which(MSEgamma==minMSEneg)][1]
gammastarneg

dir=1
piA0=sapply(testgammas,getTgamma,pihat,dir,1:length(tstats[,1]))
piA0b=NULL
for (b in 1:1000) {
  piA0b=rbind(piA0b,sapply(testgammas,getTgamma,pihat,dir,sample(x=1:length(pvals[,1]),,size=length(pvals[,1]),replace=T)))
}
MSEgamma=colSums((piA0b-max(piA0))^2)/nrow(piA0b) #NOT SURE why max() needed here, but it is in the paper. any idea?
plot(MSEgamma)
minMSEpos=min(MSEgamma)
gammastarpos=testgammas[which(MSEgamma==minMSEpos)]

if (minMSEneg<minMSEpos) {
  gammastar=gammastarneg
  piAneg=getTgamma(gammastar,pihat,-1,1:length(pvals[,1]))
  piApos=1-piAneg-pihat
} else {
  gammastar=gammastarpos
  piApos=getTgamma(gammastar,pihat,1,1:length(pvals[,1]))
  piAneg=1-piApos-pihat
}

#results
lambdastar
gammastar
piAneg
pihat
piApos

################################
#Ferson & Chen
################################
nsim=1000
gamma=2*0.1 #F&C use 2*0.1, BSW use 2*0.3
alphabtestvals=seq(-0.85,-0.05,0.2)
alphagtestvals=seq(0.05,0.85,0.2)
#loop over grid of alpha values to test for best pair
pchi2s=matrix(nrow=length(alphabtestvals),ncol=length(alphagtestvals))
pibhats=matrix(nrow=length(alphabtestvals),ncol=length(alphagtestvals))
pighats=matrix(nrow=length(alphabtestvals),ncol=length(alphagtestvals))
for (bb in 1:length(alphabtestvals)) {
  for (gg in 1:length(alphagtestvals)) {
  set.seed(1)
  alphab=alphabtestvals[bb]
  alphag=alphagtestvals[gg]
  # alphab=-0.1
  # alphag=0.1
  
  simalpha1=sample(x=tstats[,1],size=nsim,replace=T) #NOT SURE how the simulation is supposed to be done
  # tg=tratio above which 10% of the simulated tstats lie within the null of zero alphas
  tg=quantile(simalpha1,0.9)
  # tb=value below which 10% of the sim tstats lie under the null hypothesis of zero alphas
  tb=quantile(simalpha1,0.1)
  # hist(tstats[,1])
  simalpha2=sample(x=tstats[,1],size=nsim,replace=T)+alphag #NOT SURE how the simulation is supposed to be done
  # betag=fraction of simulated trats above tg
  betag=length(which(simalpha2>tg))/nsim
  # deltab=fraction of simulated trats below tb (empirical estimate of prob of rejecting the null in favor of finding a bad fund when the fund is actually goog)
  deltab=length(which(simalpha2<tb))/nsim
  simalpha3=sample(x=tstats[,1],size=nsim,replace=T)+alphab #NOT SURE how the simulation is supposed to be done
  # betab=fraction of simulated trats below tb
  betab=length(which(simalpha3<tb))/nsim
  # deltag=fraction of simulated trats above tg 
  deltag=length(which(simalpha3>tg))/nsim
  # Fb=fraction of rejections of the null hypothesis in the actual data for tb
  Fb=length(which(tstats[,1]<tb))/length(tstats[,1])
  # Fg=fraction of rejections of the null hypo in the actual data for tg
  Fg=length(which(tstats[,1]>tg))/length(tstats[,1])
  
  # two equations in two unknowns
  # Fg=(gamma/2)*pi0+deltag*pib+betag*pig=(gamma/2)+(deltag-(gamma/2))*pib+(betag-(gamma/2))*pig
  # Fb=(gamma/2)*pi0+betab*pib+deltab*pig=(gamma/2)+(betab-(gamma/2))*pib+(deltab-(gamma/2))*pig
  # pi0=1-pib-pig
  # pib>=0
  # pig>=0
  # pib+pig<=1
  # 0=(deltag-(gamma/2))*pib+(betag-(gamma/2))*pig+(gamma/2)-Fg=(betab-(gamma/2))*pib+(deltab-(gamma/2))*pig+(gamma/2)-Fb
  # minimize difference -> minimize sum of squares of above two equations -> quadratic system with below parameterisation
  Dmat=rbind(c((deltag-(gamma/2))^2+(betab-(gamma/2))^2,(deltab-(gamma/2))*(betab-(gamma/2))+(deltag-(gamma/2))*(betag-(gamma/2))),
    c((deltab-(gamma/2))*(betab-(gamma/2))+(deltag-(gamma/2))*(betag-(gamma/2)),(betag-(gamma/2))^2+(deltab-(gamma/2))^2))
  dvec=-c((deltag-(gamma/2))*((gamma/2)-Fg)+(betab-(gamma/2))*((gamma/2)-Fb),(betag-(gamma/2))*((gamma/2)-Fg)+(deltab-(gamma/2))*((gamma/2)-Fb))
  Amat=t(rbind(c(1,0),c(0,1),-c(1,1)))
  bvec=c(0,0,-1)
  qsol=solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=0)
  pibhat=qsol$solution[1]
  pighat=qsol$solution[2]
  
  #cell boundaries set so that approx equal number of tratios appear in each cell in original data
  #e.g. N/100 per cell
  K=100 #cells
  binbounds=quantile(tstats[,1],c(seq(from=0,to=1,by=1/K)))
  simtypes=(sample(c(1,2,3),nsim,p=c(max(0,pibhat),max(0,1-pibhat-pighat),min(pighat,1-pibhat)),replace=T))
  simalpha4=c(rnorm(n=length(which(simtypes==1)),mean=alphab,sd=1),rnorm(n=length(which(simtypes==2)),mean=0,sd=1),rnorm(n=length(which(simtypes==3)),mean=alphag,sd=1))
  # oi=freq of tstats for alpha in cell i in original data
  # mi=freq of tstats for alpha in cell i in model data
  Obincounts=sapply(1:K,function(x){length(which(tstats[,1]>binbounds[x]))})-sapply(1:K,function(x){length(which(tstats[,1]>binbounds[x+1]))})
  Mbincounts=sapply(1:K,function(x){length(which(simalpha4>binbounds[x]))})-sapply(1:K,function(x){length(which(simalpha4>binbounds[x+1]))})
  
  # pchi2=sum((oi-mi)^2/oi)
  pchi2=sum((Obincounts-Mbincounts)^2/Obincounts)
  pchi2s[bb,gg]=pchi2
  pibhats[bb,gg]=pibhat
  pighats[bb,gg]=pighat
  }
}

#results
minind=which(pchi2s==min(pchi2s),arr.ind=T)
alphabtestvals[minind[1]]
alphagtestvals[minind[2]]
pibhats[minind]
pighats[minind]

#BSW, occurs when betag=betab=1 and deltag=deltab=0
#pi0BSW=[1-(Fb+Fg)]/(1-gamma); pigBSW=Fg-(gamma/2)*pi0BSW



