#rm(list=ls())
#basic setup
library(openxlsx)
library(quadprog)
library(sandwich)
library(lmtest)
#Load the libraries required to do parallel processing
library("foreach")
library("parallel")
library("doParallel")
options(stringsAsFactors=FALSE)

#set the directory to read from
# workdir="/Users/jonharris/Google Drive2/EDHEC/Courses/ReneEmpirical/Assign2/EmpiricalFinance2016/"
workdir="C:/Users/jonat/Documents/GitHub/EmpiricalFinance2016/"

fof=read.xlsx(paste(c(workdir,"FoF.xlsx"),collapse=""),detectDates=T)
factors=read.xlsx(paste(c(workdir,"Factors(1).xlsx"),collapse=""),startRow=5,detectDates=T)
crsp=read.xlsx(paste(c(workdir,"CRSPzero_yields.xlsx"),collapse=""),detectDates=T)
rf=read.xlsx(paste(c(workdir,"RFR_Homework2.xlsx"),collapse=""),detectDates=T) #read in our download of risk free rate info form French website

#align factors and tbill to fof
fof2factorsmatch=match(fof$Date,factors$Date)
fof2rfmatch=match(fof$Date,rf$Date)
fof2crspmatch=match(fof$Date,crsp$qdate)
fof2crspmatch[which(fof2crspmatch==min(fof2crspmatch,na.rm=T)):which(fof2crspmatch==max(fof2crspmatch,na.rm=T))]=min(fof2crspmatch,na.rm=T):max(fof2crspmatch,na.rm=T) #repair match errors due to slightly different month end format
foffactors=factors[fof2factorsmatch,2:ncol(factors)]
fofcrsp=crsp[fof2crspmatch,2:ncol(crsp)]
fofrf=rf[fof2rfmatch,2:ncol(rf)]

plot(rowMeans(fof[,2:ncol(fof)],na.rm=T))

View(cbind(fof[,1:5],fofrf,fofcrsp,foffactors))
#select funds with 5 years of performance: 1) in line with BSW, 2) to review funds exposed to a variety of market environments

resids=NULL
coefs=as.data.frame(NULL)
tstats=as.data.frame(NULL)
serrs=as.data.frame(NULL)
pvals0=as.data.frame(NULL)
for (fcol in 2:ncol(fof)) {
  rexc=fof[,fcol]-fofrf
  validt=which(!is.na(rexc))
  if (length(validt)>=60) {
    residf=rep(NA,nrow(fof))
    fit=lm(rexc[validt] ~ foffactors[validt,1]+foffactors[validt,2]+foffactors[validt,8]+foffactors[validt,3]+foffactors[validt,4]+foffactors[validt,5]+foffactors[validt,6]+foffactors[validt,7])
    residf[validt]=fit$residuals
    resids=cbind(resids,residf)
    fitstats=unclass(coeftest(fit, vcov. = NeweyWest)) #Newey-West adjustment
    coefs=rbind(coefs,fit$coefficients)
    tstats=rbind(tstats,fitstats[,3])
    serrs=rbind(serrs,fitstats[,2])
    pvals0=rbind(pvals0,summary(fit)$coefficients[,4])
  }
}
hist(pvals0[,1],50)
names(coefs)=names(fit$coefficients)
names(tstats)=names(fit$coefficients)
names(pvals0)=names(fit$coefficients)

# #setup parallel cores
# if (!exists("cl")) {
#   detectCores()
#   numcores=4                       #number of parallel processes to run
#   cl <- makeCluster(numcores)               #setup the processes
#   registerDoParallel(cl, cores = numcores)  #register the processes to be used
#   setDefaultCluster(cl)
# }
# # stopCluster(cl)
#
# #bootstrap simulate returns in order to get simulated Tstats to use to calculate pvalues
# M=1000
# factorexplainedreturns=as.matrix(foffactors)%*%t(coefs[,2:ncol(coefs)])
# dim(factorexplainedreturns)
# res <- foreach(mm=1:M,.inorder=F,.packages=c('sandwich','lmtest'),.combine='rbind')%dopar%{
#   tis=rep(NA,ncol(resids))
#   for (cc in 1:ncol(resids)) {
#     validt=which(!is.na(resids[,cc]))
#     validti=sample(validt,length(validt),replace=T)
#     ri=resids[validti,cc]+factorexplainedreturns[validt,cc]
#     fiti=lm(ri ~ foffactors[validti,1]+foffactors[validti,2]+foffactors[validti,8]+foffactors[validti,3]+foffactors[validti,4]+foffactors[validti,5]+foffactors[validti,6]+foffactors[validti,7])
#     tis[cc]=unclass(coeftest(fiti, vcov. = NeweyWest))[1,3]
#   }
#   tis
# }
# # save(tis,file = paste(c(workdir,"tis.rdata"),collapse=""))
# 
# # #nonparallel version
# # tis = matrix(nrow=M,ncol=ncol(resids))
# # for (mm in 1:M){
# #   if (mm %% 10 ==1) {print(mm)}
# #   for (cc in 1:ncol(resids)) {
# #     validt=which(!is.na(resids[,cc]))
# #     validti=sample(validt,length(validt),replace=T)
# #     ri=resids[validti,cc]+factorexplainedreturns[validt,cc]
# #     fiti=lm(ri ~ foffactors[validti,1]+foffactors[validti,2]+foffactors[validti,8]+foffactors[validti,3]+foffactors[validti,4]+foffactors[validti,5]+foffactors[validti,6]+foffactors[validti,7])
# #     tis[mm,cc]=unclass(coeftest(fiti, vcov. = NeweyWest))[1,3]
# #   }
# # }
# 
# #calculate pvalues from tstat sample
# pvals=rep(NA,ncol(resids))
# for (cc in 1:ncol(resids)) {
#   pvals[cc]=2*min(length(which(tis[,cc]<tstats[cc,1])),length(which(tis[,cc]>tstats[cc,1])))/nrow(tis)
# }

pvals=pvals0 #use standard pvals for robustness check

#sum results for alpha
mean(coefs$'(Intercept)')
min(coefs$'(Intercept)')
max(coefs$'(Intercept)')
hist(coefs$'(Intercept)',50)

#useful functions for pvalue fit
WhatM <-function(lambda,values) {
  return(length(which(values>lambda))/length(values))
}
getpihat <-function(lambda,values) {
  return(WhatM(lambda,values)/(1-lambda))
}

#estimate lambdastar and pihat
minlambda=0.1
maxlambda=0.9
testlambdas=seq(from=minlambda,to=maxlambda,by=0.05)
pihat0=sapply(testlambdas,getpihat,pvals[,1])
pihat0b=NULL
for (b in 1:1000) {
  pihat0b=rbind(pihat0b,sapply(testlambdas,getpihat,sample(x=pvals[,1],size=length(pvals[,1]),replace=T)))
}
MSElambda=colSums((pihat0b-min(pihat0))^2)/nrow(pihat0b) 
plot(testlambdas,MSElambda)
lambdastar=testlambdas[which(MSElambda==min(MSElambda))][1]
pihat=getpihat(lambdastar,pvals[,1])
lambdastar
pihat

# Calculating gamma * via the bootstrapping methodology
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

#estimate gamma negative
mingamma=0.3
maxgamma=0.5
testgammas=seq(from=mingamma,to=maxgamma,by=0.05)
dir=-1
piA0=sapply(testgammas,getTgamma,pihat,dir,1:length(tstats[,1]))
piA0b=NULL
for (b in 1:1000) {
  piA0b=rbind(piA0b,sapply(testgammas,getTgamma,pihat,dir,sample(x=1:length(pvals[,1]),size=length(pvals[,1]),replace=T)))
}
MSEgamma=colSums((piA0b-min(piA0))^2)/nrow(piA0b) 
plot(MSEgamma)
minMSEneg=min(MSEgamma)
gammastarneg=testgammas[which(MSEgamma==minMSEneg)][1]
gammastarneg

#estimate gamma positive
dir=1
piA0=sapply(testgammas,getTgamma,pihat,dir,1:length(tstats[,1]))
piA0b=NULL
for (b in 1:1000) {
  piA0b=rbind(piA0b,sapply(testgammas,getTgamma,pihat,dir,sample(x=1:length(pvals[,1]),,size=length(pvals[,1]),replace=T)))
}
MSEgamma=colSums((piA0b-max(piA0))^2)/nrow(piA0b) 
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
nsim=10 #F&C and Danish paper use 1000, but this takes forever, so 10 used here for homework purposes
#generate base zero-alpha simulated tstats for each fund following F&C / Fama&French approach (keeping residuals and factors from same time together, in contrast to BSW)
set.seed(1)
tis = matrix(nrow=nsim,ncol=ncol(resids))
for (mm in 1:nsim) {
  print(mm)
  cc=0
  for (fcol in 2:ncol(fof)) {
    rexc=fof[,fcol]-fofrf
    validt=which(!is.na(rexc))
    if (length(validt)>=60) {
      cc=cc+1
      validti=sample(validt,length(validt),replace=T)
      fit=lm(rexc[validti] ~ foffactors[validti,1]+foffactors[validti,2]+foffactors[validti,8]+foffactors[validti,3]+foffactors[validti,4]+foffactors[validti,5]+foffactors[validti,6]+foffactors[validti,7])
      tis[mm,cc]=unclass(coeftest(fit, vcov. = NeweyWest))[1,3]
    }
  } 
}

gamma=2*0.1 #F&C use 2*0.1, BSW use 2*0.3
alphabtestvals=seq(-0.85,-0.05,0.1)
alphagtestvals=seq(0.05,0.85,0.1)

#cell boundaries set so that approx equal number of tratios appear in each cell in original data
#e.g. N/100 per cell
K=100 #cells
nsim2=length(tstats[,1])
binbounds=quantile(tstats[,1],c(seq(from=0,to=1,by=1/K)))
Obincounts=sapply(1:K,function(x){length(which(tstats[,1]>binbounds[x]))})-sapply(1:K,function(x){length(which(tstats[,1]>binbounds[x+1]))})

#loop over grid of alpha values to test for best pair
pchi2s=matrix(nrow=length(alphabtestvals),ncol=length(alphagtestvals))
pibhats=matrix(nrow=length(alphabtestvals),ncol=length(alphagtestvals))
pighats=matrix(nrow=length(alphabtestvals),ncol=length(alphagtestvals))
for (bb in 1:length(alphabtestvals)) {
  for (gg in 1:length(alphagtestvals)) {
  alphab=alphabtestvals[bb]
  alphag=alphagtestvals[gg]
  # alphab=-0.1
  # alphag=0.1
  
  tgs=tbs=betags=betabs=deltabs=deltags=rep(NA,nsim)
  for (mm in 1:nsim) {
    simalpha1=tis[mm,] 
    # tg=tratio above which 10% of the simulated tstats lie within the null of zero alphas
    tgs[mm]=quantile(simalpha1,1-gamma/2)
    # tb=value below which 10% of the sim tstats lie under the null hypothesis of zero alphas
    tbs[mm]=quantile(simalpha1,gamma/2)
    # hist(tstats[,1])
    simalpha2=tis[mm,]+alphag 
    # betag=fraction of simulated trats above tg
    betags[mm]=length(which(simalpha2>tgs[mm]))/length(simalpha2)
    # deltab=fraction of simulated trats below tb (empirical estimate of prob of rejecting the null in favor of finding a bad fund when the fund is actually goog)
    deltabs[mm]=length(which(simalpha2<tbs[mm]))/length(simalpha2)
    simalpha3=tis[mm,]+alphab 
    # betab=fraction of simulated trats below tb
    betabs[mm]=length(which(simalpha3<tbs[mm]))/length(simalpha3)
    # deltag=fraction of simulated trats above tg 
    deltags[mm]=length(which(simalpha3>tgs[mm]))/length(simalpha3)
  }
  #use mean of simulated values (p13)
  tg=mean(tgs)
  tb=mean(tbs)
  betag=mean(betags)
  betab=mean(betabs)
  deltab=mean(deltabs)
  deltag=mean(deltags)
  
  # Fb=fraction of rejections of the null hypothesis in the actual data for tb
  Fb=length(which(tstats[,1]<tb))/length(tstats[,1])
  # Fg=fraction of rejections of the null hypo in the actual data for tg
  Fg=length(which(tstats[,1]>tg))/length(tstats[,1])
  
  #solve system of equations
  # two equations in two unknowns
  # Fg=(gamma/2)*pi0+deltag*pib+betag*pig=(gamma/2)+(deltag-(gamma/2))*pib+(betag-(gamma/2))*pig
  # Fb=(gamma/2)*pi0+betab*pib+deltab*pig=(gamma/2)+(betab-(gamma/2))*pib+(deltab-(gamma/2))*pig
  # pi0=1-pib-pig
  # pib>=0
  # pig>=0
  # pib+pig<=1
  # 0=(deltag-(gamma/2))*pib+(betag-(gamma/2))*pig+(gamma/2)-Fg=(betab-(gamma/2))*pib+(deltab-(gamma/2))*pig+(gamma/2)-Fb
  # minimize difference -> minimize sum of squares of above two equations -> quadratic system with below parameterisation
  Dmat=rbind(c(
    (deltag-(gamma/2))^2+(betab-(gamma/2))^2,
    (deltab-(gamma/2))*(betab-(gamma/2))+(deltag-(gamma/2))*(betag-(gamma/2))
    ),c(
    (deltab-(gamma/2))*(betab-(gamma/2))+(deltag-(gamma/2))*(betag-(gamma/2)),
    (betag-(gamma/2))^2+(deltab-(gamma/2))^2)
    )
  dvec=-c(
    (deltag-(gamma/2))*((gamma/2)-Fg)+(betab-(gamma/2))*((gamma/2)-Fb),
    (betag-(gamma/2))*((gamma/2)-Fg)+(deltab-(gamma/2))*((gamma/2)-Fb))
  #constraints A & b
  Amat=t(rbind(c(1,0),c(0,1),-c(1,1)))
  bvec=c(0,0,-1)
  qsol=solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=0)
  pibhat=qsol$solution[1]
  pighat=qsol$solution[2]
  
  #cell boundaries set so that approx equal number of tratios appear in each cell in original data #e.g. N/100 per cell
  simalpha4=tis[mm,]+sample(c(alphab,0,alphag),nsim2,p=c(max(0,pibhat),max(0,1-pibhat-pighat),min(pighat,1-pibhat)),replace=T)
  # oi=freq of tstats for alpha in cell i in original data
  # mi=freq of tstats for alpha in cell i in model data
  Mbincounts=sapply(1:K,function(x){length(which(simalpha4>binbounds[x]))})-sapply(1:K,function(x){length(which(simalpha4>binbounds[x+1]))})
  
  # pchi2=sum((oi-mi)^2/oi)
  pchi2=sum((Obincounts-Mbincounts)^2/Obincounts)
  pchi2s[bb,gg]=pchi2
  pibhats[bb,gg]=pibhat
  pighats[bb,gg]=pighat
  }
}

#results
minind=which(pchi2s==min(pchi2s),arr.ind=T)[1,]
alphabtestvals[minind[1]]
alphagtestvals[minind[2]]
pibhats[minind[1],minind[2]]
pighats[minind[1],minind[2]]

#BSW, occurs when betag=betab=1 and deltag=deltab=0
#pi0BSW=[1-(Fb+Fg)]/(1-gamma); pigBSW=Fg-(gamma/2)*pi0BSW



