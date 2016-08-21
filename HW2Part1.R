options(stringsAsFactors=FALSE)

#set the directory to read from
# workdir="/Users/jonharris/Google Drive2/EDHEC/Courses/ReneEmpirical/Assign2/EmpiricalFinance2016/"
workdir="C:/Users/jonat/Documents/GitHub/EmpiricalFinance2016/"

#read in data
crsp=read.xlsx(paste(c(workdir,"CRSPzero_yields.xlsx"),collapse=""),detectDates=T)
crsp[which(crsp==0,arr.ind=T)]=NA #delete 0's as these seem to be null values
crsp=crsp[which(!is.na(crsp$qdate) & !is.na(rowSums(crsp[,2:ncol(crsp)]))),] #cut out rows with missing data
gdpc=read.xlsx(paste(c(workdir,"GDPC1.xlsx"),collapse=""),detectDates=T)

#align gdp and crsp
gdpc2crspmatch=match(substr(gdpc$DATE,1,7),substr(crsp$qdate,1,7))

#setup observed factors
shortrate=crsp$yldave3mth[gdpc2crspmatch[which(!is.na(gdpc2crspmatch))]]
spread=crsp$FByield5yr[gdpc2crspmatch[which(!is.na(gdpc2crspmatch))]]-shortrate
gdp=gdpc$VALUE[which(!is.na(gdpc2crspmatch))]
ys=crsp[gdpc2crspmatch[which(!is.na(gdpc2crspmatch))],c("yldave3mth","FByield1yr","FByield2yr","FByield3yr","FByield4yr","FByield5yr")]

#Step 1

#estimate standard SUR
# install.packages("vars") #If not already installed
library(vars)
x = cbind(shortrate, spread, gdp)
plot.ts(x , main = "", xlab = "")
fit=VAR(x, p=1, type="const")
summary(fit)
fit$varresult
resid   <- residuals(fit)                                #get residuals from model
# covres   <- cov(resid)                                    #"manually" compute covmarix
sigmacovar=as.matrix(summary(fit)$covres)
sigma=t(chol(sigmacovar))
# names(summary(fit))
muphi=rbind(as.data.frame(lapply(summary(fit)$varresult,function(x){x$coefficients[,1]})))
mu=as.vector(muphi[4,])
Phi=as.matrix(t(muphi[1:3,]))
#display results
mu
Phi
sigma

#Step 2

#risk premia: lambdat=lambda0+lambda1*Xt
#model implied yields: yhatnt=an+bn*Xt
#N yields
#min(lambda0,lambda1)sum_t(1..T)sum_n(1..N)(yhatnt-ynt)^2
#m(t+1)=exp(-y1t-1/2*t(lambdat)*lambdat-t(lambdat)*epsilon(t+1))

#set base values
A1=0
B1=-c(1,0,0)

#setup functions to help with 2nd step of estimation
getABi<-function(AB1,lambda){
  A2=AB1[1]+AB1[2:4]%*%t(mu-sigma%*%lambda[1:3])
  B2=t(Phi-sigma%*%matrix(lambda[4:12],nrow=3))%*%AB1[2:4]-c(1,0,0)  
  return(c(A2,B2))
}
getyn<-function(Xt,AB){
  nn=c(1,4,8,12,16,20)
  return(-(AB[nn,1]+(AB[nn,2:4])%*%Xt)/nn)
}
getAB<-function(lambda){
  AB=matrix(c(A1,B1),nrow=1)
  for (t in 2:20) {
    AB=rbind(AB,getABi(AB[nrow(AB),],lambda))
  }
  return(AB)
}
getdy2vect<-function(lambda) {
  #first three values are lambda0, next 9 form lambda1
  yhats=t(apply(x,1,getyn,getAB(lambda)))
  return(colMeans((yhats-ys)^2))
}
getdy2<-function(lambda){
  return(sum(getdy2vect(lambda)*c(100,.01,.01,.01,.01,1000000000))) #multipliers here force estimation to be perfect for two factor yields, ie shortrate and 20 quarter rate
}

#get the optimal fit
fito=optim(rep(0,12),getdy2,control = list(reltol=1e-14,maxit=50000))
lambdafit=fito$par
lambdafit

lambda0=lambdafit[1:3]
lambda1=matrix(lambdafit[4:12],nrow=3)
lambda0
lambda1

Omega=c(0,sqrt(getdy2vect(lambdafit)[2:5]),0)
Omega

#check what the optimal results look like
getdy2vect(lambdafit)
getAB(lambdafit)
yhats=t(apply(x,1,getyn,getAB(lambdafit)))
View(yhats)
