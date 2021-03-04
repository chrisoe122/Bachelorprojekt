#Opg h)
source('Monte-Carlo.R')
J<-1000000
L<- 5000
muhat<-rep(NA,J)
muhatlog<-rep(NA,J)
sigmahat<-rep(NA,J)
for (i in 1:J){
  a<-Euler(1/500,L, y0=10)
  logp<-(log(a)-log(lag(a)))[2:L+1]
  muhat[i]<-(mean(logp))*500+0.5*var(logp)*500
  muhatlog[i]<-mean(logp)
  sigmahat[i]<- var(logp)
  print(i)
}
mean(muhat)
var(muhatlog)
var(muhat)
median(muhat)
mean(sigmahat*500)
var(sigmahat*500)
cor(muhat,sigmahat*500)

hist(sigmahat, breaks=40)


0.04/(L*1/(500)) + 0.04^2/(2*L)
var(muhat)
sd(muhat)

0.04^2*2/L
var(sigmahat*500)


0.04^2/(L)
cov(sigmahat*500,muhat)