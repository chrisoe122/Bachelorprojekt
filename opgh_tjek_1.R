#Opg h)
source('Monte-Carlo.R')
J<-1000
L<- 5000
muhat<-rep(NA,J)
muhatlog<-rep(NA,J)
sigmahatlog<-rep(NA,J)
for (i in 1:J){
  a<-Euler(1/300,L, y0=10)
  logp<-(log(a)-log(lag(a)))[2:L+1]
  muhat[i]<-(mean(logp))*300+0.5*var(logp)*300
  muhatlog[i]<-mean(logp)
  sigmahatlog[i]<- var(logp)
  #print(i)
}

#Sigmahatlog^2

sqrt(2*(0.2^4*(1/500)^2)/5000) #Teoretisk v?rdi for var af sigmahatlog
s(sigmahatlog)

#Sigmahat^2
var(sqrt(sigmahatlog/L))




#Den f?rste
(0.2^2)/(2*L)
#Den anden
(0.2^2*(1/300))/(2*L)


mean(muhat)
var(muhatlog)
var(muhat)
median(muhat)
mean(sigmahatlog*300)
mean(sqrt(sigmahatlog*300))
sqrt(mean(sigmahatlog*300))
var(sqrt(sigmahatlog*300))



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