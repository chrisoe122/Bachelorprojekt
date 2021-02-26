library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(greekLetters)

#Simulering af S
Euler<-function(delta_t,k,t0=0,y0=10,W_0=0,mu=0.07,sigma=0.2){
  #parameter
  y <- rep(NA,k+1)
  ym <- rep(NA,k+1)
  y[1]<-y0
  ym[1] <- y0 #Til at lave antithetic
  dummy_t = t0
  
  #udregning
  for (i in 1:k){
   ny_t = delta_t + dummy_t
   delta_W = rnorm(1)*sqrt(delta_t)
   y[i+1]<-y[i]+mu*y[i]*delta_t+sigma*y[i]*delta_W
   ym[i+1] <- ym[i]+mu*ym[i]*delta_t-sigma*ym[i]*delta_W #antithetic
  }
  A <- cbind(y,ym)
  return(A)
}

#Fkt til at plt 4 forskellige delta_t i samme tidsperiode
plt_delta<-function(d1, d2, d3, d4, k= 100,t0=0,y0=10,delta_t,W_0=0,mu=0.07,sigma=0.2){
  #udregning
  A<-Euler(d1,1/d1)[,1]
  xa<-seq(0,1,d1)
  B<- Euler(d2,1/d2)[,1]
  xb<-seq(0,1,d2)
  C <- Euler(d3,1/d3)[,1]
  xc<-seq(0,1,d3)
  D <- Euler(d4,1/d4)[,1]
  xd<-seq(0,1,d4)
  dfa <- data.frame(xa,A)
  dfb <- data.frame(xb,B)
  dfc <- data.frame(xc,C)
  dfd <- data.frame(xd,D)
  
  #plot
  Aplt<-ggplot(dfa,aes(xa,A)) + geom_line(size=0.01) +
    ggtitle(paste(greeks('Delta'),'=',d1, ',','grid points','=',1/d1+1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    xlab('Tid') +
    ylab('')
  Bplt<-ggplot(dfb,aes(xb,B)) + geom_line(size=0.5) + 
    ggtitle(paste(greeks('Delta'),'=',d2, ',','grid points','=',1/d2+1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) + 
    xlab('Tid') +
    ylab('')
  Cplt<-ggplot(dfc,aes(xc,C)) + geom_line(size=0.01) + 
    ggtitle(paste(greeks('Delta'),'=',d3, ',','grid points','=',1/d3+1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    xlab('Tid') +
    ylab('')
  Dplt<-ggplot(dfd,aes(xd,D)) + geom_line(size=0.01) + 
    ggtitle(paste(greeks('Delta'),'=',d4, ',','grid points','=',1/d4+1)) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    xlab('Tid') +
    ylab('')
  grid.arrange(Aplt,Bplt,Cplt,Dplt)
}
plt_delta(1/10,1/100,1/1000,1/5000)


#Hjlæpefunction til at lave dataframe til at plotte trajectories
plt<-function(number=10,k= 100,t0=0,y0=10,delta_t,W_0=0,mu=0.07,sigma=0.2){
  #parameter
  delta_t0 = 1/1000
  H <- matrix(data = NA, nrow = k+1, ncol = number)
  
  #udregning
  for (i in 1:10){
    H[,i]<-Euler(delta_t = delta_t0, k=k)[,1]
    delta_t0<-  delta_t0 + 0.01
  }
  H1 <- as.data.frame(cbind(H,seq(0,100))) #Dataframe, så det kan bruges i ggplot
  return(H1)
}

#Plot 10 trajectories
graph<-function(dataframe){
  df1 <- dataframe
  colnames(df1)<-c('t0', 't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'x')
  df <- df1 %>%
    select(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, x) %>%
    gather(key = "variable", value = "value", -x) #Laver det til en enkel column, som ggplot bruger
  #Plot
  ggplot(df, aes(x = x, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() +
    xlab('Simulations') +
    ylab('') +
    ggtitle('Trajectories with increasing $delta_t$') +
    theme(plot.title = element_text(hjust = 0.5, size=20))+ 
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(axis.title = element_text(size=12)) +
    scale_colour_discrete("Trajectories") + #Ændre navn på legend
    scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
}

graph(plt())






####################  SIMULERING AF GÆT AF MEAN
#Monte carlo
E_S_t <- function(n,t0,y0,delta_t,W_0=0,k,mu,sigma){
  z <- rep(NA,n) #Holder Euler-værdierne
  for (i in 1:n){
    z[i]<-Euler(delta_t,k)[k+1,1]
  }
  a<- mean(z)
  return(a)
}

#ANTITHETIC
Ant <- function(n,t0,y0,delta_t,W_0=0,k,mu,sigma){
  z <- matrix(data=NA, ncol=2, nrow=n) 
  for (i in 1:n){
    z[i,]<-Euler(delta_t,k)[k+1,]
  }
  a <- 1/(2*n)*sum(z)
  return(a)
}



#Den teoretiske værdi (Forventet værdi af en lognormal)
Teo_v<- function(t){ #t angiver store T
  mu_log <- (0.07-0.5*0.2^2)*t+log(10)
  r <- exp(mu_log+(t*0.2^2)/2) 
  return(r)
}


#Error for de to metoder
monte<-E_S_t(10000,0,10,1/500,0,1000,0.07,0.2)
anti<-Ant(5000,0,10,1/500,0,1000,0.07,0.2)

error_m<- monte-Teo_v(2) 
error_a<- anti-Teo_v(2)
error_m
error_a
#Bemærk at n er forskelligt i de to metoder

#Loop for at skabe flere error
j<-rep(NA,10)
for (i in 1:10){
  monte<-E_S_t(10000,0,10,1/500,0,1000,0.07,0.2)
  anti<-Ant(5000,0,10,1/500,0,1000,0.07,0.2)
  error_m<- abs(monte-Teo_v(2)) 
  error_a<- abs(anti-Teo_v(2))
  j[i]<- error_m - error_a
}
#Positiv er m størst, negativ er a størst
mean(j) #Gns af error
j #Burde være positiv


#Tjek varians/sd ################### VED DET IKKE HELT OM DET BARE SKAL SLETTES
L<-2000
sv<-rep(NA,L)

for (i in 1:L){
  mon<-E_S_t(10000,0,10,1/300,0,300,0.07,0.2)
  sv[i]<-mon-Teo_v(1)
  print(i)
}

mean(sv)
sd(sv)
1/sqrt(10000)



########################### OPG H. BURDE NOK FLYTTES TIL ET ANDET DOKU
#Opg h)
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