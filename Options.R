source("Monte-Carlo.R")

#Teoretisk værdi
d1<-function(S,K,r,delta,tau,sigma){
  a<- log(S/K)+(r+(sigma^2)*0.5)*tau
  return(a/(sigma*sqrt(tau)))
}

d2<-function(S,K,r,delta,tau,sigma){
  return(d1(S,K,r,delta,tau,sigma)-sigma*sqrt(tau))
}

V_c<-function(S,K,r,delta,tau,sigma){
  return(S*pnorm(d1(S,K,r,delta,tau,sigma),0,1)-K*exp(-r*tau)*pnorm(d2(S,K,r,delta,tau,sigma),0,1)) 
}

V_p<- function(S,K,r,delta,tau,sigma){
  return(K*exp(-r*tau)+V_c(S,K,r,delta,tau,sigma)-S)
}



#Simulering


#Call
#Hvordan kan den være negativ??
V_c(10,12,0.03,0,1,0.2)

exp(-0.03)*option_ant(1000,1/500,500,12,0.03,c=T)
exp(-0.03)*option_monte(1000,1/500,500,12,0.03,c=T)
#KØR MED ANT OG MON

#GØR DET MED FORSKELLIGE VÆRDIER AF ST, T, r n, OG sigma

#sigma=0.3
exp(-0.03)*Call_ant(1000,1/500,500,12,0.03, y0=10,sigma=0.3)
exp(-0.03)*Call_monte(10000,1/500,500,12,0.03, y0=10, sigma=0.3)
V_c(10,12,0.03,0,501/500,0.3)

#S_0=8
exp(-0.03)*Call_ant(10000,1/500,500,12,0.03, y0=8,sigma=0.2)
exp(-0.03)*Call_monte(10000,1/500,500,12,0.03, y0=8, sigma=0.2)
V_c(8,12,0.03,0,501/500,0.2)

#T=3
exp(-0.03*3)*Call_ant(10000,1/500,1500,12,0.03, y0=10,sigma=0.2)
exp(-0.03*3)*Call_monte(10000,1/500,1500,12,0.03, y0=10, sigma=0.2)
V_c(10,12,0.03,0,3,0.2)

#r=0.05
exp(-0.05)*Call_ant(10000,1/500,500,12,0.05, y0=10,sigma=0.2) #Under Q er mu=r
exp(-0.05)*Call_monte(10000,1/500,500,12,0.05, y0=10, sigma=0.2) #Under Q er mu=r
V_c(10,12,0.05,0,1,0.2)



#FIND UD AF PRÆCIST HVAD T ER I KODEN 501/500 eller 1? Det er 1. Se screenshot

#Put
V_p(10,12,0.03,0,501/500,0.2)
exp(-0.03)*option_monte(100000,1/500,500,12,0.03,c=F)
exp(-0.03)*option_ant(100000,1/500,500,12,0.03,c=F)


