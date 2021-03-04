source("Monte-Carlo.R")
library(grid)

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


#GØR DET MED FORSKELLIGE VÆRDIER AF ST, T, r n, OG sigma

sigma_plot<-function(start,slut,interval, simul=1000,c=T){
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x))
  B<-rep(NA,length(x))
  C <- rep(NA,length(x))
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(simul/2,1/500,500,12,0.03, y0=10,sigma=x[i])
      B[i]<- exp(-0.03)*option_monte(simul,1/500,500,12,0.03, y0=10, sigma=x[i])
      C[i]<- V_c(10,12,0.03,0,1,sigma=x[i])
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(simul/2,1/500,500,12,0.03, y0=10,sigma=x[i],c=F)
      B[i]<- exp(-0.03)*option_monte(simul,1/500,500,12,0.03, y0=10, sigma=x[i],c=F)
      C[i]<- V_p(10,12,0.03,0,1,sigma=x[i])
    }
  }
  D<-data.frame(x,A,B,C)
  return(D)
}

S_plot<-function(start,slut,interval, simul=1000,c=T){
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x))
  B<-rep(NA,length(x))
  C <- rep(NA,length(x))
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(simul/2,1/500,500,12,0.03, y0=x[i])
      B[i]<- exp(-0.03)*option_monte(simul,1/500,500,12,0.03, y0=x[i])
      C[i]<- V_c(S=x[i],12,0.03,0,1,sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(simul/2,1/500,500,12,0.03, y0=x[i],c=F)
      B[i]<- exp(-0.03)*option_monte(simul,1/500,500,12,0.03, y0=x[i],c=F)
      C[i]<- V_p(S=x[i],12,0.03,0,1,sigma=0.2)
  }}
  D<-data.frame(x,A,B,C)
  return(D)
}

T_plot<-function(start,slut,interval, simul=1000,c=T){
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x))
  B<-rep(NA,length(x))
  C <- rep(NA,length(x))
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03*x[i])*option_ant(simul/2,1/500,as.integer(500*x[i]),12,0.03, y0=10)
      B[i]<- exp(-0.03*x[i])*option_monte(simul,1/500,as.integer(500*x[i]),12,0.03, y0=10)
      C[i]<- V_c(10,12,0.03,0,tau=x[i], sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03*x[i])*option_ant(simul/2,1/500,as.integer(500*x[i]),12,0.03, y0=10,c=F)
      B[i]<- exp(-0.03*x[i])*option_monte(simul,1/500,as.integer(500*x[i]),12,0.03, y0=10,c=F)
      C[i]<- V_p(10,12,0.03,0,tau=x[i], sigma=0.2)
  }}
  D<-data.frame(x,A,B,C)
  return(D)
}


r_plot<-function(start,slut,interval, simul=1000,c=T){
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x))
  B<-rep(NA,length(x))
  C <- rep(NA,length(x))
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-x[i])*option_ant(simul/2,1/500,500,12,mu=x[i], y0=10)
      B[i]<- exp(-x[i])*option_monte(simul,1/500,500,12,mu=x[i], y0=10)
      C[i]<- V_c(10,12,x[i],0,1,sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-x[i])*option_ant(simul/2,1/500,500,12,mu=x[i], y0=10,c=F)
      B[i]<- exp(-x[i])*option_monte(simul,1/500,500,12,mu=x[i], y0=10,c=F)
      C[i]<- V_p(10,12,x[i],0,1,sigma=0.2)
  }}
  D<-data.frame(x,A,B,C)
  return(D)
}

pris_graf<-function(data,xv,title){
  colnames(data)<-c('x','Anti','Monte','BS')
  data1 <- data %>%
    select(x, Anti, Monte, BS) %>%
    gather(key = "variable", value = "value", -x) #Laver det til en enkel column, som ggplot bruger
  data1$variable <- factor(data1$variable,levels = c('Anti','Monte','BS')) #Så legend står i korrekt rækkefølge
  ggplot(data1, aes(x = x, y = value, group=variable)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() +
    xlab(xv) +
    ylab('Optionspris') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size=18))+ 
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(axis.title = element_text(size=12)) +
    scale_colour_discrete('') + #Ændre navn på legend
    scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
}
#Call plot
rc<-pris_graf(r_plot(0.01,0.05,0.001,c=T),'r','r')
Sc<-pris_graf(S_plot(3,8,0.1,c=T),'S(t)','S(t)')
Sigmac<-pris_graf(sigma_plot(0.1,0.5,0.01),greeks('sigma'),greeks('sigma'))
Tc<-pris_graf(T_plot(0.5,2,0.025),'T','T')
grid.arrange(rc,Sc,Sigmac,Tc, top = textGrob('Call option',gp=gpar(fontsize=28,font=1)))

#Put plot
rp<-pris_graf(r_plot(0.01,0.05,0.001,c=F),'r','r')
Sp<-pris_graf(S_plot(3,8,0.1,c=F),'S(t)','S(t)')
Sigmap<-pris_graf(sigma_plot(0.1,0.5,0.01,c=F),greeks('sigma'),greeks('sigma'))
Tp<-pris_graf(T_plot(0.5,2,0.025,c=F),'T','T')
grid.arrange(rp,Sp,Sigmap,Tp, top = textGrob('Put option',gp=gpar(fontsize=28,font=1)))

