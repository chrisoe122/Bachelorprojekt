source("Monte-Carlo.R")
library(grid) #Har fkt 'textGrob' så title på plot kan forstørres

###Teoretisk værdi ####
d1<-function(S,K,r,tau,sigma){
  a<- log(S/K)+(r+(sigma^2)*0.5)*tau
  return(a/(sigma*sqrt(tau)))
}

d2<-function(S,K,r,tau,sigma){
  return(d1(S,K,r,tau,sigma)-sigma*sqrt(tau))
}

V_c<-function(S,K,r,tau,sigma){
  return(S*pnorm(d1(S,K,r,tau,sigma),0,1)-K*exp(-r*tau)*pnorm(d2(S,K,r,tau,sigma),0,1)) 
}

V_p<- function(S,K,r,tau,sigma){
  return(K*exp(-r*tau)+V_c(S,K,r,tau,sigma)-S)
}


####Simulering ######

#Sigma
sigma_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x))
  B<-rep(NA,length(x))
  C <- rep(NA,length(x))
  D<-rep(NA,length(x))
  
  #Udregning
  if(c==T){ #Call option
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=12, mu=0.03, y0=10, sigma=x[i])
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=12, mu=0.03, y0=10, sigma=x[i])
      C[i]<- option_cv(n=simul, delta_t=1/500, k=500, K=12, 
                       K2=9, mu=0.03, y0=10, sigma=x[i], r=0.03)
      D[i]<- V_c(S=10, K=12, r=0.03, tau=1, sigma=x[i])
    }}
  else{ #Put option
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=12, mu=0.03, y0=10, sigma=x[i], c=F)
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=12, mu=0.03, y0=10, sigma=x[i],c=F)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=500, K=12, 
                       K2=9, mu=0.03, y0=10, sigma=x[i], c=F, r=0.03)
      D[i]<- V_p(S=10, K=12, r=0.03, tau=1, sigma=x[i])
    }
  }
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#S
S_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x)) #ant
  B<-rep(NA,length(x)) #monte
  C<-rep(NA,length(x)) #control
  D<-rep(NA,length(x)) #bs
  
  #Udregning
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=12, mu=0.03, y0=x[i], sigma=0.2)
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=12, mu=0.03, y0=x[i], sigma=0.2)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=500, K=12, 
                       K2=9, mu=0.03, y0=x[i], sigma=0.2, r=0.03)
      D[i]<- V_c(S=x[i], K=12, r=0.03, tau=1, sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=12, mu=0.03, y0=x[i], sigma=0.2, c=F)
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=12, mu=0.03, y0=x[i], sigma=0.2, c=F)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=500, K=12, 
                 K2=9, mu=0.03, y0=x[i], sigma=0.2, c=F, r=0.03)
      D[i]<- V_p(S=x[i], K=12, r=0.03, tau=1, sigma=0.2)
  }}
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#Tid
T_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x)) #ant
  B<-rep(NA,length(x)) #monte
  C<-rep(NA,length(x)) #control
  D<-rep(NA,length(x)) #bs
  
  #Udregning
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03*x[i])*option_ant(n=simul/2, delta_t=1/500, k=as.integer(500*x[i]), K=12, mu=0.03, y0=10, sigma=0.2)
      B[i]<- exp(-0.03*x[i])*option_monte(n=simul, delta_t=1/500, k=as.integer(500*x[i]), K=12, mu=0.03, y0=10, sigma=0.2)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=as.integer(500*x[i]), K=12, 
                       K2=9, mu=0.03, y0=10, sigma=0.2, r=0.03)
      D[i]<- V_c(S=10, K=12, r=0.03, tau=x[i], sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03*x[i])*option_ant(n=simul/2, delta_t=1/500, k=as.integer(500*x[i]), K=12, mu=0.03, y0=10, sigma=0.2,c=F)
      B[i]<- exp(-0.03*x[i])*option_monte(n=simul, delta_t=1/500, k=as.integer(500*x[i]), K=12, mu=0.03, y0=10, sigma=0.2,c=F)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=as.integer(500*x[i]), K=12, 
                       K2=9, mu=0.03, y0=10, sigma=0.2, c=F, r=0.03)
      D[i]<- V_p(S=10, K=12, r=0.03, tau=x[i], sigma=0.2)
  }}
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#Rente
r_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x)) #ant
  B<-rep(NA,length(x)) #monte
  C<-rep(NA,length(x)) #control
  D<-rep(NA,length(x)) #bs
  
  #udregning
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-x[i])*option_ant(n=simul/2, delta_t=1/500, k=500, K=12, mu=x[i], y0=10, sigma=0.2)
      B[i]<- exp(-x[i])*option_monte(n=simul, delta_t=1/500, k=500, K=12, mu=x[i], y0=10, sigma=0.2)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=500, K=12, 
                       K2=9, mu=x[i], y0=10, sigma=0.2, r=x[i])
      D[i]<- V_c(S=10, K=12, r=x[i], tau=1, sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-x[i])*option_ant(n=simul/2, delta_t=1/500, k=500, K=12, mu=x[i], y0=10, sigma=0.2,c=F)
      B[i]<- exp(-x[i])*option_monte(n=simul, delta_t=1/500, k=500, K=12, mu=x[i], y0=10, sigma=0.2,c=F)
      C[i]<- option_cv(n=simul, delta_t=1/500, k=500, K=12, 
                       K2=9, mu=x[i], y0=10, sigma=0.2, c=F, r=x[i])
      D[i]<- V_p(S=10, K=12, r=x[i], tau=1, sigma=0.2)
  }}
  E<-data.frame(x,A,B,C,D)
  return(E)
}

pris_graf<-function(data,xv,title){
  colnames(data)<-c('x','Anti','Monte', 'Control', 'BS')
  data1 <- data %>%
    select(x, Anti, Monte, Control, BS) %>%
    gather(key = "variable", value = "value", -x) #Laver det til en enkel column, som ggplot bruger
  data1$variable <- factor(data1$variable,levels = c('Anti','Monte', 'Control', 'BS')) #Så legend står i korrekt rækkefølge
  ggplot(data1, aes(x = x, y = value, group=variable, size=variable, colour=variable)) + 
    geom_line() +
    scale_size_manual(breaks=c("Anti","Monte","Control","BS"), values=c(0.8,0.8,0.8,1.1), guide=F) + #Forskelligt størrelse, guide=F laver ingen legend
    theme_minimal() +
    xlab(xv) + #x-axis navn
    ylab('Optionspris') + #y-axis navn
    ggtitle(title) + #title
    theme(plot.title = element_text(hjust = 0.5, size=18))+ #Størrelse på title
    theme(legend.key.size = unit(1.5, 'cm')) + #Ændre legend
    theme(axis.title = element_text(size=12)) + #Størrelse på axis
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank(),
          legend.key = element_blank(), legend.title = element_blank()) +
    scale_colour_discrete('') + #Ændre navn på legend
    scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
}



###Udregning af abs fejl ved monte, anti og cv i forhold til teoretisk værdi.
monte_abs_opt<-function(delta_t=1/500, k=500, mu=0.03, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(option_monte(n=i*step+start, delta_t=1/500, k=k, K=12, mu=0.03, y0=10, sigma=0.2)-V_c(S=10, K=12, r=0.03, tau=1, sigma=0.2))
  }
  return(a)
}

ant_abs_opt<-function(delta_t=1/500, k=500, mu=0.03, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(option_ant(n=as.integer(i*step+start)/2, delta_t=1/500, k=k, K=12, mu=0.03, y0=10, sigma=0.2)-V_c(S=10, K=12, r=0.03, tau=1, sigma=0.2))
  }
  return(a)
}

cv_abs_opt<-function(delta_t=1/500, k=500, mu=0.03, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(option_cv(n=start+step*i, delta_t=1/500, k=500, K=12, 
                          K2=9, mu=0.03, y0=10, sigma=0.2, r=mu)-V_c(S=10, K=12, r=0.03, tau=1, sigma=0.2))
  }
  return(a)
}




#Plot til option data
iv_plt<-function(dataset, title, legend){
  ggplot(dataset, aes(x = x, y = y)) + 
    geom_point(color='red', size = 1) +
    theme_minimal() +
    xlab('Log-moneyness') +
    ylab('IV') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size=15))+ 
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(axis.title = element_text(size=12)) +
    scale_colour_discrete(legend) + #Ændre navn på legend
    scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
}
