source("Monte-Carlo.R")
library(grid) #Har fkt 'textGrob' så title på plot kan forstørres


##### BS værdier 
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


##### Simulering
#Simulering af optioner via MC, AV og CV

#MC
option_monte <- function(n,delta_t,k, K, mu, y0=10, sigma=0.2,c=T){
  z <- rep(NA,n) #Holder Euler-værdierne
  j <- rep(NA,n)
  for (i in 1:n){
    z[i]<-Euler(delta_t,k, mu=mu, y0=y0, sigma=sigma)[k+1]
    if(c==T){
      j[i] <- max(z[i]-K,0)
    }
    else{
      j[i] <- max(K-z[i],0)
    }
  }
  a<- mean(j)
  return(a)
}

#AV
option_ant <- function(n,delta_t,k, K, mu, y0=10,sigma=0.2, c=T){
  z <- matrix(data=NA, ncol=2, nrow=n)
  j <- matrix(data=NA, ncol=2, nrow=n)
  for (i in 1:n){
    z[i,]<-Euler(delta_t,k,mu=mu, y0=y0, sigma=sigma, ant=T)[k+1,1:2]
    if (c==T){
      for (l in 1:2){
        j[i,l]=max(z[i,l]-K,0)
      }}
    else{
      for (l in 1:2){
        j[i,l]=max(K-z[i,l],0)
      }}
  }
  a<- 1/(2*n)*sum(j)
  return(a)
}


#CV
option_cv <- function(n,delta_t,k, K, K2, mu, y0=10,sigma=0.2, c=T, r=0.03){
  z <- rep(NA,n) #Euler-værdierne
  j <- rep(NA,n) #Det vi er interesseret i
  b <- rep(NA,n) #Kalibrering værdier
  if(c==T){
    for (i in 1:n){
      z[i] <- Euler(delta_t,k,mu=mu, y0=y0, sigma=sigma)[k+1]
      j[i] <- max(z[i]-K,0)
      b[i] <- max(z[i]-K2,0)
    }
    a<- exp(-r*delta_t*k)*mean(j)+V_c(y0,K2,r,delta_t*k,sigma)-exp(-r*delta_t*k)*mean(b)
  }
  else{
    for (i in 1:n){
      z[i] <- Euler(delta_t,k,mu=mu, y0=y0, sigma=sigma)[k+1]
      j[i] <- max(K-z[i],0)
      b[i] <- max(K2-z[i],0)
    }
    a<- exp(-r*delta_t*k)*mean(j)+V_p(y0,K2,r,delta_t*k,sigma)-exp(-r*delta_t*k)*mean(b)
  }
  return(a)
}





##### Til figur 5
#MC
monte_abs_opt<-function(delta_t=1/500, k=500, mu=0.03, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(option_monte(n=i*step+start, delta_t=1/500, k=k, K=11, mu=0.03, y0=10, sigma=0.2)-V_c(S=10, K=11, r=0.03, tau=1, sigma=0.2))
  }
  return(a)
}

#AV
ant_abs_opt<-function(delta_t=1/500, k=500, mu=0.03, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(option_ant(n=as.integer(i*step+start)/2, delta_t=1/500, k=k, K=11, mu=0.03, y0=10, sigma=0.2)-V_c(S=10, K=11, r=0.03, tau=1, sigma=0.2))
  }
  return(a)
}

#CV
cv_abs_opt<-function(delta_t=1/500, k=500, mu=0.03, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(option_cv(n=as.integer(start+step*i)/2, delta_t=1/500, k=500, K=11, 
                          K2=9, mu=0.03, y0=10, sigma=0.2, r=mu)-V_c(S=10, K=11, r=0.03, tau=1, sigma=0.2))
  }
  return(a)
}



##### Til figur 6 og 7

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
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=11, mu=0.03, y0=10, sigma=x[i])
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=11, mu=0.03, y0=10, sigma=x[i])
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=500, K=11, 
                       K2=9, mu=0.03, y0=10, sigma=x[i], r=0.03)
      D[i]<- V_c(S=10, K=11, r=0.03, tau=1, sigma=x[i])
    }}
  else{ #Put option
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=11, mu=0.03, y0=10, sigma=x[i], c=F)
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=11, mu=0.03, y0=10, sigma=x[i],c=F)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=500, K=11, 
                       K2=9, mu=0.03, y0=10, sigma=x[i], c=F, r=0.03)
      D[i]<- V_p(S=10, K=11, r=0.03, tau=1, sigma=x[i])
    }
  }
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#S
S_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x)) #AV
  B<-rep(NA,length(x)) #MC
  C<-rep(NA,length(x)) #CV
  D<-rep(NA,length(x)) #BS
  
  #Udregning
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=11, mu=0.03, y0=x[i], sigma=0.2)
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=11, mu=0.03, y0=x[i], sigma=0.2)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=500, K=11, 
                       K2=9, mu=0.03, y0=x[i], sigma=0.2, r=0.03)
      D[i]<- V_c(S=x[i], K=11, r=0.03, tau=1, sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03)*option_ant(n=simul/2, delta_t=1/500, k=500, K=11, mu=0.03, y0=x[i], sigma=0.2, c=F)
      B[i]<- exp(-0.03)*option_monte(n=simul, delta_t=1/500, k=500, K=11, mu=0.03, y0=x[i], sigma=0.2, c=F)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=500, K=11, 
                 K2=9, mu=0.03, y0=x[i], sigma=0.2, c=F, r=0.03)
      D[i]<- V_p(S=x[i], K=11, r=0.03, tau=1, sigma=0.2)
  }}
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#Tid
T_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x)) #AV
  B<-rep(NA,length(x)) #MC
  C<-rep(NA,length(x)) #CV
  D<-rep(NA,length(x)) #BS
  
  #Udregning
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-0.03*x[i])*option_ant(n=simul/2, delta_t=1/500, k=as.integer(500*x[i]), K=11, mu=0.03, y0=10, sigma=0.2)
      B[i]<- exp(-0.03*x[i])*option_monte(n=simul, delta_t=1/500, k=as.integer(500*x[i]), K=11, mu=0.03, y0=10, sigma=0.2)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=as.integer(500*x[i]), K=11, 
                       K2=9, mu=0.03, y0=10, sigma=0.2, r=0.03)
      D[i]<- V_c(S=10, K=11, r=0.03, tau=x[i], sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-0.03*x[i])*option_ant(n=simul/2, delta_t=1/500, k=as.integer(500*x[i]), K=11, mu=0.03, y0=10, sigma=0.2,c=F)
      B[i]<- exp(-0.03*x[i])*option_monte(n=simul, delta_t=1/500, k=as.integer(500*x[i]), K=11, mu=0.03, y0=10, sigma=0.2,c=F)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=as.integer(500*x[i]), K=11, 
                       K2=9, mu=0.03, y0=10, sigma=0.2, c=F, r=0.03)
      D[i]<- V_p(S=10, K=11, r=0.03, tau=x[i], sigma=0.2)
  }}
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#Rente
r_data<-function(start,slut,interval, simul=1000,c=T){
  
  #parameter
  x<-seq(start,slut,interval)
  A<-rep(NA,length(x)) #AV
  B<-rep(NA,length(x)) #MC
  C<-rep(NA,length(x)) #CV
  D<-rep(NA,length(x)) #BS
  
  #udregning
  if(c==T){
    for (i in 1:length(x)){
      A[i]<- exp(-x[i])*option_ant(n=simul/2, delta_t=1/500, k=500, K=11, mu=x[i], y0=10, sigma=0.2)
      B[i]<- exp(-x[i])*option_monte(n=simul, delta_t=1/500, k=500, K=11, mu=x[i], y0=10, sigma=0.2)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=500, K=11, 
                       K2=9, mu=x[i], y0=10, sigma=0.2, r=x[i])
      D[i]<- V_c(S=10, K=11, r=x[i], tau=1, sigma=0.2)
    }}
  else{
    for (i in 1:length(x)){
      A[i]<- exp(-x[i])*option_ant(n=simul/2, delta_t=1/500, k=500, K=11, mu=x[i], y0=10, sigma=0.2,c=F)
      B[i]<- exp(-x[i])*option_monte(n=simul, delta_t=1/500, k=500, K=11, mu=x[i], y0=10, sigma=0.2,c=F)
      C[i]<- option_cv(n=simul/2, delta_t=1/500, k=500, K=11, 
                       K2=9, mu=x[i], y0=10, sigma=0.2, c=F, r=x[i])
      D[i]<- V_p(S=10, K=11, r=x[i], tau=1, sigma=0.2)
  }}
  E<-data.frame(x,A,B,C,D)
  return(E)
}

#Plot
pris_graf<-function(data,xv,title){
  colnames(data)<-c('x','AV','MC', 'CV', 'BS')
  data1 <- data %>%
    select(x, AV, MC, CV, BS) %>%
    gather(key = "variable", value = "value", -x) #Laver det til en enkel column, som ggplot bruger
  data1$variable <- factor(data1$variable,levels = c('AV','MC', 'CV', 'BS')) #Så legend står i korrekt rækkefølge
  ggplot(data1, aes(x = x, y = value, group=variable, size=variable, colour=variable)) + 
    geom_line() +
    scale_size_manual(breaks=c("AV","MC","CV","BS"), values=c(0.8,0.8,0.8,1.1), guide=F) + #Forskelligt størrelse, guide=F laver ingen legend
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




##### Til figur 9

#Newtons metode
newton<-function(V,x0,x1,S,K,r,tau,v, term){ 
  a<-1
  b<-1
  n <- 1
  while((a>0.0001 || b>0.0001) && n<term){
    x2<- x1 - (v-V(S,K,r,tau,x1))/((v-V(S,K,r,tau,x1))-(v-V(S,K,r,tau,x0))/(x1-x0))
    a<-abs(x2-x1)
    b<-abs(v-V(S,K,r,tau,x2))
    x0<-x1
    x1<-x2
    n <- n+1
    print(n)
  }
  c <- max(x2,0)
  d <- min(c,1)
  return(d)
}

#Funktion til data
iv_2<-function(n, start, step, data, date, tau, c=T, r=0.04, S){
  a<-rep(NA,n)
  b<-rep(NA,n)
  if(c==T){
    for (i in 1:n){
      ret_data <- data$mar.22.2021$calls$Vol>25
      ck_data <- na.omit(data$mar.22.2021$calls$Strike[ret_data])
      cl_data <- na.omit(data$mar.22.2021$calls$Last[ret_data])
      K1<- ck_data[start+i*step]
      value <- cl_data[start+i*step]
      print(value)
      a[i]<-newton(V_c, 0.02, 0.04, S=S, K=K1, r=r, tau=1/262, v=value, term=200)
      b[i]<-K1
      
    }
    c<-cbind(a,b)}
  else{
    for (i in 1:n){
      ret_data <- data$mar.22.2021$puts$Vol>25
      pk_data <- na.omit(data$mar.22.2021$puts$Strike[ret_data])
      pl_data <- na.omit(data$mar.22.2021$puts$Last[ret_data])
      K1<- pk_data[start+i*step]
      value <- pl_data[start+i*step]
      print(value)
      a[i]<-newton(V_p, 0.02, 0.04, S=S, K=K1, r=r, tau=1/262, v=value, term=200)
      b[i]<-K1 
    }
    c<-cbind(a,b)}
  return(c)
}

#Funktion til plot
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
