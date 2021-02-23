library(ggplot2)
library(tidyr)
library(dplyr)

#Simulering af S
Euler<-function(delta_t,k,t0=0,y0=10,W_0=0,mu=0.07,sigma=0.2){
  y <- rep(NA,k+1)
  y[1]<-y0
  dummy_t = t0
  for (i in 1:k){
   ny_t = delta_t + dummy_t
   delta_W = rnorm(1)*sqrt(delta_t)
   y[i+1]<-y[i]+mu*y[i]*delta_t+sigma*y[i]*delta_W
  }
  return(y)
}

#Function til at lave dataframe til at plotte trajectories
plt<-function(number=10,k= 100,t0=0,y0=10,delta_t,W_0=0,mu=0.07,sigma=0.2){
  #Udregning
  delta_t0 = 1/1000
  H <- matrix(data = NA, nrow = k+1, ncol = number)
  for (i in 1:10){
    H[,i]<-Euler(delta_t = delta_t0, k=k)
    delta_t0<-  delta_t0 + 0.01
  }
  H1 <- as.data.frame(cbind(H,seq(0,100))) #Dataframe, så det kan bruges i ggplot
  return(H1)
}


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


#SIMULERING AF MEAN
E_S_t <- function(n,t0,y0,delta_t,W_0=0,k,mu,sigma){
  z <- rep(NA,n) #Holder Euler-værdierne
  for (i in 1:n){
    z[i]<-Euler(t0,y0,delta_t,W_0=0,k,mu,sigma)[k+1]
  }
  a<- mean(z)
  return(a)
}

monte<-E_S_t(10000,0,10,1/1000,0,1000,0.07,0.2)
#T=10001
mu_log <- (0.07-0.5*0.2^2)*(1001/1000)+log(10) 
exp(mu_log+(0.2^2)/2) #Den teoretiske værdi
error<- monte-exp(mu_log+(1001/1000)*(0.2^2)/2) #Error


