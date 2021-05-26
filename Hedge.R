source('Options.R')

V_c<-function(S,K,r,tau,sigma){
  return(S*pnorm(d1(S,K,r,tau,sigma),0,1)-K*exp(-r*tau)*pnorm(d2(S,K,r,tau,sigma),0,1)) 
}



hedge<-function(S,K,r,sigma_m,sigma_i,sigma_h,mu,n, sd=T, sti=F){
  #Sæt op
  hedg <- rep(NA, n+1)
  C <- rep(NA, n+1)
  
  #Periode 0
  
  # Aktivet
  S1 <- S
  
  # Porteføljen
  C[1]<- V_c(S,K,r,1,sigma_i) #Værdi af call
  delta<- pnorm(d1(S,K,r,1,sigma_h),0,1)
  s_del <- S*delta
  bond <- C[1]-s_del
  hedg[1]<- s_del + bond
  
  #Opdatering
  for (i in 1:(n-1)){
    #Aktivet
    rn <- rnorm(1)*sqrt(1/n)
    S1<- S1*exp((mu-0.5*sigma_m^2)*(1/n)+sigma_m*(rn))
    
    #Porteføljen
    delta_1 <- delta
    delta<-pnorm(d1(S1,K,r,1-i/n,sigma_h),0,1)
    bond <- (S1*delta_1-S1*delta)+bond*exp(r*1/n) #Banken
    s_del <- S1*delta #Andel af aktivet
    hedg[i+1]<- s_del + bond #Porteføljen
    C[i+1] <- V_c(S1,K,r,1-i/n,sigma_i) #Værdien af call
  }
  
  #Sidste periode
  #Aktivet
  rn <- rnorm(1)*sqrt(1/n)
  S1<- S1*exp((mu-0.5*sigma_m^2)*(1/n)+sigma_m*(rn))
  
  #Porteføljen
  bond <- bond*exp(r*1/n) #Banken
  s_del <- S1*delta #Andel af aktivet
  hedg[n+1]<- s_del + bond #Porteføljen
  C[n+1] <- max(S1-K,0) #Værdien af call
  if (sd==T){
    if (sti==F){
      return(sd(hedg-C)) #Hedgin error
    }
    else{ #Plot i forhold til størrelsen af S
      a<-hedg[n+1]
      b<- S1
      return(c(b,a))
    }
  }
  else{ #Udvikling af værdi af pf (Bruges til 4.2)
    return(C-hedg) 
  }
}


#FEJL PLOT ________________________________
delta_fejl<-function(t, n, sigma_m, sigma_i, sigma_h){
  a <- rep(NA,t)
  for (i in 1:t){
    a[i]<-hedge(S=10, K=7, r=0.02, sigma_m = sigma_m, sigma_i = sigma_i, sigma_h = sigma_h, 
                mu=0.1, n, sd=T)
  }
  return(mean(a))
}


delta_er_data<-function(t, sigma_m, sigma_i, sigma_h){
  #data
  vardi10<-delta_fejl(t,10, sigma_m, sigma_i, sigma_h)
  vardi25<-delta_fejl(t,25, sigma_m, sigma_i, sigma_h)
  vardi50<- delta_fejl(t,50, sigma_m, sigma_i, sigma_h)
  vardi100<-delta_fejl(t,100, sigma_m, sigma_i, sigma_h)
  vardi200<-delta_fejl(t,200, sigma_m, sigma_i, sigma_h)
  vardi250<-delta_fejl(t,250, sigma_m, sigma_i, sigma_h)
  vardi500<-delta_fejl(t,500, sigma_m, sigma_i, sigma_h)
  vardi1000<-delta_fejl(t,1000, sigma_m, sigma_i, sigma_h)
  dv<-c(vardi10,vardi25, vardi50,vardi100,vardi200,vardi250,vardi500,vardi1000)
  dx <- c(10, 25,50,100,200,250,500,1000)
  dataf<-data.frame(dx,dv)
  return(dataf)
}

#plot 
##Rigtig sigma
data_real<-delta_er_data(1000, 0.3, 0.3, 0.3)

lm(log(data_real[,2])~log(data_real[,1])) #For at lave linje

ggplot(data_real, aes(data_real[,1],data_real[,2])) + geom_point() +
  geom_function(fun = function(x) exp(-2.1064)*x^(-0.5)) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab('Hedge Points') +
  ylab('Standard error')+
  theme_minimal()




#Forkert sigma
data_w <- delta_er_data(1000, 0.1, 0.1, 0.3)

lm(log(data_w[,2])~log(data_w[,1])) #For at lave linje

ggplot(data_real, aes(data_w[,1],data_w[,2])) + geom_point() +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab('Hedge Points') +
  ylab('Standard error') +
  ylim(0,0.02) +
  theme_minimal()























#HEDGING PLOTS___________________________
graph_delta<-function(dataframe){
  df1 <- dataframe
  colnames(df1)<-c('t0', 't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'x')
  df <- df1 %>%
    select(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, x) %>%
    gather(key = "variable", value = "value", -x) #Laver det til en enkel column, som ggplot bruger
  #Plot
  ggplot(df, aes(x = x, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() +
    xlab('T') +
    ylab('PnL') +
    ggtitle('') +
    theme(plot.title = element_text(hjust = 0.5, size=20))+ 
    theme(axis.title = element_text(size=12)) +
    scale_x_continuous(expand = c(0.01, 0)) + #Så plot starter og slutter ved fct (næsten)
    theme(legend.position = "none")
}



#Hedging model sigma

loop<-function(n, k, sigma_m, sigma_i, sigma_h){
  H<- matrix(data=NA, nrow=n+1, ncol=k)
  for (i in 1:k){
    H[,i]<- hedge(S=10, K=11, r= 0.02, sigma_m=sigma_m, 
                  sigma_i = sigma_i, sigma_h = sigma_h , mu= 0.1, n=n, sd=F)
  }
  x <- seq(from=0, to =1, by=1/n)
  H1 <- as.data.frame(cbind(H,x))
  return(H1)
}

#Hedging with IV
dataasd_iv<-loop(250,10,0.2,0.1,0.1)
graph_delta(dataasd_iv)

#Hedging with model sigma
dataasd_m<-loop(250,10,0.2,0.1,0.2)
graph_delta(dataasd_m)
exp(0.02*1)*(V_c(10,11,0.02,1,0.2)-V_c(10,11,0.02,1,0.1))
#Hvorfor kan den godt være negativ???












#Payoff fkt med forskellige sigma__________


#Forkert sigma
ddd<-matrix(data=NA, nrow=1000,ncol=2)
for (i in 1:1000){
  ddd[i,]<-hedge(10,7,0.02,0.3,0.3, 0.1,0.1,1000, sd=T, sti=T)
}

dataf1<-data.frame(ddd[,1],ddd[,2])
ggplot(dataf1, aes(ddd[,1],ddd[,2])) + geom_function(fun = function(x) pmax(x-7,0), col='blue', size=1) +
  geom_point() +
  xlab('Hedge Points') +
  ylab('V�rdi') +
  ggtitle('') +
  theme_minimal()
#Ser også fint ud. 


#Rigtig sigma
ddd1<-matrix(data=NA, nrow=1000,ncol=2)
for (i in 1:1000){
  ddd1[i,]<-hedge(10,7,0.02,0.3,0.3, 0.3,0.1,1000, sd=T, sti=T)
}

dataf11<-data.frame(ddd1[,1],ddd1[,2])
ggplot(dataf11, aes(ddd1[,1],ddd1[,2])) + geom_function(fun = function(x) pmax(x-7,0), col='blue', size=1) +
  geom_point() +
  xlab('Hedge Points') +
  ylab('V�rdi') +
  ggtitle('') + 
  theme_minimal()


