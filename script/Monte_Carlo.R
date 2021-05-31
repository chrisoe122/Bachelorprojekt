library(ggplot2) #ggplot
library(tidyr) #Har 'gather', så man kan ordne data (Bruges i ggplot)
library(dplyr) #Har 'select', bruges også til at ordne data (Bruges i ggpplot)
library(gridExtra) #Har fkt 'grid.arrange'


#Eulers diskretisering
Euler<-function(delta_t,k,y0=10,mu=0.07,sigma=0.2, ant=F, mu2=0.05, sigma2=0.2, cv=F){
  #parameter
  y <- rep(NA,k+1)
  ym <- rep(NA,k+1)
  ycv <- rep(NA,k+1)
  y[1]<-y0
  ym[1] <- y0 #Til at lave antithetic
  ycv[1] <- y0
  
  #udregning
  if(ant==T && cv==F){ #Antithetic
    for (i in 1:k){
     delta_W = rnorm(1)*sqrt(delta_t)
     y[i+1]<-y[i]+mu*y[i]*delta_t+sigma*y[i]*delta_W
     ym[i+1] <- ym[i]+mu*ym[i]*delta_t-sigma*ym[i]*delta_W #Modsat variable
     }
    A <- cbind(y,ym)
    }
  if(ant==F && cv==T){ #Control
    for (i in 1:k){
      delta_W = rnorm(1)*sqrt(delta_t)
      y[i+1]<-y[i]+mu*y[i]*delta_t+sigma*y[i]*delta_W
      ycv[i+1]<-ycv[i]+mu2*ycv[i]*delta_t+sigma2*ycv[i]*delta_W #Control variable
    }
    A <- cbind(y,ycv)
  }
  if(ant==F && cv==F){
    for (i in 1:k){
      delta_W = rnorm(1)*sqrt(delta_t)
      y[i+1]<-y[i]+mu*y[i]*delta_t+sigma*y[i]*delta_W
    }
    A <- y
  }
  return(A)
}


##### Forventet værdi af en lognormal
Teo_v<- function(t,mu=0.07,sigma=0.2, y0=10, mean=T){ #t angiver store T
  mu_log <- (mu-0.5*sigma^2)*t+log(y0)
  if(mean==T){
    a <- exp(mu_log+(t*sigma^2)/2)
  }
  else{
    a <- (exp(sigma^2)-1)*exp(2*mu_log+sigma^2)
  }
  return(a)
}



##### Funktion vedr. simulering

#MC
monte <- function(n,delta_t,k,y0=10,mu=0.07,sigma=0.2, Euler=F){
  z <- rep(NA,n) #Holder Euler-værdierne
  if(Euler==T){
    for (i in 1:n){
      z[i]<-Euler(y0=y0, delta_t=delta_t, k=k, mu=mu, sigma=sigma)[k+1]
    }}
  else{
    for (i in 1:n){
      W<-rnorm(1,0,sqrt(delta_t*k))
      z[i]<- y0*exp((mu-sigma^2*0.5)*delta_t*k+sigma*W)
    }
  }
  a<- mean(z)
  return(a)
}


#AV
ant <- function(n,delta_t,k,y0=10,mu=0.07,sigma=0.2, normal=T, Euler=F){
  z <- matrix(data=NA, ncol=2, nrow=n)
  if(Euler==T){
    for (i in 1:n){
      z[i,]<-Euler(y0=y0, delta_t=delta_t, k=k, mu=mu, sigma=sigma, ant=T)[k+1,]
    }}
  else{
    for( i in 1:n){
      W <- rnorm(1,0,sqrt(delta_t*k))
      ba<- y0*exp((mu-sigma^2*0.5)*delta_t*k+sigma*W)
      bb<- y0*exp((mu-sigma^2*0.5)*delta_t*k-sigma*W)
      z[i,1]<- ba
      z[i,2]<- bb
  }}
  a <- 1/(2*n)*sum(z)
  b <- cov(z[,1],z[,2])
  if(normal==T){
    c<-paste('Mean:',formatC(a, digits = 5, format = "f"), 'cov:', 
             formatC(b, digits = 8, format = "f"))
  }
  else{
    c<-c(a,b)
  }
  return(c)
}


#CV
cv<-function(n,delta_t,k,y0=10,mu=0.07,sigma=0.2,mu2=0.05,sigma2=0.2, normal=T, Euler=F){
  z <- rep(NA,n)
  b1 <- rep(NA,n)
  b2 <- rep(NA,n)
  if(Euler==T){
    for (i in 1:n){
      b<-Euler(y0=y0,delta_t,k,mu=mu,sigma=sigma,mu2=mu2,sigma2=sigma2, cv=T)[k+1,]
      z[i]<-b[1]+Teo_v(delta_t*k,mu=mu2, sigma=sigma2)-b[2]
      b1[i]<-b[1]
      b2[i]<-b[2]
    }}
  
  else{
    for( i in 1:n){
      W <- rnorm(1,0,sqrt(delta_t*k))
      ba<- y0*exp((mu-sigma^2*0.5)*delta_t*k+sigma*W)
      bb<- y0*exp((mu2-sigma2^2*0.5)*delta_t*k+sigma2*W)
      z[i]<-ba+Teo_v(delta_t*k,mu=mu2, sigma=sigma2)-bb
      b1[i]<-ba
      b2[i]<-bb
    }
  }
  b <- cov(b1,b2)
  a <- 1/(n)*sum(z)
  if(normal==T){
    c<-paste('Mean:', formatC(a, digits = 5, format = "f"), 'cov:', formatC(b, digits = 5, format = "f"))
  }
  else{
    c<-c(a,b)
  }
  return(c)
}


##### Til figur 3a
Eulertest<-function(y0=10,mu=0.07,sigma=0.2, end=T){
  
  #parameter
  y2k <- rep(NA,2001)
  y1k <- rep(NA,1001)
  y500 <- rep(NA,501)
  y400 <- rep(NA,401)
  y250 <- rep(NA,251)
  y200 <- rep(NA,201)
  y125 <- rep(NA,126)
  y100 <- rep(NA,101)
  y80 <- rep(NA,81)
  teo <- rep(NA,501)
  
  y2k[1]<-y0
  y1k[1]<-y0
  y500[1]<-y0
  y400[1]<-y0
  y250[1]<-y0
  y200[1]<-y0
  y125[1]<-y0
  y100[1]<-y0
  y80[1]<-y0
  teo[1]<-y0
  rn <- rnorm(2000)*sqrt(1/2000)
  
  #udregning
  
  for (i in 1:2000){ #Teoretisk
    teo[i+1]<-y0*exp((mu-0.5*sigma^2)*(1/2000*i)+sigma*(sum(rn[1:i])))
  }
  for (i in 1:2000){ #2000
    delta_W = rn[i]
    y2k[i+1]<-y2k[i]+mu*y2k[i]*1/2000+sigma*y2k[i]*delta_W
  }
  for (i in 1:1000){ #1000
    a<- 1+(i-1)*2
    b <- i*2
    delta_W1 <- sum(rn[a:b])
    y1k[i+1]<-y1k[i]+mu*y1k[i]*1/1000+sigma*y1k[i]*delta_W1
  }
  for (i in 1:500){ #500
    a<- 1+(i-1)*4
    b <- i*4
    delta_W1 <- sum(rn[a:b])
    y500[i+1]<-y500[i]+mu*y500[i]*1/500+sigma*y500[i]*delta_W1
  }
  for (i in 1:400){ #400
    a<- 1+(i-1)*5
    b <- i*5
    delta_W1 <- sum(rn[a:b])
    y400[i+1]<-y400[i]+mu*y400[i]*1/400+sigma*y400[i]*delta_W1
  }
  for (i in 1:250){ #250
    a<- 1+(i-1)*8
    b <- i*8
    delta_W1 <- sum(rn[a:b])
    y250[i+1]<-y250[i]+mu*y250[i]*1/250+sigma*y250[i]*delta_W1
  }
  for (i in 1:200){ #200
    a<- 1+(i-1)*10
    b <- i*10
    delta_W1 <- sum(rn[a:b])
    y200[i+1]<-y200[i]+mu*y200[i]*1/200+sigma*y200[i]*delta_W1
  }
  for (i in 1:125){ #125
    a<- 1+(i-1)*16
    b <- i*16
    delta_W1 <- sum(rn[a:b])
    y125[i+1]<-y125[i]+mu*y125[i]*1/125+sigma*y125[i]*delta_W1
  }
  for (i in 1:100){ #100
    a<- 1+(i-1)*20
    b <- i*20
    delta_W1 <- sum(rn[a:b])
    y100[i+1]<-y100[i]+mu*y100[i]*1/100+sigma*y100[i]*delta_W1
  }
  for (i in 1:80){ #80
    a<- 1+(i-1)*25
    b <- i*25
    delta_W1 <- sum(rn[a:b])
    y80[i+1]<-y80[i]+mu*y80[i]*1/80+sigma*y80[i]*delta_W1
  }
  if(end==F){
    A <- cbind(teo, y2k, y1k, y500, y200, y100)
  }
  else{
    A<-cbind(teo[2001], y2k[2001], y1k[1001], y500[501], y400[401], y250[251], y200[201], y125[126], y100[101], y80[81])
  }
  return(A)
}




##### Til figur 4
#Data

#MC
monte_abs_data<-function(delta_t=1/500, k=500, mu=0.07, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(monte(n=(step*i+start), delta_t=1/500, k=500, mu=0.07, sigma=0.2)-Teo_v(1,mu=0.07, sigma=0.2))
  }
  return(a)
}

#AV
ant_abs_data <- function(delta_t=1/500, k=500, mu=0.07, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(ant(n=as.integer(step*i+start)/2, delta_t=1/500, k=500, mu=0.07, sigma=0.2, normal=F)[1]-Teo_v(1,mu=0.07, sigma=0.2))
  }
  return(a)
}

#CV
cv_abs_data <- function(delta_t=1/500, k=500, mu=0.07, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(cv(n=as.integer(step*i+start)/2, delta_t=1/500, k=500, mu=0.07, sigma=0.2, normal=F)[1]-Teo_v(1,mu=0.07, sigma=0.2))
  }
  return(a)
}

#Plt 
abs_opt_plt<-function(dataset, title, legend){
  ggplot(dataset, aes(x = x, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() +
    xlab('Simuleringer') +
    ylab('Fejl') +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size=20))+ 
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(axis.title = element_text(size=12)) +
    scale_colour_discrete(legend) + #Ændre navn på legend
    scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
}