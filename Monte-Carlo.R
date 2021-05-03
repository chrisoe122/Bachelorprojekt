library(ggplot2) #ggplot
library(tidyr) #Har 'gather', så man kan ordne data (Bruges i ggplot)
library(dplyr) #Har 'select', bruges også til at ordne data (Bruges i ggpplot)
library(gridExtra) #Har fkt 'grid.arrange'


#Simulering af S
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
  if(ant==F && cv==F){ #Hvorfor kan jeg ikke bruge else?
    for (i in 1:k){
      delta_W = rnorm(1)*sqrt(delta_t)
      y[i+1]<-y[i]+mu*y[i]*delta_t+sigma*y[i]*delta_W
    }
    A <- y
  }
  return(A)
}



###GRAFER ######

#Fkt til at plt 4 forskellige delta_t i samme tidsperiode
plt_delta<-function(d1, d2, d3, d4, k= 100,y0=10,delta_t,mu=0.07,sigma=0.2){
  #udregning
  A<-Euler(d1,1/d1)
  xa<-seq(0,1,d1)
  B<- Euler(d2,1/d2)
  xb<-seq(0,1,d2)
  C <- Euler(d3,1/d3)
  xc<-seq(0,1,d3)
  D <- Euler(d4,1/d4)
  xd<-seq(0,1,d4)
  dfa <- data.frame(xa,A)
  dfb <- data.frame(xb,B)
  dfc <- data.frame(xc,C)
  dfd <- data.frame(xd,D)
  
  #plot
  Aplt<-ggplot(dfa,aes(xa,A)) + geom_line(size=0.01) +
    ggtitle(bquote(paste(Delta,'=', .(d1), ',','grid points','=', .(1/d1+1)))) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    xlab('Tid') +
    ylab('')
  Bplt<-ggplot(dfb,aes(xb,B)) + geom_line(size=0.5) + 
    ggtitle(bquote(paste(Delta,'=', .(d2), ',','grid points','=', .(1/d2+1)))) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) + 
    xlab('Tid') +
    ylab('')
  Cplt<-ggplot(dfc,aes(xc,C)) + geom_line(size=0.01) + 
    ggtitle(bquote(paste(Delta,'=', .(d3), ',','grid points','=', .(1/d3+1)))) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    xlab('Tid') +
    ylab('')
  Dplt<-ggplot(dfd,aes(xd,D)) + geom_line(size=0.01) + 
    ggtitle(bquote(paste(Delta,'=', .(d4), ',','grid points','=', .(1/d4+1)))) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=15)) +
    xlab('Tid') +
    ylab('')
  grid.arrange(Aplt,Bplt,Cplt,Dplt)
}


#Hjlæpefunction til at lave dataframe til at plotte trajectories
plt<-function(number=10,k= 100,y0=10,mu=0.07,sigma=0.2){
  
  #parameter
  delta_t0 = 1/1000 #startværdi
  H <- matrix(data = NA, nrow = k+1, ncol = number)
  
  #udregning
  for (i in 1:10){
    H[,i]<-Euler(delta_t = delta_t0, k=k)
    delta_t0<-  delta_t0 + 0.01
  }
  H1 <- as.data.frame(cbind(H,seq(0,k))) #Dataframe, så det kan bruges i ggplot
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
    ggtitle(bquote(paste('Trajectories with increasing ', Delta))) +
    theme(plot.title = element_text(hjust = 0.5, size=20))+ 
    theme(legend.key.size = unit(1.5, 'cm')) +
    theme(axis.title = element_text(size=12)) +
    scale_colour_discrete("Trajectories") + #Ændre navn på legend
    scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
}

####################  SIMULERING AF FORVENTNING AF S_T ###########

#Den teoretiske værdi (Forventet værdi af en lognormal)
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

#Monte carlo
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

monte_test<-function(n,delta_t,k,y0=10,mu=0.07,sigma=0.2, loop){
  monte_t<-rep(NA,loop)
  for (i in 1:loop){
    monte_t[i]<-monte(n=n,delta_t=delta_t,k=k, mu=mu, sigma=sigma)
  }
  a<-mean(monte_t)
  b<-var(monte_t)
  return(paste('Mean:',formatC(a, digits = 5, format = "f"), 'Var:', 
               formatC(b, digits = 8, format = "f")))
}


#ANTITHETIC
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

ant_test<-function(n,delta_t,k,y0=10,mu=0.07,sigma=0.2, loop){
  ant_t<-rep(NA,loop)
  ant_cov<-rep(NA,loop)
  c <- rep(NA,2)
  for (i in 1:loop){
    c <-ant(n=n,delta_t=delta_t,k=k, mu=mu, sigma=sigma, normal=F)
    ant_t[i]<-c[1]
    ant_cov[i]<-c[2]
  }
  a<-mean(ant_t)
  b<-var(ant_t)
  d<-mean(ant_cov)
  return(paste('Mean:',formatC(a, digits = 5, format = "f"), 'Var:', 
               formatC(b, digits = 8, format = "f"), d))
}


#Control
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

cv_test<-function(n,delta_t,k,y0=10,mu=0.07,sigma=0.2, mu2=0.05, sigma2=0.2, loop){
  cv_t<-rep(NA,loop)
  cv_cov<-rep(NA,loop)
  for (i in 1:loop){
    c <- cv(n=n,delta_t=delta_t,k=k, mu=mu, sigma=sigma, mu2=mu2, sigma2=sigma2, normal=F)
    cv_t[i]<-c[1]
    cv_cov[i]<- c[2]
  }
  a<-mean(cv_t)
  b<-var(cv_t)
  c<-mean(cv_cov)
  return(paste('Mean:',formatC(a, digits = 5, format = "f"), 'Var:', 
               formatC(b, digits = 8, format = "f"),c))
}


#Udregning af abs fejl ved monte- og anti-metoden i forhold til teoretisk værdi
monte_abs_data<-function(delta_t=1/500, k=500, mu=0.07, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(monte(n=(step*i+start), delta_t=1/500, k=500, mu=0.07, sigma=0.2)-Teo_v(1,mu=0.07, sigma=0.2))
  }
  return(a)
}

ant_abs_data <- function(delta_t=1/500, k=500, mu=0.07, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(ant(n=as.integer(step*i+start)/2, delta_t=1/500, k=500, mu=0.07, sigma=0.2, normal=F)[1]-Teo_v(1,mu=0.07, sigma=0.2))
  }
  return(a)
}


cv_abs_data <- function(delta_t=1/500, k=500, mu=0.07, sigma=0.2, points, start, step){
  a<-rep(NA,points)
  for (i in 1:points){
    a[i] <- abs(cv(n=as.integer(step*i+start)/2, delta_t=1/500, k=500, mu=0.07, sigma=0.2, normal=F)[1]-Teo_v(1,mu=0.07, sigma=0.2))
  }
  return(a)
}

#Plt til det 
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











#########  OPTIONER  ####
#Monte
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

#Antithetic
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


#Control
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