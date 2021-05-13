source('Monte-Carlo.R')


##FOR N
#Error for de to metoder
monte_e<-monte(n=10000,delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)
anti<-ant(n=5000, delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)

error_m<- monte_e-Teo_v(t=2) 
error_a<- anti-Teo_v(t=2)
error_m
error_a
#Bemærk at n er forskelligt i de to metoder


#Loop for at skabe flere error (Finde den forventet værdi)
j<-rep(NA,10)
for (i in 1:10){
  monte<-E_S_t(n=10000,delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)
  anti<-Ant(n=5000, delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)
  error_m<- abs(monte-Teo_v(t=2)) 
  error_a<- abs(anti-Teo_v(t=2))
  j[i]<- error_m - error_a
}
#Positiv er monte-fejl størst, negativ er anti-fejl størst
mean(j) #Gns af error
j #Burde være positiv








####### For delta t. For euler
Eulertest<-function(y0=10,mu=0.07,sigma=0.2, end=T){ #Husk at ændre mu og sigma
  
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

#BRUGER IKKE DET KOMMENDE PLOT

dataset<-Eulertest(end=F)
teodata<-dataset[,1]
y2kdata<-dataset[,2]
y1kdata<-dataset[1:1001,3]
y500data<-dataset[1:501,4]
y200data<-dataset[1:201,5]
y100data<-dataset[1:101,6]

teodata<-data.frame(y=teodata,x=seq(0,1,1/2000))
y2kdata<-data.frame(y=y2kdata,x=seq(0,1,1/2000))
y1kdata<-data.frame(y=y1kdata,x=seq(0,1,1/1000))
y500data<-data.frame(y=y500data,x=seq(0,1,1/500))
y200data<-data.frame(y=y200data,x=seq(0,1,1/200))
y100data<-data.frame(y=y100data,x=seq(0,1,1/100))

ggdata<-rbind(teodata, y100data, y500data, y200data)
ggdata$dataset <- c(rep("A", nrow(teodata)),rep("C", nrow(y100data)),
                    rep("F", nrow(y200data)), rep("D", nrow(y500data)))

ggplot(data=ggdata, aes(x=x, y=y, col=dataset)) + geom_line()







####DELTA PLOT MED STORE T. FIGUR 1 MONTECARLO
set.seed(20213)
df_err1<-matrix(data=NA, nrow=200, ncol=9)
for (i in 1:200){
  ds<-Eulertest()
  for (k in 1:9){
    hold<-abs(ds[1]-ds[k+1])
    df_err1[i,k]<- hold
  }
}
x<-c(2000,1000,500,400,250,200,125,100,80)
df_err<-cbind(colMeans(df_err1),x)
df_err<-as.data.frame(df_err)
colnames(df_err)<-c('Error', 'x')


ggplot(data=df_err, aes(x=x, y=Error)) + 
  geom_function(aes(color='App. fkt'), fun = function(x) 0.24/sqrt(x), size=1) + 
  geom_point(aes(color='Fejl'), size=2)+
  theme_minimal() +
  xlab('Finhed') +
  ylab('Fejl') +
  ggtitle(bquote(paste(''))) +
  theme(plot.title = element_text(hjust = 0.5, size=20))+ 
  theme(legend.key.size = unit(1.5, 'cm')) +
  theme(axis.title = element_text(size=12)) +
  scale_colour_discrete("") + #Ændre navn på legend
  scale_x_continuous(expand = c(0.01, 0)) #Så plot starter og slutter ved fct (næsten)
  
#Selve time table
df_err #Konstanten er omkring 0.24

set.seed(20214)
df_err1<-matrix(data=NA, nrow=200, ncol=9)
for (i in 1:200){
  ds<-Eulertest()
  for (k in 1:9){
    hold<-abs(ds[1]-ds[k+1])
    df_err1[i,k]<- hold
  }
}
x<-c(2000,1000,500,400,250,200,125,100,80)
df_err<-cbind(colMeans(df_err1),x)
df_err<-as.data.frame(df_err)
colnames(df_err)<-c('Error', 'x')
df_err

set.seed(20215)
df_err1<-matrix(data=NA, nrow=200, ncol=9)
for (i in 1:200){
  ds<-Eulertest()
  for (k in 1:9){
    hold<-abs(ds[1]-ds[k+1])
    df_err1[i,k]<- hold
  }
}
x<-c(2000,1000,500,400,250,200,125,100,80)
df_err<-cbind(colMeans(df_err1),x)
df_err<-as.data.frame(df_err)
colnames(df_err)<-c('Error', 'x')
df_err


