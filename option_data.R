library(quantmod)
source('Options.R')

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


#Option data
data<- getOptionChain(Symbols = 'SPY', Exp = c("2021", "20222"), data_source="yahoo")
data1 <- getOptionChain(Symbols = '^SPX', Exp = c("2021", "20222"), data_source="yahoo")

data$apr.16.2021
data1$apr.16.2021$calls
data1$apr.16.2021$calls$Strike==3000

data1$apr.16.2021$calls$Last[data1$apr.16.2021$calls$Strike==2400]

nytdata_eu<- data1$apr.16.2021$calls$Last[data1$apr.16.2021$calls$Vol>20]
nytdata_eu
data1$apr.16.2021$calls
hold <-rep(NA,18)
for (i in 1:18){
  hold[i]<-data1$apr.16.2021$calls$Last[data1$apr.16.2021$calls$Strike==2400+i*100]
}

hold


data1$jul.16.2021$puts[100:200,]
data$jul.16.2021$puts

#PLAN ER AT SKRIVE TIL DEM OG SPØRGE

hold1<-rep(NA,18)
for (i in 1:18){
  hold1[i]<-data$apr.16.2021$calls$Last[data$apr.16.2021$calls$Strike==240+i*10]
}
10*hold1-hold  



#### IV SMILE

#Ved ikke hvordan man skifter date, så er nødt til at lave to fkt
iv<-function(n, start, step, data, date, tau, c=T, r=0.04, S){
  a<-rep(NA,n)
  b<-rep(NA,n)
  if(c==T){
    for (i in 1:n){
      ret_data <- na.omit(data$apr.16.2021$calls$Vol>20)
      ck_data <- data$apr.16.2021$calls$Strike[ret_data]
      cl_data <- data$apr.16.2021$calls$Last[ret_data]
      K1<- ck_data[start+i*step]
      value <- cl_data[start+i*step]
      print(value)
      a[i]<-newton(V_c, 0.02, 0.04, S=S, K=K1, r=r, tau=27/365, v=value, term=100) #Hvorfor kan den ikke lide når gæt er 0.2 og 0.4?
      b[i]<-K1
    }
    c<-cbind(a,b)}
  else{
    for (i in 1:n){
      ret_data <- data$apr.16.2021$puts$Vol>20
      pk_data <- na.omit(data$apr.16.2021$puts$Strike[ret_data])
      pl_data <- na.omit(data$apr.16.2021$puts$Last[ret_data])
      K1<- pk_data[start+i*step]
      value <- pl_data[start+i*step]
      print(value)
      a[i]<-newton(V_p, 0.02, 0.04, S=S, K=K1, r=r, tau=27/365, v=value, term=100)
      b[i]<-K1 
    }
    c<-cbind(a,b)}
  return(c)
}
data1$apr.16.2021$puts$Strike[na.omit(data1$apr.16.2021$puts$Vol>20)]
data1$apr.16.2021$puts$Strike
data1$apr.16.2021$puts$Vol>20
data1$apr.16.2021$puts[300:338,]
#call

data1$apr.16.2021$calls[200:400,]
call_y<-matrix(data=iv(n=70, start=1, step=1, data=data1, S=3913, r=0.008), nrow = 70, ncol = 2)
call_y[,]
call_y_l<-call_y[call_y[,1]!=0,] #Fjerne lower limit, som er 0
call_y_lh<- call_y_l[call_y_l[,1]!=1,] #Fjerne higher limit, som er 1

call_x<-log(3913/call_y_lh[,2]) #Nogle pointer er off. Skyldes det newton? Efterforsk.

#plot
call_da<-data.frame(call_x, call_y_lh[,1])
colnames(call_da)<-c('x','y')
call1<-iv_plt(call_da, 'Call, T=apr.16', '')
call1 #Hvad skal jeg gøre (Ingen out of the money)


#Nogle obs. er meget lave/høje 
#Observation 3
data1$apr.16.2021$calls$Strike[131] #Strike passer
data1$apr.16.2021$calls$Last[128:140] #Priser omkring obs 3
data1$apr.16.2021$calls$Last[131] #Selve værdien
V_c(3913,3645,0.04,27/365,0.4) #Kan ikke komme længere ned end omkring 270. 

#Konklusion: Det er 'bare' en lav pris fra markedet af. 



#put
put_y<-matrix(data=iv(n=122, start=30, step=1, data=data1, S=3913, c=F, r=0.006), nrow = 122, ncol = 2)
put_y_l<-put_y[put_y[,1]>0.13,] #Fjerne lower limit, som er 0
put_y_lh<- put_y_l[put_y_l[,1]<0.4,] #Fjerne higher limit, som er 1
put_x <- log(3913/put_y_lh[,2])
put_x<-log(3913/put_y_lh[,2])

noget_data<-data1$apr.16.2021$puts


#plot
put_da<-data.frame(put_x, put_y_lh[,1])
colnames(put_da)<-c('x','y')
put1<-iv_plt(put_da, 'Put, T=apr.16', '')
put1


put_y




#Ny dato
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
      a[i]<-newton(V_c, 0.02, 0.04, S=S, K=K1, r=r, tau=1/260, v=value, term=200) #Hvorfor kan den ikke lide når gæt er 0.2 og 0.4?
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
      a[i]<-newton(V_p, 0.02, 0.04, S=S, K=K1, r=r, tau=1/260, v=value, term=200)
      b[i]<-K1 
    }
    c<-cbind(a,b)}
  return(c)
}


#Call
call_y2<-matrix(data=iv_2(n=50, start=1, step=1, data=data1, S=3913, r=0.0007), nrow = 50, ncol = 2)
call_y2[,]
call_y_l2<-call_y2[call_y2[,1]!=0,] #Fjerne lower limit, som er 0
call_y_lh2<- call_y_l2[call_y_l2[,1]!=1,] #Fjerne higher limit, som er 1
call_y_lh2
call_x2<-log(3913/call_y_lh2[,2]) #Nogle pointer er off. Skyldes det newton? Efterforsk.

#plot
call_da_2<-data.frame(call_x2, call_y_lh2[,1])
colnames(call_da_2)<-c('x','y')
call2<-iv_plt(call_da_2, 'Call, T=22.mar', 'sd')
call2





#Put
put_y2<-matrix(data=iv_2(n=35, start=73, step=1, data=data1, S=3913, c=F, r=0.0007), nrow = 35, ncol = 2)
put_y2[,]
put_y_l2<-put_y2[put_y2[,1]!=0,] #Fjerne lower limit, som er 0
put_y_lh2<- put_y_l2[put_y_l2[,1]!=1,] #Fjerne higher limit, som er 1
put_x2<-log(3913/put_y_lh2[,2])

#plot
put_da_2<-data.frame(put_x2, put_y_lh2[,1])
colnames(put_da_2)<-c('x','y')
put2<-iv_plt(put_da_2, 'Put, T=22.mar', 'sd')
put2


#Samlet plot
grid.arrange(put2, call2)





### værdi af option: Data vs. teoretisk
#Put
data1$mar.22.2021$put
data1$mar.22.2021$put[120,]
V_p(3913,3865,0.0007,1/260,0.12) #Lavere. Passer fint med IV-plot

data1$mar.22.2021$put[127,]
V_p(3913,3900,0.0007,1/260,0.12) #Lavere. Passer fint med IV-plot

data1$mar.22.2021$put[137,]
V_p(3913,3950,0.0007,1/260,0.12) #Lavere. Passer fint med IV-plot

#Call
data1$mar.22.2021$call
data1$mar.22.2021$call[58,]
V_c(3913,3900,0.0007,1/260,0.12) #Lavere. Passer fint med IV-plot, men tæt på

data1$mar.22.2021$call[78,]
V_c(3913,4000,0.0007,1/260,0.12) #Lavere. Passer fint med IV-plot

data1$mar.22.2021$call[68,]
V_c(3913,3950,0.0007,1/260,0.12) #Lavere. Passer fint med IV-plot



