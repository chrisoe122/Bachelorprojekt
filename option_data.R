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


data1$apr.16.2021$calls$Last[data1$apr.16.2021$calls$Strike==2400]

nytdata_eu<- data1$apr.16.2021$calls$Last[data1$apr.16.2021$calls$Vol>20]
nytdata_eu
data1$apr.16.2021$calls
hold <-rep(NA,18)
for (i in 1:18){
  hold[i]<-data1$apr.16.2021$calls$Last[data1$apr.16.2021$calls$Strike==2400+i*100]
}


#### IV SMILE

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
      a[i]<-newton(V_c, 0.02, 0.04, S=S, K=K1, r=r, tau=1/262, v=value, term=200) #Hvorfor kan den ikke lide når gæt er 0.2 og 0.4?
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


#Call
call_y2<-matrix(data=iv_2(n=50, start=1, step=1, data=data1, S=3913, r=0.0007), nrow = 50, ncol = 2)
call_y2
call_y_l2<-call_y2[c(-1,-4),] #Fjerne outliers
call_y_l2

#plot
call_da_2<-data.frame(call_x2, call_y_l2[,1])
colnames(call_da_2)<-c('x','y')
call2<-iv_plt(call_da_2, '', 'sd')
call2





#Put
put_y2<-matrix(data=iv_2(n=35, start=73, step=1, data=data1, S=3913, c=F, r=0.0007), nrow = 35, ncol = 2)
put_y2<- put_y2[-34,]
put_x2<-log(3913/put_y2[,2])
put_y2
#plot
put_da_2<-data.frame(put_x2, put_y2[,1])
colnames(put_da_2)<-c('x','y')
put2<-iv_plt(put_da_2, '', 'sd')
put2


#Samlet plot
grid.arrange(put2, call2)





### værdi af option: Data vs. teoretisk
#Put
data1$mar.22.2021$put
data1$mar.22.2021$put[120,]
V_p(3913,3865,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

data1$mar.22.2021$put[127,]
V_p(3913,3900,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

data1$mar.22.2021$put[137,]
V_p(3913,3950,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

#Call
data1$mar.22.2021$call
data1$mar.22.2021$call[58,]
V_c(3913,3900,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot, men tæt på

data1$mar.22.2021$call[78,]
V_c(3913,4000,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

data1$mar.22.2021$call[68,]
V_c(3913,3950,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot



