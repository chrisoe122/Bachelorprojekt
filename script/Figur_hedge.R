source('Hedge.R')

##### Figur 10a 
##Rigtig sigma
#Data
data_real<-delta_er_data(1000, 0.3, 0.3, 0.3)

lm(log(data_real[,2])~log(data_real[,1])) #For at lave linje

#Plot
ggplot(data_real, aes(data_real[,1],data_real[,2])) + geom_point() +
  geom_function(fun = function(x) exp(-2.1064)*x^(-0.5)) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab('Hedge Points') +
  ylab('Standard error')+
  theme_minimal()



##### Figur 10b
##Forkert sigma
#Data
data_w <- delta_er_data(1000, 0.1, 0.1, 0.3)

lm(log(data_w[,2])~log(data_w[,1]))

#Plot
ggplot(data_real, aes(data_w[,1],data_w[,2])) + geom_point() +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab('Hedge Points') +
  ylab('Standard error') +
  ylim(0,0.02) +
  theme_minimal()




##### Figur 11a
#Rigtig sigma
ddd1<-matrix(data=NA, nrow=1000,ncol=2)
for (i in 1:1000){
  ddd1[i,]<-hedge(10,7,0.02,0.3,0.3, 0.3,0.1,1000, sd=T, sti=T)
}

#Data
dataf11<-data.frame(ddd1[,1],ddd1[,2])
#Plot
ggplot(dataf11, aes(ddd1[,1],ddd1[,2])) + geom_function(fun = function(x) pmax(x-7,0), col='blue', size=1) +
  geom_point() +
  xlab(expression('S'[T])) +
  ylab('Værdi') +
  ggtitle('') + 
  theme_minimal()


##### Figur 11b
#Forkert sigma
ddd<-matrix(data=NA, nrow=1000,ncol=2)
for (i in 1:1000){
  ddd[i,]<-hedge(10,7,0.02,0.3,0.3, 0.1,0.1,1000, sd=T, sti=T)
}

#Data
dataf1<-data.frame(ddd[,1],ddd[,2])
#Plot
ggplot(dataf1, aes(ddd[,1],ddd[,2])) + geom_function(fun = function(x) pmax(x-7,0), col='blue', size=1) +
  geom_point() +
  xlab(expression('S'[T])) +
  ylab('Værdi') +
  ggtitle('') +
  theme_minimal()



##### Figur 12a
dataasd_iv<-loop(250,10,0.2,0.1,0.1)
graph_delta(dataasd_iv)




##### Figur 12b
dataasd_m<-loop(250,10,0.2,0.1,0.2)
graph_delta(dataasd_m)
exp(0.02*1)*(V_c(10,11,0.02,1,0.2)-V_c(10,11,0.02,1,0.1))