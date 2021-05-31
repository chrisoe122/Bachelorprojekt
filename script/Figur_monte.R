source('Monte-Carlo.R')


##### Figur 3a

#Data
df_err1<-matrix(data=NA, nrow=200, ncol=9)
for (i in 1:200){
  ds<-Eulertest()
  for (k in 1:9){
    hold<-abs(ds[1]-ds[k+1])
    df_err1[i,k]<- hold
  }
}

df_err<-cbind(colMeans(df_err1),x) #Tager gns. af kolonner

#Plot
df_err<-as.data.frame(df_err)
colnames(df_err)<-c('Error', 'x')
x<-c(2000,1000,500,400,250,200,125,100,80)
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

df_err #Konstanten er omkring 0.24



##### Figur 3b
#Første seed
set.seed(2021)
df_err1<-rep(NA,9)
ds<-Eulertest()
for (k in 1:9){
  hold<-abs(ds[1]-ds[k+1]) #Sammenlign med den teoretiske værdi
  df_err1[k]<- hold
}
df_err1

#Anden seed
df_err2<-rep(NA,9)
ds<-Eulertest()
for (k in 1:9){
  hold<-abs(ds[1]-ds[k+1])
  df_err2[k]<- hold
}
df_err2

#Tredje seed
df_err3<-rep(NA,9)
ds<-Eulertest()
for (k in 1:9){
  hold<-abs(ds[1]-ds[k+1])
  df_err3[k]<- hold
}
df_err3




##### Figur 4a
set.seed(20213)
#Data
ant_data<-ant_abs_data(points=100, start=50, step=10)
monte_data<-monte_abs_data(points=100, start=50, step=10)
#Graf
x<-seq(50,1040,10)  #Skal kaldes x, ellers virker plot ikke
H <- as.data.frame(cbind(ant_data, monte_data, x))
colnames(H)<-c('AV','MC', 'x')
df <- H %>%
  select(AV, MC,x) %>%
  gather(key = "variable", value = "value", -x)
abs_opt_plt(df, title='', '')

######### Tabel 4b
set.seed(20213)
monte(n=100, delta_t=1/500, k=500, mu=0.07, sigma=0.2, Euler=F)
ant(n=50, delta_t=1/500, k=500, mu=0.07, sigma=0.2, Euler=F)
#Ændr n for at få de resterende værdier.
