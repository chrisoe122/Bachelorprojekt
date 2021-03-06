source('Monte-Carlo.R')
#Error for de to metoder
monte<-E_S_t(n=10000,delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)
anti<-Ant(n=5000, delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)

error_m<- monte-Teo_v(t=2) 
error_a<- anti-Teo_v(t=2)
error_m
error_a
#Bemærk at n er forskelligt i de to metoder

#Loop for at skabe flere error
j<-rep(NA,10)
for (i in 1:10){
  monte<-E_S_t(n=10000,delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)
  anti<-Ant(n=5000, delta_t=1/500, k=1000, y0=10, mu=0.07, sigma=0.2)
  error_m<- abs(monte-Teo_v(t=2)) 
  error_a<- abs(anti-Teo_v(t=2))
  j[i]<- error_m - error_a
}
#Positiv er m størst, negativ er a størst
mean(j) #Gns af error
j #Burde være positiv
