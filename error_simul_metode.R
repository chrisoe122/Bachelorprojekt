source('Monte-Carlo.R')
#Error for de to metoder
monte<-E_S_t(10000,0,10,1/500,0,1000,0.07,0.2)
anti<-Ant(5000,0,10,1/500,0,1000,0.07,0.2)

error_m<- monte-Teo_v(2) 
error_a<- anti-Teo_v(2)
error_m
error_a
#Bemærk at n er forskelligt i de to metoder

#Loop for at skabe flere error
j<-rep(NA,10)
for (i in 1:10){
  monte<-E_S_t(10000,0,10,1/500,0,1000,0.07,0.2)
  anti<-Ant(5000,0,10,1/500,0,1000,0.07,0.2)
  error_m<- abs(monte-Teo_v(2)) 
  error_a<- abs(anti-Teo_v(2))
  j[i]<- error_m - error_a
}
#Positiv er m størst, negativ er a størst
mean(j) #Gns af error
j #Burde være positiv
