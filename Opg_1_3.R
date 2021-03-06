source('Monte-Carlo.R')

#Opg 1.3d)
plt_delta(1/10,1/100,1/1000,1/2000) #Plot med forskellige delta under samme tidsperiode

graph(plt()) #Forskelligt delta med forskelligt tidsperiode (Giver vel ikke mening)


#Opg 1.3e)
cv(1000,1/500,500, mu2=0.05)
E_S_t(1000,1/500,500, mu=0.07, sigma=0.2)
Ant(1000,1/500,500, mu=0.07, sigma=0.2)
Teo_v(1,mu=0.07, sigma=0.2)



