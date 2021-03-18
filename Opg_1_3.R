source('Monte-Carlo.R')

#Opg 1.3d)
plt_delta(1/10,1/100,1/1000,1/2000) #Plot med forskellige delta under samme tidsperiode

graph(plt()) #Forskelligt delta med forskelligt tidsperiode (Giver vel ikke mening)


#Opg 1.3e)
cv(n=1000, delta_t=1/500, k=500, mu2=0.03)
monte(n=1000, delta_t=1/500, k=500, mu=0.07, sigma=0.2)
ant(n=1000/2, delta_t=1/500, k=500, mu=0.07, sigma=0.2)
Teo_v(1,mu=0.07, sigma=0.2)



#Plot til at se, hvordan fejlen på monte og anti mindskes når antal simuleringer stiger
#Data til abs
ant_data<-ant_abs_data(points=100, start=50, step=10)
monte_data<-monte_abs_data(points=100, start=50, step=10)
x<-seq(50,1040,10)  #Skal kaldes x, ellers virker plot ikke
sd<-sqrt(1/x *Teo_v(1,mu=0.07, sigma=0.2, mean=F))
H <- as.data.frame(cbind(ant_data, monte_data,sd, x))
df <- H %>%
  select(ant_data,monte_data,sd,x) %>%
  gather(key = "variable", value = "value", -x)


abs_opt_plt(df, title='Størrelse af fejl ved monte og anti', '')




#Test af variance (Loop n gange over estimatoren)
#Simulering
monte_test(n=100, delta_t=1/500, k=500, loop=500)
cv_test(n=1000, delta_t=1/500, k=500, loop=3000, mu2=0.03, sigma2=0.1)
ant_test(n=1000/2, delta_t=1/500, k=500, loop=400)

#Teoretisk
Teo_v(1,mu=0.07, sigma=0.2, mean=F)/100 #Monte
(Teo_v(1,mu=0.07, sigma=0.2, mean=F)-4.50932687558246)/(2*500) #Ant
(Teo_v(1,mu=0.07, sigma=0.2, mean=F)+Teo_v(1,mu=0.03, sigma=0.1, mean=F)-2*2.2275325201913)/1000 #Den burde vel også halveres

