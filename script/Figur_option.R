source('Options.R')

##### Figur 5
monte_data_opt<-monte_abs_opt(points=50, start=250, step=25)
ant_data_opt<-ant_abs_opt(points=50, start=250, step=25)
cv_data_opt<-cv_abs_opt(points=50, start=250, step=25)
x<-seq(250,1475,25)
H<-as.data.frame(cbind(ant_data_opt,monte_data_opt, cv_data_opt, x))
colnames(H)<-c('AV', 'MC', 'CV', 'x')
df <- H %>%
  select(AV,MC, CV, x) %>%
  gather(key = "variable", value = "value", -x)
abs_opt_plt(df, title='', legend='')


##### Figur 6
#Data
rcdata<-r_data(start=0.01, slut=0.05, interval=0.0005, simul=500)
Scdata<-S_data(start=8, slut=12, interval=0.05, simul=500)
Tcdata<-T_data(start=0.5, slut=2, interval=0.025, simul=500)
Sigmacdata<-sigma_data(start=0.1, slut=0.5, interval=0.005, simul=500)

#Plot
rc<-pris_graf(data=rcdata, xv='r', title='r')
Sc<-pris_graf(data=Scdata, xv=expression('S'[0]), title=expression('S'[0]))
Sigmac<-pris_graf(Sigmacdata, bquote(sigma), title=bquote(sigma))
Tc<-pris_graf(Tcdata, xv='T', title='T')
grid.arrange(rc,Sc,Sigmac,Tc, top = textGrob('Call-option', gp=gpar(fontsize=28,font=1)))



##### Figur 7
#Data
rpdata<-r_data(start=0.01, slut=0.05, interval=0.0005, simul=500, c=F)
Spdata<-S_data(start=8, slut=12, interval=0.05, simul=500, c=F)
Tpdata<-T_data(start=0.5, slut=2, interval=0.025, simul=500, c=F)
Sigmapdata<-sigma_data(start=0.1, slut=0.5, interval=0.005, simul=500, c=F)

#Plot
rp<-pris_graf(data=rpdata, xv='r', title='r')
Sp<-pris_graf(data=Spdata, xv=expression('S'[0]), title=expression('S'[0]))
Sigmap<-pris_graf(Sigmapdata, bquote(sigma), title=bquote(sigma))
Tp<-pris_graf(Tpdata, xv='T', title='T')
grid.arrange(rp, Sp, Sigmap, Tp, top = textGrob('Put-option', gp=gpar(fontsize=28,font=1)))




######## DATA

#Option data
#Hentet d. 21 marts
data1 <- getOptionChain(Symbols = '^SPX', Exp = c("2021", "20222"), data_source="yahoo")

##### Tabel 8
#Put
#Strike 3865
data1$mar.22.2021$put[120,]
V_p(3913,3865,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

#Strike 3900
data1$mar.22.2021$put[127,]
V_p(3913,3900,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

#Strik 3950
data1$mar.22.2021$put[137,]
V_p(3913,3950,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

#Call
#Strike 3900
data1$mar.22.2021$call[58,]
V_c(3913,3900,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

#Strike 3950
data1$mar.22.2021$call[68,]
V_c(3913,3950,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot

#Strike 4000
data1$mar.22.2021$call[78,]
V_c(3913,4000,0.0007,1/262,0.12) #Lavere. Passer fint med IV-plot


##### Figur 9a
#Data
call_y2<-matrix(data=iv_2(n=50, start=1, step=1, data=data1, S=3913, r=0.0007), nrow = 50, ncol = 2)
call_y2
call_y_l2<-call_y2[c(-1,-4),] #Fjerner outliers
call_y_l2

#plot
call_da_2<-data.frame(call_x2, call_y_l2[,1])
colnames(call_da_2)<-c('x','y')
call2<-iv_plt(call_da_2, '', 'sd')




##### Figur 9b
#Data
put_y2<-matrix(data=iv_2(n=35, start=73, step=1, data=data1, S=3913, c=F, r=0.0007), nrow = 35, ncol = 2)
put_y2<- put_y2[-34,] #Fjerner outliers
put_x2<-log(3913/put_y2[,2])

#plot
put_da_2<-data.frame(put_x2, put_y2[,1])
colnames(put_da_2)<-c('x','y')
put2<-iv_plt(put_da_2, '', 'sd')
put2


