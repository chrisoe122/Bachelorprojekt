source('Options.R')

#opg 2.1 e)

###Call plot
#Data
rcdata<-r_data(start=0.01, slut=0.05, interval=0.0005, simul=500)
Scdata<-S_data(start=8, slut=12, interval=0.05, simul=500)
Tcdata<-T_data(start=0.5, slut=2, interval=0.025, simul=500)
Sigmacdata<-sigma_data(start=0.1, slut=0.5, interval=0.005, simul=500)

#Plot
rc<-pris_graf(data=rcdata, xv='r', title='r')
Sc<-pris_graf(data=Scdata, xv='S(t)', title='S(t)')
Sigmac<-pris_graf(Sigmacdata, bquote(sigma), title=bquote(sigma))
Tc<-pris_graf(Tcdata, xv='T', title='T')
grid.arrange(rc,Sc,Sigmac,Tc, top = textGrob('Call option', gp=gpar(fontsize=28,font=1)))


#Put plot
#Data
rpdata<-r_data(start=0.01, slut=0.05, interval=0.0005, simul=500, c=F)
Spdata<-S_data(start=8, slut=12, interval=0.05, simul=500, c=F)
Tpdata<-T_data(start=0.5, slut=2, interval=0.025, simul=500, c=F)
Sigmapdata<-sigma_data(start=0.1, slut=0.5, interval=0.005, simul=500, c=F)

#Plot
rp<-pris_graf(data=rpdata, xv='r', title='r')
Sp<-pris_graf(data=Spdata, xv='S(t)', title='S(t)')
Sigmap<-pris_graf(Sigmapdata, bquote(sigma), title=bquote(sigma))
Tp<-pris_graf(Tpdata, xv='T', title='T')
grid.arrange(rp, Sp, Sigmap, Tp, top = textGrob('Put option', gp=gpar(fontsize=28,font=1)))



#Udregning af abs fejl ved monte, anti og cv i forhold til teoretisk (Stigende n)
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
