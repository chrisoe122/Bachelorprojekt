source('Options.R')

#opg 2.1 e)

###Call plot
#Data
rcdata<-r_data(start=0.01, slut=0.05, interval=0.0005, simul=2000)
Scdata<-S_data(start=8, slut=12, interval=0.05, simul=2000)
Tcdata<-T_data(start=0.5, slut=2, interval=0.025, simul=2000)
Sigmacdata<-sigma_data(start=0.1, slut=0.5, interval=0.005, simul=2000)

#Plot
rc<-pris_graf(data=rcdata, xv='r', title='r')
Sc<-pris_graf(data=Scdata, xv='S(t)', title='S(t)')
Sigmac<-pris_graf(Sigmacdata, bquote(sigma), title=bquote(sigma))
Tc<-pris_graf(Tcdata, xv='T', title='T')
grid.arrange(rc,Sc,Sigmac,Tc, top = textGrob('Call option', gp=gpar(fontsize=28,font=1)))


#Put plot
#Data
rpdata<-r_data(start=0.01, slut=0.05, interval=0.0005, simul=2000, c=F)
Spdata<-S_data(start=8, slut=12, interval=0.05, simul=2000, c=F)
Tpdata<-T_data(start=0.5, slut=2, interval=0.025, simul=2000, c=F)
Sigmapdata<-sigma_data(start=0.1, slut=0.5, interval=0.005, simul=2000, c=F)

#Plot
rp<-pris_graf(data=rpdata, xv='r', title='r')
Sp<-pris_graf(data=Spdata, xv='S(t)', title='S(t)')
Sigmap<-pris_graf(Sigmapdata, bquote(sigma), title=bquote(sigma))
Tp<-pris_graf(Tpdata, xv='T', title='T')
grid.arrange(rp, Sp, Sigmap, Tp, top = textGrob('Put option', gp=gpar(fontsize=28,font=1)))
