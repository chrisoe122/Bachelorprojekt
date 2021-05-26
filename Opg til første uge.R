#Libraries

library(quantmod)
library(EnvStats)


## Scraping S&P 500 data from Yahoo! Finance. The ticker is GSPC
environment_sp500 <- new.env()
suppressWarnings(getSymbols(c("^GSPC"), env = environment_sp500, src = "yahoo",from = as.Date("1960-01-01"),
                            to = as.Date("2021-02-01")))

sp500 <- environment_sp500$GSPC

## Placing data into a more familiar data structure


df_sp500 <- data.frame(date = index(sp500), coredata(sp500))




#############OPG 1.1 #####
ggplot(df_sp500, aes(x=date, y=GSPC.Close)) + geom_line(color='red') +
  theme_minimal() +
  xlab('Tid') +
  ylab('Pris') +
  ggtitle('S&P500') +
  theme(legend.key.size = unit(1.5, 'cm')) +
  theme(plot.title = element_text(hjust = 0.5, size=20)) +
  theme(axis.title = element_text(size=12)) +
  scale_colour_discrete(legend) #Ændre navn på legend














##############OPG 1.2 #####

## opg. e
logp <- rep(NA,15374-1)
for (i in 1:15374){
  logp[i-1] <- log(df_sp500$GSPC.Close[i])-log(df_sp500$GSPC.Close[i-1])
}
qqnorm(logp)

## opg. f
#Blot mle fra normalfordeling

## opg. g
mu <- (mean(logp)+var(logp)*0.5)*252
mu
var(logp)*252



#tal fra 2020
logp2020<-logp[15104:15355]
var(logp2020)*252
#varians og mean er meget st?rre. Variansen skyldes usikkerheden skabt af corona og mean stigning
#skyldes at de rige er blevet rigere (Meget cirkelargument)


#Vores mu er
mu20<- (mean(logp2020)+var(logp2020)*0.5)*250
mu20

# opg. h
#Man antager vel den asymptotiske fordeling. S? er de normalfordelt, ellers har man ingen ide.
#S? f?lger det at mu_1 og sigma har varians sigma^2/n og 2*sigma^2/n
#Varians af mu kan dermed findes ved at kigge i h?ftet.





#link til info
#https://www.statlect.com/fundamentals-of-statistics/normal-distribution-maximum-likelihood