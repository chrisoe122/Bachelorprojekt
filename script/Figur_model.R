#Libraries
library(quantmod)
library(EnvStats)

####Henter data

## Scraping S&P 500 data from Yahoo! Finance. The ticker is GSPC
environment_sp500 <- new.env()
suppressWarnings(getSymbols(c("^GSPC"), env = environment_sp500, src = "yahoo",from = as.Date("1960-01-01"),
                            to = as.Date("2021-02-01")))

sp500 <- environment_sp500$GSPC

## Placing data into a more familiar data structure

df_sp500 <- data.frame(date = index(sp500), coredata(sp500))




##### Figur 1
ggplot(df_sp500, aes(x=date, y=GSPC.Close)) + geom_line(color='red') +
  theme_minimal() +
  xlab('Tid') +
  ylab('Pris') +
  ggtitle('S&P500') +
  theme(legend.key.size = unit(1.5, 'cm')) +
  theme(plot.title = element_text(hjust = 0.5, size=20)) +
  theme(axis.title = element_text(size=12)) +
  scale_colour_discrete(legend) #Ændre navn på legend

##### Figur 2a og tabel 2b
logp <- rep(NA,15374-1)
for (i in 1:15374){
  logp[i-1] <- log(df_sp500$GSPC.Close[i])-log(df_sp500$GSPC.Close[i-1])
}

qqnorm(logp)

#Hele datasæt
mu <- (mean(logp)+var(logp)*0.5)*262
mu
sqrt(var(logp)*262)

#Tal fra 2020
logp2020<-logp[15104:15355]
mu20<- (mean(logp2020)+var(logp2020)*0.5)*262
mu20
sqrt(var(logp2020)*262)
