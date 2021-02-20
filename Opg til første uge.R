#Libraries

library(quantmod)

## Scraping S&P 500 data from Yahoo! Finance. The ticker is GSPC
environment_sp500 <- new.env()
suppressWarnings(getSymbols(c("^GSPC"), env = environment_sp500, src = "yahoo",from = as.Date("1960-01-01"),
                            to = as.Date("2021-02-01")))

sp500 <- environment_sp500$GSPC

## Placing data into a more familiar data structure


df_sp500 <- data.frame(date = index(sp500), coredata(sp500))

## opg. e
logp <- rep(NA,15374-1)
for (i in 1:15374){
  logp[i-1] <- log(df_sp500$GSPC.Close[i])-log(df_sp500$GSPC.Close[i-1])
}
qqnorm(logp)
qqline(logp)
hist(logp, breaks=40)
mlogp<-(logp-mean(logp))/sd(logp)
var(mlogp)
mean(mlogp)
qqnorm(mlogp)
qqline(mlogp)
abline(0,1)
#P? ingen m?de normalfordelt. Taleb har ret! Langt tykkere haler end en normalfordeling.

## opg. f
#Blot mle fra normalfordeling

## opg. g
mean(logp)
var(logp)

#Med andet sigma
m<- 
n<- 15374
sigma<-(-n+sqrt(2*n*m+n^2))/n


#tal fra 2020
logp2020<-logp[15102:15354]
mean(logp2020)
var(logp2020)
#varians og mean er meget st?rre. Variansen skyldes usikkerheden skabt af corona og mean stigning
#skyldes at de rige er blevet rigere (Meget cirkelargument)

#Vores mu er
mu <- mean(logp)+sd(logp)*0.5
mu


# opg. h
#Man antager vel den asymptotiske fordeling. S? er de normalfordelt, ellers har man ingen ide.
#S? f?lger det at mu_1 og sigma har varians sigma^2/n og 2*sigma^2/n
#Varians af mu kan dermed findes ved at kigge i h?ftet.





#link til info
#https://www.statlect.com/fundamentals-of-statistics/normal-distribution-maximum-likelihood