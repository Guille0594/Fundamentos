##############################################################

# ANALISIS FINANCIERO DEL MERCADO DE LAS CRIPTOMONEDAS, XRP ADA
# Y ETHEREUM EN ESTE CASO. EL PERÍODO COMPRENDIDO ENTRE EL 1-09- 
# 2019 Y EL 1-09-2021. NUESTRA VARIABLE CATEGÓRICA SERÁ EL COVID
# PARA VER SU IMPACTO EN ESTE TIPO DE DIVISAS.

##############################################################

###############################################################
###############################################################
###############################################################
###############################################################

# install.packages("devtools")
# install.packages("coinmarketcapr")

library(coinmarketcapr)
library(tidyverse)
key <- "32d3cc1d-379a-4a3d-8220-93be4e034ef4"
coinmarketcapr::setup(key)

###############################################################
###############################################################

# NOW WE HAVE SET UP OUR KEY
# LET'S SEE THE AVAILABLE CRYPTOCURRENCIES

criptos <- get_crypto_listings(limit=50)

marketcap<- get_global_marketcap()
View(marketcap)

# devtools::install_github("amrrs/coinmarketcapr")

###############################################################
###############################################################

# LET'S CHECK THE TOP 10 CRYPTOCURRENCIES TODAY AND PLOT EM

top_10 <- criptos %>% 
  head(10) %>%
  select(c('name','symbol', 'USD_price','USD_market_cap_dominance'))

# PLOTTING THE TOP 10 SEEING NAME AND DOMINANCE 

top_10<-top_10[order(top_10$USD_market_cap_dominance,decreasing = T),]

top_10 %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, +USD_price), y = USD_price, color=USD_market_cap_dominance), 
           size=0.6, alpha=0.7, color="blue", fill="lightblue") +
  coord_flip()+
  labs(x="Criptomoneda", y="Precio")

top_10 %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, +USD_market_cap_dominance ), 
               y = USD_market_cap_dominance), fill="orange",
           color= "lightblue")+
  coord_flip()+
  labs(y="Cuota de mercado", x="Criptomoneda")

###############################################################
###############################################################
###############################################################


# AÑADIMOS USD_percent_change_90d Y VEMOS LA EVOLUCION % DE LAS MONEDAS Y LAS 
# DIBUJAMOS

top_10_2 <- criptos %>% 
  head(10) %>% 
  select(c('name','symbol', 'USD_price','USD_market_cap_dominance','USD_percent_change_90d'))

# VEMOS EL CRECIMIENTO MEDIO EN LOS ÚLTIMOS 90 DÍAS

top_10_2 %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, -USD_percent_change_90d), y=USD_percent_change_90d), col="blue", fill="lightblue")+
  labs(title="90 Days Growth", x="Coins", y="Growth_percentage")+
  labs(x="criptomoneda", y="Crecimiento en %")

###############################################################
###############################################################

# DOMINANCE CON SECTORES

pie(top_10_2$USD_market_cap_dominance, top_10_2$name, main="Dominance",
    col= rainbow(length(top_10_2)))


# CARGAMOS LOS DATOS SEGÚN EL PRECIO DE APERTURA DIARIO:

rm(list=ls())
datos_crypto<-read.csv("~/Desktop/FUNDAMENTOS_DS/TRABAJO/crypto.csv")
names(datos_crypto)
attach(datos_crypto)
head(datos_crypto)
tail(datos_crypto)
datos_crypto

nrow(datos_crypto)
ncol(datos_crypto)

##############################################################

# CREAMOS LA VARIABLE CATEGÓRICA COVID DESDE EL 11 DE MARZO DE 
# 2020 QUE ES CUANDO LA OMS DECLARÓ LA PANDEMIA:

once_marzo <- 192 
covid_crypto<- c(rep(0,once_marzo),rep(1,nrow(datos_crypto)-once_marzo)) 
covid_crypto
fecha_final <- 732

pre_covid <- c(2:once_marzo)

postcovid_cripto <- c(rep(0,once_marzo),rep(1, nrow(datos_crypto)-192))
post_covid <- c(once_marzo + 1:n)


##############################################################
##############################################################

# CALCULO DE LOS RENDIMIENTOS DE CADA CRIPTOMONEDA EN TODO EL PERIODO

n <- dim(datos_crypto)[1]
n

RBTC_todo <- btc[2:n] / btc[1:n-1] - 1  
RETH_todo <- eth[2:n] / eth[1:n-1] - 1
RXRP_todo <- xrp[2:n] / xrp[1:n-1] - 1
RADA_todo <- ada[2:n] / ada[1:n-1] - 1


covid_crypto_rendimientos_totales <- cbind(RBTC_todo,RETH_todo,RXRP_todo,RADA_todo)
rendimientos_totales <-as.factor(covid_crypto_rendimientos_totales)
summary(covid_crypto_rendimientos_totales)

# RENDIMIENTOS ANTES DEL COVID

RBTC_precovid <- btc[2:once_marzo] / btc[1:once_marzo-1] - 1  
RETH_precovid <- eth[2:once_marzo] / eth[1:once_marzo-1] - 1
RXRP_precovid <- xrp[2:once_marzo] / xrp[1:once_marzo-1] - 1
RADA_precovid <- ada[2:once_marzo] / ada[1:once_marzo-1] - 1

covid_cripto_precovid <- cbind(RBTC_precovid,RETH_precovid,RXRP_precovid,RADA_precovid)
Rendimientos_crypto_precovid <- as.data.frame(covid_cripto_precovid)
summary(covid_cripto_precovid)

# RENDIMIENTOS DESPUÉS DEL COVID


RBTC_postcovid <- btc[192:n] / btc[192:n-1] - 1  
RETH_postcovid <- eth[192:n] / eth[192:n-1] - 1
RXRP_postcovid <- xrp[192:n] / xrp[192:n-1] - 1
RADA_postcovid <- ada[192:n] / ada[192:n-1] - 1

covid_cripto_postcovid <- cbind(RBTC_postcovid,RETH_postcovid,
                                RXRP_postcovid,RADA_postcovid)
Rendimientos_crypto_postcovid <- as.data.frame(covid_cripto_postcovid)
summary(covid_cripto_postcovid)


##############################################################################
##############################################################################
##############################################################################

# NORMALIZAMOS LAS VARIABLES #################################################


btc_norm<-btc/max(btc)
eth_norm<-eth/max(eth)
xrp_norm<-xrp/max(xrp)
ada_norm<-ada/max(ada)

# NUEVO DATA FRAME CON LAS MONEDAS NORMALIZADAS ##############################

datos_crypto_norm <- 
  data.frame(Date=as.Date(datos_crypto$Date, format="%d/%m/%Y"),
                                btc_norm=btc_norm,
                                eth_norm=eth_norm,
                                xrp_norm=xrp_norm,
                                ada_norm=ada_norm)

# COMPROBAMOS QUE LA NUEVA DF NO TIENE DUPLICADOS #############################

table(duplicated(datos_crypto_norm$Date))
dim(datos_crypto_norm)
head(datos_crypto_norm)
tail(datos_crypto_norm)

# PIVOTAMOS EL DF PARA TENERLO EN ESTILO TIDY #################################

datos_crypto_norm_longer <- pivot_longer(datos_crypto_norm, cols =
                                       c(btc_norm,eth_norm,xrp_norm,ada_norm),
                                       values_to = "normalizada", names_to = "Crypto")

ggplot(datos_crypto_norm_longer) +
  geom_line(aes(x=Date,y=normalizada, col=Crypto))+
   scale_x_date(date_breaks = "1 month")
  
ggplot(datos_crypto_norm_longer) +
  geom_smooth(aes(x=Date,y=normalizada, col=Crypto))+
  scale_x_date(date_breaks = "1 month")


##############################################################################

# COMPARAMOS LA EVOLUCIÓN DE LOS PRECIOS DE LAS DISTINTAS MONEDAS EN EL TIEMPO

par(mfrow=c(2,2))

plot(btc,type = "l", col="lightgreen", main = "Precio Bitcoin")

plot(eth,type="l",col="lightblue", main = "Precio Ethereum")

plot(xrp,type="l", col="orange", main ="Precio XRP")

plot(ada,type="l", main ="Precio ADA")

##############################################################################

# CREAMOS UN DATA FRAME DE TODOS LOS RENDIMIENTOS JUNTOS Y COMENTAMOS

# TOD0 EL PERIODO

Rendimientos_crypto <- cbind(RBTC_todo, RETH_todo , RXRP_todo, RADA_todo) 
Rendimientos_crypto <- as.data.frame(Rendimientos_crypto)
summary(Rendimientos_crypto)





# COMPARAMOS LOS RENDIMIENTOS ANTES Y DESPUÉS DEL COVID CON BOXPLOTS 

# boxplots ----------------------------------------------------------------

covid_crypto_rendimientos <- covid_crypto[2:n]
covid_crypto_rendimientos <-as.factor(covid_crypto_rendimientos)


library(ggplot2)
par(mfrow=c(2,2))
boxplot(RBTC_todo ~ covid_crypto_rendimientos, col="lightgreen", xlab = "Rendimientos")
boxplot(RETH_todo ~ covid_crypto_rendimientos, col="lightblue", xlab="Rendimientos")
boxplot(RXRP_todo ~ covid_crypto_rendimientos, col="orange", xlab="Rendimientos")
boxplot(RADA_todo ~ covid_crypto_rendimientos, xlab="Rendimientos") 

##############################################################################

# PODEMOS HACER LO DE ANTES PERO DE UNA
# par(mfrow=c(2,2))

for (j in 1:4) {
  boxplot(Rendimientos_crypto[,j]
          ~covid_crypto_rendimientos,main=colnames(Rendimientos_crypto)[j],
          ylab="",xlab="",col="orange") }

##############################################################################
##############################################################################

# HISTOGRAMAS

par(mfrow=c(2,2))  

hist(RBTC_todo,prob=T,main="Rendimientos BTC", ylim= c (0,10))
lines(density(RBTC))
box()
hist(RETH_todo,prob=T,main="Rendimientos ETH", ylim= c (0,10))
lines(density(RETH))
box()
hist(RXRP_todo,prob=T,main="Rendimientos XRP", ylim= c (0,10))
lines(density(RXRP))
box()
hist(RADA_todo,prob=T,main="Rendimientos ADA", ylim = c(0,10))
lines(density(RADA))
box()

##############################################################################

# PAREJAS DE SERIES:

plot(RADA_todo,RBTC_todo)

pairs(Rendimientos_crypto, panel = panel.smooth, main = "Rendimientos", col="orange")

##############################################################################

# PASAMOS A VER LA CORRELACIÓN QUE HAY ENTRE LAS VARIABLES

# TOTALES
cor(Rendimientos_crypto)
cov(Rendimientos_crypto)

# PRECOVID
cor(Rendimientos_crypto_precovid)
cov(Rendimientos_crypto_precovid)

# POSTCOVID
cor(Rendimientos_crypto_postcovid)
cov(Rendimientos_crypto_postcovid)

##############################################################################

install.packages("Hmsic")
library(Hmisc)

rh <- rcorr(as.matrix(Rendimientos_crypto),type="pearson") 

rh   # SI HAY CORRELATION YA QUE LOS VALORES ESTAN CERCA DE 1

rh$r

corrplot(rh$r, type = "upper", order="hclust", tl.col="black", tl.srt=50)


##############################################################################

# REPRESENTATION GRÁFICA DE LAS CORRELACIONES ################################

install.packages("corrplot")

library(corrplot)

corrplot(rh$r, type = "upper", order="hclust", tl.col="black", tl.srt=45)


# MODELOS DE REGRESIÓN SIMPLE MEDIANTE LM ####################################

regresion<-lm(RETH_todo ~ RBTC_todo)
summary(regresion)

regresion2<-lm(RADA_todo ~ RBTC_todo)
summary(regresion2)

regresion3 <- lm(RXRP_todo ~ RBTC_todo)
summary(regresion3)


# Grafico Regresion Simple con ggplot2 


plot(RBTC_todo, RETH_todo)
abline(regresion, col="red")

plot(RBTC_todo, RADA_todo)
abline(regresion2, col="red")

plot(RBTC_todo, RXRP_todo)
abline(regresion3, col="red")




# PREDICCIÓN A PARTIR DEL MODELO DE RL

predict(regresion, data.frame(RBTC_todo=RBTC_todo*1.40))
boxplot(RBTC_todo)









