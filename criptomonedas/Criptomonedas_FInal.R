##############################################################

# ANALISIS FINANCIERO DEL MERCADO DE LAS CRIPTOMONEDAS, XRP ADA
# Y ETHEREUM EN ESTE CASO. EL PERÍODO COMPRENDIDO ENTRE EL 1-09- 
# 2019 Y EL 1-09-2021. NUESTRA VARIABLE CATEGÓRICA SERÁ EL COVID
# PARA VER SU IMPACTO EN ESTE TIPO DE DIVISAS.

##############################################################

# Para recoger datos actuales de las criptomonedas cargamos el paquete 
# coinmarketcapr y solicitamos en su web después de darnos de alta una 
# key para obtener datos actualizados.

install.packages("devtools")
install.packages("coinmarketcapr")
install.packages("Hmsic")
install.packages("corrplot")
library(corrplot)
library(Hmisc)
library(ggplot2)
library(coinmarketcapr)
library(tidyverse)
key <- "32d3cc1d-379a-4a3d-8220-93be4e034ef4"
coinmarketcapr::setup(key)

###############################################################
# Obtenemos una vision general del mercado de criptodivisas.

criptos <- get_crypto_listings(limit=50)

marketcap<- get_global_marketcap()
View(marketcap)

# devtools::install_github("amrrs/coinmarketcapr")

###############################################################
###############################################################

# TOP 10 de criptomonedas en función de su precio

top_10 <- criptos %>% 
  head(10) %>%
  select(c('name','symbol', 'USD_price','USD_market_cap_dominance'))

# Dibujamos para verlo de forma más intuitiva

top_10<-top_10[order(top_10$USD_market_cap_dominance,decreasing = T),]

top_10 %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, +USD_price), 
               y = USD_price),
               size=0.6, alpha=0.7, fill="lightblue", color="red")+
  coord_flip() 
  
###############################################################

# Añadimos la evolución de los precios en los últimos 90 días para conocer 
# la tendencia de este mercado:

top_10_2 <- criptos %>% 
  head(10) %>% 
  select(c('name','symbol', 'USD_price','USD_market_cap_dominance',
           'USD_percent_change_90d'))

# Pasamos a representar gráficamente esa evolución y vemos la tendencia 
# positiva, llamando especialmente la atención SOLANA.

top_10_2 %>% 
  ggplot()+
  geom_col(aes(x=reorder(name, -USD_percent_change_90d),y=USD_percent_change_90d),
           col="blue", fill="lightblue") +
  labs(title="90 Days Growth", x="Coins", y="Growth percentage")

###############################################################

# DOMINANCE CON SECTORES: No obstante, este grafico no lo incluimos en la 
# presentación ya que no aporta nada nuevo.

pie(top_10_2$USD_market_cap_dominance, top_10_2$name, main="Cuota de Mercado",
    col= rainbow(length(top_10_2$USD_market_cap_dominance)))


# Cargamos los datos extraidos en un CSV de la evolución de los precios de 
# apertura de nuestras 4 criptomonedas en el periodo comprendido entre el 1 
# de septiembre de 2019 y el mismo dia de 2021.

datos_crypto<-read.csv("~/Desktop/FUNDAMENTOS_DS/TRABAJO/crypto.csv")
names(datos_crypto)
attach(datos_crypto)
nrow(datos_crypto)
ncol(datos_crypto)

#Comparativa de principio y final del periodo, interesante comentarla:

head(datos_crypto)
tail(datos_crypto)


##############################################################

# El 11 de marzo la OMS declaró coronavitrus como Pandemia, creamos variable 
# categórica de ese día, que cae en nuestra fila 192.

once_marzo <- 192 
covid_crypto <- c(rep(0,once_marzo),rep(1,nrow(datos_crypto)-once_marzo)) 
covid_crypto
pre_covid <- c(2:once_marzo)

##############################################################
# Calculamos los rendimientos de cada criptomoneda durante todo el periodo:

n <- dim(datos_crypto)[1]
n

RBTC_todo <- btc[2:n] / btc[1:n-1] - 1  
RETH_todo <- eth[2:n] / eth[1:n-1] - 1
RXRP_todo <- xrp[2:n] / xrp[1:n-1] - 1
RADA_todo <- ada[2:n] / ada[1:n-1] - 1


covid_crypto_rendimientos <- covid_crypto[2:n]
covid_crypto_rendimientos <-as.factor(covid_crypto_rendimientos)

# Calculamos los rendimientos separados por periodos:

# PRECOVID

RBTC_precovid <- btc[2:once_marzo] / btc[1:once_marzo-1] - 1  
RETH_precovid <- eth[2:once_marzo] / eth[1:once_marzo-1] - 1
RXRP_precovid <- xrp[2:once_marzo] / xrp[1:once_marzo-1] - 1
RADA_precovid <- ada[2:once_marzo] / ada[1:once_marzo-1] - 1

covid_cripto_precovid <- cbind(RBTC_precovid,RETH_precovid,RXRP_precovid,RADA_precovid)

Rendimientos_crypto_precovid <- as.data.frame(covid_cripto_precovid)
summary(covid_cripto_precovid)

# POSTCOVID:

RBTC_postcovid <- btc[192:n] / btc[192:n-1] - 1  
RETH_postcovid <- eth[192:n] / eth[192:n-1] - 1
RXRP_postcovid <- xrp[192:n] / xrp[192:n-1] - 1
RADA_postcovid <- ada[192:n] / ada[192:n-1] - 1

covid_cripto_postcovid <- cbind(RBTC_postcovid,RETH_postcovid,
                                RXRP_postcovid,RADA_postcovid)

Rendimientos_crypto_postcovid <- as.data.frame(covid_cripto_postcovid)
summary(covid_cripto_postcovid)

##############################################################################

# Querremos comparar la evolucion de los precios en una grafica, para ello 
# tenemos que normalizar dividiendo entre sus maximos:

btc_norm<-btc/max(btc)
eth_norm<-eth/max(eth)
xrp_norm<-xrp/max(xrp)
ada_norm<-ada/max(ada)

# Creamos un data frame con los precios normalizamos:

datos_crypto_norm <- data.frame(Date=as.Date(datos_crypto$Date, format="%d/%m/%Y"),
                                btc_norm=btc_norm,
                                eth_norm=eth_norm,
                                xrp_norm=xrp_norm,
                                ada_norm=ada_norm)

# Miramos que no haya datos raros ni duplicados en nuestro df. Debido a la 
# peculiaridad de ggplot, tendremos que pivotar nuestro data frame. 

table(duplicated(datos_crypto_norm$Date))
dim(datos_crypto_norm)
head(datos_crypto_norm)
tail(datos_crypto_norm)

# Pivotamos nuestro data frame para posteriormente poder dibujarlo:

datos_crypto_norm_longer <- pivot_longer(datos_crypto_norm, cols =
                                       c(btc_norm,eth_norm,xrp_norm,ada_norm),
                                       values_to = "normalizada", names_to = "Crypto")

# Dibujamos nuestra gráfica con la evolución de los precios: 

ggplot(datos_crypto_norm_longer) +
  geom_line(aes(x=Date,y=normalizada, col=Crypto))+
   scale_x_date(date_breaks = "1 month")

# Se observa que todas siguen una misma tendencia, siendo el primero que 
# cambia de tendencia Bitcoin y el resto siguiendola. Esto lo demostraremos
# mas adelante.

# Suavizamos la grafica:
  
ggplot(datos_crypto_norm_longer) +
  geom_smooth(aes(x=Date,y=normalizada, col=Crypto))+
  scale_x_date(date_breaks = "1 month")

##############################################################################

# Ahora comparamos los precios sin normalizar para comentarlos junto con 
# head y tail de nuestro data frame original:

par(mfrow=c(2,2))

plot(btc,type = "l", col="lightgreen", main = "Precio Bitcoin")

plot(eth,type="l",col="lightblue", main = "Precio Ethereum")

plot(xrp,type="l", col="orange", main ="Precio XRP")

plot(ada,type="l", main ="Precio ADA")

##############################################################################

# Necesitamos un dataframe con todos nuestros rendimientos juntos:

Rendimientos_crypto <- cbind(RBTC, RETH , RXRP, RADA)
Rendimientos_crypto <- as.data.frame(Rendimientos_crypto)
summary(Rendimientos_crypto)


# Vemos en un diagrama de cajas y bigotes la diferencia y la mayor dispersion 
# que nos encontramos en el periodo posterior al 11 de marzo:

par(mfrow=c(2,2))
boxplot(RBTC ~ covid_crypto_rendimientos, col="lightgreen", xlab = "Rendimientos")
boxplot(RETH ~ covid_crypto_rendimientos, col="lightblue", xlab="Rendimientos")
boxplot(RXRP ~ covid_crypto_rendimientos, col="orange", xlab="Rendimientos")
boxplot(RADA ~ covid_crypto_rendimientos, xlab="Rendimientos") 

##############################################################################
##############################################################################

# Creamos histogramas de todo el periodo para ver como los rendimientos de todas
# nuestras monedas se comportan de forma parecida y se concentran en 0.

par(mfrow=c(2,2))  

hist(RBTC,prob=T,main="Rendimientos BTC", ylim= c (0,10), col="lightgreen")
lines(density(RBTC), col="red")
box()
hist(RETH,prob=T,main="Rendimientos ETH", ylim= c (0,10), col= "lightblue")
lines(density(RETH), col="red")
box()
hist(RXRP,prob=T,main="Rendimientos XRP", ylim= c (0,10), col="orange")
lines(density(RXRP), col="red")
box()
hist(RADA,prob=T,main="Rendimientos ADA", ylim = c(0,10))
lines(density(RADA), col="red")
box()

##############################################################################

# PAREJAS DE SERIES: EMmpezamos a visualizar las correlaciones:

pairs(Rendimientos_crypto, panel = panel.smooth, main = "Rendimientos", col="orange")

##############################################################################
# Analizamos los rendimientos con la matriz de correlaciones y de covarianzas:
# Vemos que existe fuerte correlación

cor(Rendimientos_crypto)
cov(Rendimientos_crypto)

##############################################################################

# Vemos si hay correlación con el coeficiente de Pearson:
rh <- rcorr(as.matrix(Rendimientos_crypto),type="pearson") 
rh 
rh$r # SI HAY CORRELATION YA QUE LOS VALORES ESTAN CERCA DE 1

##############################################################################

# Representación gráfica de las correlaciones:

corrplot(rh$r, type = "upper", order="hclust", tl.col="black", tl.srt=45,
         method="pie")

# Queremos demostrar que todas siguen a Bitcoin en su tendencia, por eso
# la utilizamos como variable independiente en este modelo de regresion lineal:

regresion<-lm(RETH_todo ~ RBTC_todo)
summary(regresion)

regresion2<-lm(RADA_todo ~ RBTC_todo)
summary(regresion2)

regresion3 <- lm(RXRP_todo ~ RBTC_todo)
summary(regresion3)


# Representamos esa regresión lineal junta y la expondremos juntos a los p 
# valor anteriores y r cuadrados para reforzar nuestra argumentación:


plot(RBTC_todo, RETH_todo)
abline(regresion, col="red")

plot(RBTC_todo, RADA_todo)
abline(regresion2, col="red")

plot(RBTC_todo, RXRP_todo)
abline(regresion3, col="red")








