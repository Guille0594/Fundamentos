#----------------------------------------------------------------------
# MUCD - Fundamentos 
# An?lisis exploratorio multivariante
# Datos MC 
#----------------------------------------------------------------------
library(readr)
rm(list=ls())
datos<-read.table("~/Desktop/Cotizaciones2020.txt",header=TRUE)
names(datos)
attach(datos)
head(datos)
tail(datos)
datos


nrow(datos)
ncol(datos)

# Variable dummy covid que es el punto de corte para calcular los rendimientos. 

p_break <- 164 #esto es un punto de corte
covid <- c(rep(0,p_break),rep(1,nrow(datos)-p_break)) #para definir la variable categórica covid. 
covid

#----------------------------------------------------------------------
# Cálculo de los Rendimientos, hay que hacerlo como vemos aqui
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1 #el -1 es el truco, los podemos hacer uno por uno. 
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1

covidr <- covid[2:n]
covidr <-as.factor(covidr)

#----------------------------------------------------------------------
# Tiempo continuo y discreto
#----------------------------------------------------------------------
RSAN2  <- log(SAN.MC[2:n] / SAN.MC[1:n-1])

plot(RSAN, RSAN2) #interpretación del gráfico: son el mismo dato, por eso sale asi

# Contraste formal : lo estudiaremos m?s adelante
t.test(RSAN, RSAN2)

#----------------------------------------------------------------------
# Gr?ficos b?sicos
#----------------------------------------------------------------------
plot(SAN.MC,type = "l") #gráfico de los precios del santander
plot(RSAN,type = "l") #gráfico de los rendimientos del santander


#----------------------------------------------------------------------
# Propiedades Estad?sticas
#----------------------------------------------------------------------
Rendimientos<-cbind(RIBEX, RSAN, RBBVA, RREP, RITX,RTL5) #data frame donde juntamos todos los rendimientos
Rendimientos <- as.data.frame(Rendimientos)
summary(Rendimientos) #esto estaría bien como primer comentario


#----------------------------------------------------------------------
# Box-Plot
#----------------------------------------------------------------------

boxplot(RIBEX ~ covidr, col="orange") #compara antes y después del covid
boxplot(RSAN ~ covidr, col="orange") #lo mismo con los rendimientos del santander

# Mediante bucle

par(mfrow=c(2,3)) #porque tenemos 6 activos, divide la pantalla en dos filas y tres columnas. 

# Rendimientos, la j es que me coja la columna J. Aquí coge todo el periodo incluyendo covid

for (j in 1:6) {
  boxplot(Rendimientos[,j],
          main=colnames(Rendimientos)[j],xlab="",col="orange")
} 
# con ,j es una variable con un nombre cualquieracoge las columnas. quitamos el label y lo ponemos en naranja, y lo hacemos de la 
# columna 1 a  la 6. Y el boxplot de ese dataframe es lo que va a dibujar, con rendimientos,
# j valdría. El resto es un adorno

# Rendimientos antes y despues covid, y aquí si se puede compara. 
for (j in 1:6) {
  boxplot(Rendimientos[,j]~covidr,main=colnames(Rendimientos)[j],ylab="",xlab="",col="orange") #aqui hace lo mismo que antes pero con la variable covid, para compararlos
}


# Como vector (este comando sapply, es igual que un for pero sin incluirlo. Creamos una secuencia que es de 1 a 6,
# y luego una funcion: que nos dibuje el bloxpot del rendimiento y luego añadimos lo de antes)

sapply(seq(1,6),function(j) boxplot(Rendimientos[,j],main=colnames(Rendimientos)[j], xlab="",ylab="",col="orange"))

sapply(seq(1,6),function(j) boxplot(Rendimientos[,j]~covidr,main=colnames(Rendimientos)[j], xlab="",ylab="",col="orange"))


par(mfrow=c(1,1))

# Histogramas y kernel (selecci?n) - Creamos una matriz 2x2 de graficos

par(mfrow=c(2,2))
hist(RIBEX,prob=T,main="Rendimientos IBEX")
lines(density(RIBEX))
box()
hist(RSAN,prob=T,main="Rendimientos SANTANDER")
lines(density(RSAN))
box()
hist(RREP,prob=T,main="Rendimientos REPSOL")
lines(density(RREP))
box()
hist(RITX,prob=T,main="Rendimientos DIA")
lines(density(RITX))
box()
par(mfrow=c(1,1))

#----------------------------------------------------------------------
# Parejas de Series
#----------------------------------------------------------------------
plot(RIBEX,RSAN)

pairs(Rendimientos, panel = panel.smooth, main = "Rendimientos", col="orange") #este es el gráfico que vemos en las diapos de clase

#----------------------------------------------------------------------
# Matrices de covarianzas y correlaciones
#----------------------------------------------------------------------
cov(Rendimientos)
cor(Rendimientos) #esta es la matriz de correlaciones, basada en 264 observaciones

install.packages("Hmisc")
library(Hmisc)
rh <- rcorr(as.matrix(Rendimientos),type="pearson") 
rh #siendo 0 se rechaza la hipotesis nula de que no hay relacion, por lo que SI hay correlacion
rh$r
rh$P


#-----------------------------------------------
# Representaci?n gr?fica de la matriz de correlaciones
#-----------------------------------------------

install.packages("corrplot")
library(corrplot)
corrplot(rh$r, type = "upper", order="hclust", tl.col="black", tl.srt=45)

# No significativos
corrplot(rh$r, type = "upper", order="hclust", p.mat=rh$P, sig.level=0.01, insig = "blank")


#-----------------------------------------------------------------
# Diagrama de Estrellas
#-----------------------------------------------------------------

library(TeachingDemos)
stars(Rendimientos[seq(144,207,1),]) #diagrama de estrellas: dibuja las estrellas de cada individuo. , estrellas parecidas son individuos parecidos.
#son estrellas con 6 puntas. Se ibuja el radio en función del valor. 

#----------------------------------------------------------------------
# Hip?tesis de Normalidad : Lo veremos m?s adelante 
#----------------------------------------------------------------------
shapiro.test(RIBEX)
shapiro.test(RSAN)

sapply(seq(1,6),function(j) shapiro.test(Rendimientos[,j])) #para hacer contrasres de normalidad
#los datos de rendimientos nunca son normales. La Ho es que los datos si son normales, si la rechazamos sabemos que no son normales. 



