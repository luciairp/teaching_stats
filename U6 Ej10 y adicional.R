########## Problema guía ##########
#### Ejercicio 10 #######

plot(function(x){dnorm(x,mean=170.8, sd=30.6)}, 90, 300, lwd=2,col='blue',
     ylab = ' ',xlab='colesterol (mg/dl)')
plot(function(x){dnorm(x,mean=181.1, sd=30.8)}, 90, 300, add = T,lwd=2,col='red')
legend('topright',c('hombres','mujeres'),lty=c(1,1),col = c('blue','red'),bty='n')

curve(df(x,df1 = 95,df2 = 84),from=0,to=2)

set.seed(333)
var.test(rnorm(96,mean=170.8, sd=30.6),rnorm(85,mean=181.1, sd=30.8))

############# Otro ejemplo ############
# Se quiere estudiar qué aumenta más la presión sanguínea: 
# (1) resistir las tonterías del cuñado en la comida de Navidad, o 
# (2) estar esperando ansioso por un paquete pedido por internet y, 
# luego de estar toda la tarde en casa, descubrir que el repartidor ha 
# pegado un papel en tu buzón donde dice “Ausente en el momento del reparto”.
# 
# Para ello se seleccionan dos grupos, se les somete a la tortura explicada, 
# y luego se obtienen las presiones sistólicas al finalizar la sesión:

x <- c(104,88,100,98,102,92,96,100,96,96)
y <- c(100,102,96,106,110,110,120,112,112,90)

# ¿Puede considerarse que las presiones medias son iguales en ambos casos?
# ¿Y las varianzas?

t.test(x,y)
var.test(x, y)


datos<- data.frame(as.factor(c(rep(1,length(x)),rep(2,length(y)))),c(x,y))
names(datos)<- c("muestra","variable")

library(ggplot2)
ggplot(datos, aes(col=muestra))+ 
  geom_density(aes(x=variable),adjust=2) 
