# EJERCICIO RESUELTO ADICIONAL 2 - FUEGO Y COIRONES

# los datos que no tengo:
datos <- read.table("pastos.txt", header = T)
head(datos)
table(datos$condicion,datos$especie)
pastos_abs <- table(datos$condicion,datos$especie)

# miro los datos que sí tengo: tengo frecuencias
# ¿cuáles son las variables?

especie <- c("Festuca","Festuca","Stipa","Stipa")
condicion <- c("vivo","muerto","vivo","muerto")

# cargo las frecuencias según combinación de niveles 
# de las variables
pastos <- c(80,45,141,42)
datos <- data.frame(condicion,especie,pastos)
datos

# fabrico tabla de contingencia
pastos_abs <- xtabs(pastos ~ condicion + especie, data = datos)
pastos_abs

# si quisiera ver la proporcional uso prop.table()
prop.table(pastos_abs)
prop.table(pastos_abs,1)
prop.table(pastos_abs,2)

# si quisiera ver distribución marginal uso addmargins()
addmargins(pastos_abs,c(1,2))

# estudio la asociación calculando el valor de chi cuadrado
res <- chisq.test(pastos_abs)
res$expected
res$observed
res$statistic

# calculo el coeficiente de contingencia de Pearson
C <- sqrt((res$statistic)/(res$statistic+sum(pastos_abs)))
C

# calculo el C* corregido por cantidad de niveles de las variables
Ccorr <- C/sqrt((2-1)/(2))
Ccorr
