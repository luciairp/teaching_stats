# Prueba independencia y homogeneidad -------------------------------------

# Retomamos ejercicio adicional de la unidad 2
# Pastos en un coironal, miramos si existía asociación entre la especie y su
# tolerancia al fuego. A partir de puntos al azar se registró el coirón más
# cercano y estas dos variables: especie (Festuca o Stipa) y si estaba vivo o muerto.

#
datos <- read.table("pastos.txt", header = T)
head(datos)
pastos_abs <- table(datos$condicion,datos$especie)

# estudio la asociación calculando el valor de chi cuadrado
res <- chisq.test(pastos_abs)

res$expected
res$observed
res$statistic

# ¡Ahora sí podemos mirar el objeto resultados completo!
res 

# Coeficiente de contingencia de Pearson
C <- sqrt((res$statistic)/(res$statistic+sum(pastos_abs)))

# C* corregido por cantidad de niveles de las variables
Ccorr <- C/sqrt((2-1)/(2))



# Homogeneidad ------------------------------------------------------------

# ¿Por qué es diferente?
# Quiero saber si la respuesta al fuego es homogénea entre Festuca y Stipa...
# En mi diseño ahora tomo 50 de cada especie, y me fijo si vivos o muertos
# La cantidad de Festuca y Stipa es fija, marginales fijos
# La pregunta es diferente: ¿responden igual las dos especies?
# Lo que hacemos es parecido, por eso ¡¡¡cuidado!!!


datos_homog <- read.table("nuevospastos.txt", header = T)
head(datos_homog)
pastos_homog_abs <- table(datos_homog$condicion,datos_homog$especie)

res_homog <- chisq.test(pastos_homog_abs)
res_homog
