
# trabajar con tablas de contingencia (doble entrada) en R requiere orden
# y ENTENDER BIEN qué estamos haciendo, ya que no hay una aproximación
# visual directa parecida a la que hacemos a mano (la hay pero termina resultando
# más complicada todavía...)

# la principal función a usar es table(x, y, useNA= c("no", "ifany", "always"))
# que contabiliza la frecuencia absoluta según niveles de una variable

# para calcular las frecuencias relativas vamos a usar prop.table(x, margin)
# donde x es una tabla y margen puede valer
# 0 para realtivizar al gran total, 1 a filas, 2 a columnas

# para obtener la distribución marginal como vector margin.table(x, margin)
# con valor 1 para filas y 2 para columnas


# con datos crudos --------------------------------------------------------

# basado en material de Martín Paladino
# ejemplo con datos crudos: 10 personas y gusto por 2 tipos de música 
rock <-   c("Sí", "No", "Sí", "No", "No", "Sí", "No", "No", "No", "No")
cumbia <- c("No", "No", "Sí", "Sí", "Sí", "No", "Sí", "Sí", "Sí", "Sí")

table(rock)
table(cumbia)

#tabla de contingencia
table(rock, cumbia)

# para lo que sigue le asignamos un nombre a la tabla de contingencia
tabla <- table(rock, cumbia)

# hacemos tabla de frecuencias relativas al gran total (defecto)
prop.table(tabla)
# a filas y a columnas
prop.table(tabla, margin = 1)
prop.table(tabla, margin = 2)

# para ver la distribución marginal uso la otra función:
margin.table(tabla, margin = 1)
margin.table(prop.table(tabla, margin = 1), margin = 1)
margin.table(prop.table(tabla, margin = 1), margin = 2)

# última función de esta parte: agregar a la tabla las distrib marginales
# para lograr tablas más parecidas a las que hacemos a mano
addmargins(tabla, c(1, 2)) #para frec absoluta
addmargins(prop.table(tabla), c(1,2))


# tengo las frecuencias ---------------------------------------------------

datos <- read.csv("ej1 U2.csv")

# uso la función xtabs que me permite explicitar de dónde provienen
# las frecuencias observadas
ej1_abs <- xtabs(numero ~ ejercicio + genero, data = datos)
ej1_abs

prop.table(ej1_abs)
prop.table(ej1_abs, margin = 2)
addmargins(prop.table(ej1_abs,margin=2),margin = c(1,2))

# cuantificar contingencia ------------------------------------------------

# chi2
?chisq.test
chisq.test(ej1_abs)

# si le doy un nombre puedo manejar mejor lo que tiene dentro
res <- chisq.test(ej1_abs)
res$observed
res$expected

# coeficiente de contingencia
C <- sqrt((res$statistic)/(res$statistic+sum(ej1_abs)))
C

# Coeficiente corregido: C*
Ccorr <- C/sqrt((2-1)/(2))
Ccorr


# entendiendo el oeficiente de contingencia -------------------------------
# juguemos con situaciones hipotéticas:
C <- sqrt((chi2)/(chi2+n))
Ccorr <- C/sqrt((min-1)/(min))

# un chi2 bajo con un n bajo:
chi2 <- 1
n <- 5
min <- 2
C # 0.41
Ccorr # 0.58

# un chi bajo con un n alto:
chi2 <- 1
n <- 50
min <- 2
C # 0.14
Ccorr # 0.20
# el chi2 es el mismo que antes, pero en un n más alto 
# ese valor habla de menos asociación!

# ahora con chi2 alto y n bajo:
chi2 <- 100
n <- 5
min <- 2
C # 0.98
Ccorr # 1.38
# un chi alto con n bajo habla de mucha asociación (mucho desvío
# de valores esperados)

# y chi2 alto y n alto:
chi2 <- 100
n <- 50
min <- 2
C # 0.82
Ccorr # 1.15
# pero si el n es alto, no es tanta la asociación porque en más
# mediciones se pudieron haber acumulado diferencias