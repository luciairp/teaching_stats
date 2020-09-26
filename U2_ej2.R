# ejercicio 2 - flúor 

localidad <- c("A","A","B","B","C","C","D","D")
fluor <- c("debajo","encima","debajo","encima","debajo","encima","debajo","encima")
pozos <- c(32,55,43,65,16,64,9,16)
datos <- data.frame(localidad,fluor,pozos)

# fabrico la tabla del enunciado
ej2_abs <- xtabs(pozos ~ fluor + localidad, data = datos)

# le agrego los marginales
addmargins(ej2_abs,margin = c(1,2))

# para saber si hay asociación necesito construir la tabla de 
# frecuencias esperadas bajo independencia de la localidad

# si no existe asociación entre estas variables, esperaría que
# en cada localidad se respetara la distribución marginal del 
# nivel de flúor:

# en el total de 300 pozos estudiados hubo 100 con niveles de F
# por debajo de lo permitido, y 200 por encima

(prop_esperada_debajo <- 100/300)
(prop_esperada_encima <- 200/300)

# también podemos pedirlo a R así:
margin.table(prop.table(ej2_abs),margin = 1)

# para construir el valor esperado tenemos que multiplicar
# la proporción esperada por la cantidad de pozos de cada localidad

(esperado_debajo_A <- 0.33*87)
(esperado_encima_A <- 0.67*87)

(esperado_debajo_B <- 0.33*108)
(esperado_encima_B <- 0.67*108)

# y así para cada valor...

# para fabricar el chi cuadrado a mano tenemos que comparar
# las frecuencias observadas (datos) contra las esperadas

# la suma de los desvíos al cuadrado dividido n dan el valor 
# del estadístico chi2

# en R lo hacemos con la función chisq.test()
# para poder acceder a toda la información que calcula le asignamos
# un nombre

res <- chisq.test(ej2_abs)

# para ver todo lo que calcula pueden usar la ayuda con el acceso
?chisq.test
# miren en la sección "value"

# usando el nombre y lo que sé que calcula la función puedo acceder
# a esos elementos, por ejemplo:
# si le pido que me muestre la tabla de frecuencias observadas:
res$observed

# y al de frecuencias esperadas bajo independencia:
res$expected

# si le pido el estadístico me da el valor directamente
res$statistic

# calculo el coeficiente de contingencia
# que usa el valor de chi2 y la cantidad de datos tomados
C <- sqrt((res$statistic)/(res$statistic+sum(ej2_abs)))
C

# si quiero el C* uso la cantidad de niveles mínima entre filas
# y columnas: en este caso las localidades tienen 4 niveles (A, B, C D)
# y las categorías de pozos de agua 2 niveles: por encima y por debajo
# uso el mínimo entre 2 y 4: 2
# Divido al C por la raíz cuadrada de 2-1/2
Ccorr <- C/sqrt((2-1)/(2))
Ccorr
