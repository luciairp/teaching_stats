############### distribuciones #######################

###############    discretas   #######################

# Es posible calcular probabilidades para variables 
# aleatorias discretas:
# Binomial: binom
# Hipergeométrica: hyper
# Poisson: pois
# Binomial negativa: nbinom
# Geométrica: geom.

# Hay que anteponer letra para indicar qué buscamos:
# d para función de masa o densidad
# p para la frec acumulada
# r para obtener número al azar
# q para cuantil

# BINOMIAL
# dbinom: devuelve probabilidad de obtener un valor
# para binomial indicamos n = size y p = prob
dbinom(0,size=5,prob=0.9)

# POISSON
# dpois: devuelve probabilidad de obtener un valor
# para poisson indicamos parámetro lambda
dpois(10,lambda=3.52)

# EJERCICIO 4
# tomo siempre n = 5
# varío p (0.5 - 0.1 - 0.9)
# busco probabilidad de cada valor posible de X

dbinom(0,size=5,prob=0.9)
dbinom(1, size = 5, prob = 0.9)
dbinom(2, size=5, prob=0.9)

# hago el gráfico
x <- 0:5
plot(x,dbinom(x,size=5,prob=0.5),type="h")
plot(x,dbinom(x,size=5,prob=0.1),type="h")
plot(x,dbinom(x,size=5,prob=0.9),type="h")

################## continuas ####################
# Uniforme: unif con parámetros min y max
# Exponencial: exp con parámetro rate = 1/beta
# Normal: norm con parámetros mean y sd
# chi cuadrado: chisq
# t de Student: t
# F de Snedecor: f

# normal

# para calcular la probabilidad acumulada hasta el 
# valor 3 en una distribución normal de media -2 y
# desvío estándar 4 uso la función pnorm() con argumentos
pnorm(3,mean=-2,sd=4)


# si quiero obtener la probabilidad de obtener un 
# valor entre 5 y 3 puedo restar las probabilidades
# acumuladas hasta 5 y hasta 3:
pnorm(5,-2,4) - pnorm(3,-2,4)

# para la exponencial el parámetro es
curve(dexp(x,rate=1/3),from=-3,to=10)




#############
curve(dnorm(x,mean=0, sd=1),from=-5,to=5)





curve(dnorm(x,mean=74, sd=3),from=64,to=84)
curve(dnorm(x,mean=5, sd=0.8),from=3,to=7)
