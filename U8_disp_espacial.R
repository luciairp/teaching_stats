
# Bondad de ajuste --------------------------------------------------------

library(fitdistrplus)

# Camarones
# Evaluar la disposición espacial de una población de camarones en el fondo del mar. 
# Los datos disponibles en el archivo ¨datos_camarones_agrup.csv¨ muestran el número de 
# individuos registrados por unidad muestral. Determinen si estas frecuencias se ajustan 
# a una distribución Poisson o una binomial negativa.

data_camarones <-read.csv("datos_camarones_agrup.csv",header=T,sep=",",dec=".")

head(data_camarones)

# ver las frecuencias de los datos
par(mfrow=c(1,3))
hist(data_camarones$camarones, right = FALSE, breaks = seq(0, 12, 1),
     main = "Frecuencia de camarones por \nunidad de muestreo")
mean(data_camarones$camarones)

camarones <- table(data_camarones$camarones)

# ¿Cómo se comporta algo con esa media que sigue una distribución Poisson?
# Simulamos una muestra de ea distribución, calculamos las probabilidades
# (teléfono para unidades 4 y 5)
poisson <- rpois(500,1.195)
hist(poisson, right = FALSE, breaks = seq(0, 12, 1),
     main = "Poisson",col=2)

# Vamos a calcular la probabilidad puntual de obtener cada valor en esta distibución
# Para eso usamos la función que ya conocemos: dpois
# Para no hacerlo valor a valor hacemos un vector con todos los valores que queremos
prob_poisson <- dpois(c(0,1,2,3),lambda=1.195)
prob_poisson[5] <- 1-sum(prob_poisson)

# Ajuste a Poisson
# con chisq.test
chisq.test(camarones, p = prob_poisson)

# ¿Y binomial negativa?
ajuste_nb <- fitdist(data_camarones$camarones, "nbinom")
summary(ajuste_nb)

bn <- rnbinom(500, mu = 1.19, size = 1.80)
hist(bn, right = FALSE, breaks = seq(0, 12, 1),
     main = "Binomial negativa",col=3)

prob_bn <- dnbinom(c(0,1,2,3),size = 1.80,mu = 1.19)
prob_bn[5] <- 1-sum(prob_bn)

chisq.test(camarones, p = prob_bn)


# gráfico bastones comparado ----------------------------------------------

# función fitdist()
ajuste_p <- fitdist(data_camarones$camarones, "pois")
ajuste_nb <- fitdist(data_camarones$camarones, "nbinom")
par(mfrow=c(1,1))
denscomp(list(ajuste_p,ajuste_nb), 
         legendtext = c("Poisson", "Binomial negativa"),
         fittype = "o", lwd=2, pch=16)
