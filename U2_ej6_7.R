datos <- read.table("oro.txt", header = T)
datos
library(tidyverse)

plot(datos$grava,datos$oro)

mean(datos$grava)
mean(datos$oro)

tabla <- datos %>%
  mutate(gr_dif_media = grava - mean(grava)) %>%
  mutate(gr_dif_media_2 = gr_dif_media^2) %>%
  mutate(oro_dif_media = oro - mean(oro)) %>% 
  mutate(oro_dif_media_2 = oro_dif_media^2) %>% 
  mutate(dif_grava_oro = gr_dif_media * oro_dif_media)

# para construir el numerador necesito la sumatoria de la columna
# dif_grava_oro y dividirla por n = 12

sum(tabla$dif_grava_oro)/12
numerador <- sum(tabla$dif_grava_oro)/12

# para el denominador necesito usar las columnas de las diferencias^2
# hago la sumatoria de cada una y a eso lo divido por n, multiplico
# y saco raíz cuadrada

sqrt((sum(tabla$gr_dif_media_2)/12)*(sum(tabla$oro_dif_media_2)/12))
denominador <- sqrt((sum(tabla$gr_dif_media_2)/12)*(sum(tabla$oro_dif_media_2)/12))

# el coeficiente de correlación es el resultado de la razón entre
# esos valores:

(coef_corr <- numerador/denominador)

# o uso función de correlación de Pearson en R
cor(datos$grava, datos$oro)
