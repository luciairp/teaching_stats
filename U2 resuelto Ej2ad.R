# EJERCICIO RESUELTO ADICIONAL 2 - MORTALIDAD MERLUZAS

# cargo los datos
edad <- c(4,5,6,7,8,9)
N <- c(2353,1751,786,339,159,70)
datos <- data.frame(edad,N)

# hago el diagrama de dispersión
plot(datos$edad,datos$N)

# para calcular paso a paso el coeficiente de correlación
# miro primero los valores de la media aritmética de cada variable
mean(datos$edad)
mean(datos$N)

# ahora sí fabrico la tabla con los cálculos correspondientes
library(tidyverse)
tabla <- datos %>%
  mutate(edad_dif_media = edad - mean(edad)) %>%
  mutate(edad_dif_media_2 = edad_dif_media^2) %>%
  mutate(N_dif_media = N - mean(N)) %>% 
  mutate(N_dif_media_2 = N_dif_media^2) %>% 
  mutate(dif_edad_N = edad_dif_media * N_dif_media)

# hago las cuentas por separado para numerador
numerador <- sum(tabla$dif_edad_N)/6

# y denominador
denominador <- sqrt((sum(tabla$edad_dif_media_2)/6)*(sum(tabla$N_dif_media_2)/6))

# y el coeficiente de correlación es:
(coef_corr <- numerador/denominador)

# si prefiero puedo usar la función integrada en R que hace esto mismo
cor(datos$edad, datos$N)
