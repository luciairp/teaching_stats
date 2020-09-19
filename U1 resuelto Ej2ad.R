# EJERCICIO RESUELTO ADICIONAL 2 - GARRAPATAS

# variable: cuantitativa discreta

# cargamos los datos de cantidad de garrapatas
# en ratones, le damos nombre Ixodes
ixodes <- c(0,2,0,0,2,2,0,0,1,
            1,3,0,0,1,0,0,1,0,
            1,4,0,0,1,4,2,0,0,
            1,0,0,2,2,1,1,0,6,
            0,5,1,3,0,1,0,1)

# primero miro el tipo de variable y los datos
# exploro el máximo y le mínimo:
max(ixodes)
min(ixodes)

# veo que la cantidad de garrapatas por ratón toma valores
# entre 0 y 6...
# realizaré un gráfico de barras o bastones para cada valor posible,
# es decir, con 7 clases (0, 1, 2, ..., 6)

# hago el recuento de casos para cada valor
fi <- as.vector(table(ixodes))
# las relativas y las acumuladas
fri <- fi/length(ixodes)
Fi <-  cumsum(fi) 
Fri <- Fi/length(ixodes)

# uno la información en un tabla para verlo más fácil
(tabla <-  data.frame(0:6, fi, fri, Fi, Fri))

# con esta información puedo construir el gráfico 
# cargo paquete que necesitamos
library(tibble)
library(ggplot2)
ggplot(as_tibble(ixodes),aes(x=value))+
  geom_bar(width = .1, fill = 'darkgreen')+
  labs(y= "frecuencia absoluta", x = "garrapatas")+
  theme_light()
