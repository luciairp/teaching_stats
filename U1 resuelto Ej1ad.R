# EJERCICIO RESUELTO ADICIONAL 1 - DERRUMBES

# variable: cuantitativa continua

# cargamos los datos de alcance de derrumbes de la tabla
# le asignamos el nombre 'derrumbe'
derrumbe <- c(1.4,9.8,3.2,7.1,7.9,8.6,
              6.1,10.3,4.0,8.6,6.7,6.6,
              6.2,6.8,7.2,11.5,3.4,5.8,
              2.7,5.6,8.3,9.3,5.8,6.8)

# cargamos las funciones del paquete aplpack
library(aplpack)

# usamos función para el diagrama de tallo y hojas de Tukey
# la barra vertical representa el punto decimal:
# a la izquierda las unidades, a la derecha los decimales
stem.leaf(derrumbe, trim.outliers = F, depths = F)

library(ggplot2)
library(tibble)
ggplot(as_tibble(derrumbe),aes(x=value))+
  geom_histogram(fill='darkgreen', closed = "left",binwidth = 2)+
  labs(y= "frecuencia", x = "distancia (km)")+
  theme_light()
