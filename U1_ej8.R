# cargamos los datos de la tabla en un objeto de nombre 'pez
pez <- c(3.6,1.2,2.1,1.2,1.5,3.6,3.6,1.2,2.1,3.6,2.3,1.5,2,1.2)

# evaluamos la cantidad de intervalos k adecuada para 14 datos
# usamos la función de Sturges para evaluar el mínimo
nclass.Sturges(pez)
# para respetar el histograma son 6
k <- 6

# calculamos el rango R: x(n)-x(1)
diff(range(pez))

# calculamos la amplitud de los intervalos de clase según
# el rango R dividido por el número de clases k
amplitud <- diff(range(pez))/k

# partimos el conjunto de datos en 5 intervalos
# que es lo que se ve en el histograma
# usamos intervalos abiertos a derecha
pez_part <- cut(pez, breaks = 6, right =F)

# los intervalos generados los vemos como texto acá
(intervalos <- levels(pez_part))

# para calcular las marcas de clase necesito los límites 
# de cada intervalo

mi <- (min(pez)+amplitud*(0:(k-1)))+(amplitud/2)
  
fi <- as.vector(table(pez_part))
fri <- fi/length(pez)

Fi <-  cumsum(fi) 
Fri <- Fi/length(pez)

# fabrico tabla de frecuencias con todo este material 
tabla <-  data.frame(intervalos, mi, fi, fri, Fi, Fri)

# calculo coeficiente de variación
# CV = s/media * 100
(cv <- (sqrt(var(pez))/mean(pez))*100)

# vamos a aprovechar para hacer gráficos descriptivos:
pez_t <- tibble::as_tibble(pez)
library(tidyverse)

# histograma 
# ¡CUIDADO! por defecto R usa intervalos abiertos a izquierda

# histograma de la guía: por defecto, abiertos a izquierda
ggplot(pez_t,aes(x=value))+
  geom_histogram(bins = 6,fill='darkgreen')+
  labs(y= "frecuencia", x = "talla")+
  theme_light()

# histograma con intervalos cerrados a izquierda
ggplot(pez_t,aes(x=value))+
  geom_histogram(bins = 6,fill='darkgreen', closed = "left")+
  labs(y= "frecuencia", x = "talla")+
  theme_light()

# histograma que empieza en el mínimo valor
# (¡como la tabla de frecuencias que hicieron ustedes!)
ggplot(pez_t,aes(x=value))+
  geom_histogram(binwidth = 0.4 ,fill='darkgreen', closed = "left",boundary=min(pez))+
  labs(y= "frecuencia", x = "talla")+
  theme_light()


# boxplot
ggplot(pez_t, aes(x=value))+
  geom_boxplot(varwidth = F,col='darkgreen',fill='lightgreen')+
  #geom_dotplot(stackdir='center', dotsize=1)+
  labs(x = "talla")+
  coord_flip()+
  theme_light()
