###########################################
#   ESTADISTICA DESCRIPTIVA UNIVARIADA    #
#                  EN R                   #
#          Lucía Rodríguez Planes         #
###########################################


# cargar datos ------------------------------------------------------------

# los datos se cargan como un vector de valores
# para hacer eso cargamos los valores separados por comas
# dentro del paréntesis de la función "c"
# se ve así 
c(1,2,45,6,9,1.3,4)

# para que quede guardado en la memoria de R
# le asignamos un nombre con la flechita
# se ve así 
datos <- c(1,2,45,6,9,1.3,4)

# valores mín y máx con funcion range()
# rango con la función diff() aplicada al rango
diff(range(datos))

# para definir la cantidad de intervalos podemos usar
# la regla de Sturges:
nclass.Sturges(datos)

# usando el rango y el número de clases podemos calcular 
# la amplitud de los intervalos de clase de igul tamaño
diff(range(datos))/nclass.Sturges(datos)
# si le asigno un nombre después lo puedo usar para calcular
# los límites de los intervalos de clase
a <- diff(range(datos))/nclass.Sturges(datos)


# agrupar datos

# para los intervalos de clase necesito calcular los valores
# límite de cada uno. Empiezo por el izquierdo

L1 <- min(datos)
L2 <- L1 + a
L3 <- L2 + a

lim <- numeric(nclass.Sturges(datos)+1)
lim[1] <- min(datos)
for (i in 1:nclass.Sturges(datos)){
  lim[i+1]<-lim[i]+a
}

# para las marcas de clase
mi <- numeric(nclass.Sturges(datos))
for (i in 1:nclass.Sturges(datos)){
  mi[i] <- (lim[i]+lim[i+1])/2
}

# para los frecuencias absolutas
# usamos el conteo que hace la función hist()
fi <- hist(datos,right=F,plot=F)$count

# para las frecuencias relativas
fri <- fi/length(datos)

# para ver los intervalos

# uso función cut() y otras para cortar y agrupar
clases <-  cut(datos, breaks = nclass.Sturges(datos), right=FALSE)
intervalos <- levels(clases)
# mi <- numeric(nclass.Sturges(datos))
# for (i in 1:nclass.Sturges(datos)){
#   mi[i] <- (lim[i]+lim[i+1])/2
# }

fi <- table(clases)
fri <- fi/length(datos)

Fr.cum.abs <-  cumsum(fi) 

# fabrico tabla de frecuencias con todo este material 
tabla <-  data.frame(intervals, mc, Fr.abs, Fr.cum.abs, Fr.rel, Fr.cum.rel)