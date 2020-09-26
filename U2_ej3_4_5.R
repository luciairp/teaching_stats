
datos <- read.table("alumni.txt", header = T)
datos

ej3_abs <- xtabs(numero ~ trabaja + examenes, data = datos)
ej3_abs

res <- chisq.test(ej3_abs)
res$observed
res$expected
res$statistic

sqrt(1.71/21.71)
sqrt(0.5)
0.28/0.71


datos <- read.table("pajaritos.txt", header = T)
datos

ej4_abs <- xtabs(pajaritos ~ supervivencia + lugar, data = datos)
ej4_abs

res <- chisq.test(ej4_abs)
res$observed
res$expected
res$statistic

sum(datos$pajaritos)

0.08/0.71

datos <- read.table("quimio.txt", header = T)
datos

ej5_abs <- xtabs(pacientes ~ grupo + reaccion, data = datos)
ej5_abs

res <- chisq.test(ej5_abs)
res$observed
res$expected
res$statistic

sum(datos$pajaritos)

0.08/0.71
