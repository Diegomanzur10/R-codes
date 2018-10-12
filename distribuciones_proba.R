#ejercicio de planta eléctrica de cuidad
pgamma(10, 3, scale = 2, lower.tail = F)#a) se esta resolviendo como P(X>10), luego se debe falsear lower.tail, pues if TRUE (default), probabilities are P[X ??? x], otherwise, P[X > x]. Como es un día cualquiera, es un dato puntual, por eso la función de densidad
pgamma(8, 3, scale = 2, lower.tail = T) - pgamma(3, 3, scale = 2, lower.tail = T)#b) como es la probabilidad e un rango, se hace con la función de distribución acumulada
qgamma(0.9, 3, scale = 2, lower.tail = T)#c)lo que saca es un cuartil, por ende es el área de la curva desde -inf hasta ese dato de probabilidad
#otra forma de decir este último, es que la probabilidad de q el consumo sea inferior a ese dato es de danto

#Para el ejemplo de las notas
qchisq(1-0.05,df=1)#R trabaja con el área d ela izquierda, la de aceptación, la tabla tiene la de rechazo.
1-pchisq(0.1068,df=1)#el valor p para 

#Chi2 para independencia
tabla_notas <- read.xlsx("Notas.xlsx",sheetIndex = 1)#la encuesta
tabla_contingencia<-table(tabla_notas$GÃ.nero,tabla_notas$Nota)
chisq.test(tabla_contingencia)#como el p-value es mayor que el nivel de significancia (aproximadamente uno), de que los hombres, las mujeres y las notas son variables independientes, el warning lo que quiere decir es que no se rechaza la hipótesis 

setwd("F:/JLSC/EIA/Educación continua/Diplomado Analítica/Modelos matemáticos y estadísticos para la Toma de Decisiones")
library(xlsx)
datos1 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 1)
edad1<-datos1$Edad


#probedad1<-rep(1/length(edad1),length(edad1))#pone en todos los espacios la misma probailidad e ocurrencia
#con probedad1 es probar con ella misma

#Debe hacerse la prueba por rangos o datos puntuales, se sugiere hacerlo por cuartiles
q<-quantile(edad1, seq(0,1,0.25))
media_edad<-mean(edad1)
ds_edad<-sd(edad1)
#pq<-dnorm(q,mean=media_edad,sd=ds_edad)#saca las probalidades de una distribución normal equivalente, la densidad pues es el punto a punto
#pq1<-pnorm(q,mean=media_edad,sd=ds_edad)#probabilidad acumulada
h1<-hist(edad1,breaks = 19)#hace un histograma con un número de barras determinado (buckets)
h1$counts#muestra cuántos hay en cada barra, no debería quedar más del 20% con menos de 5 frecuencias
h1$breaks#contiene los valores de las edades de cada barra en orden ascendente
#probando con una normal
pa1<-pnorm(h1$breaks,mean=media_edad,sd=ds_edad)#contiene la probabilidad acumulada de cada punto con una normal equivalente

#install.packages("zoo")
#library(zoo)
p<-rollapply(pa1,2,function(x)x[2]-x[1])# esta función corre continuamente la función que allí se declara, en este caso se desea restar la probabilidad acumulada del dato i+1, menos la del dato i.
chisq.test(h1$counts, p=p, rescale.p=TRUE, simulate.p.value=TRUE)# p es la probailidades a comparar, recale es por que no esta normalizada entonces la normaliza si es necesario, simular el pvalue es para usar el método montecarlo de muestro aleatorio, los 2000 replicas son por defecto (se peude cambiar con B=###)

#install.packages("rriskDistributions")
library(rriskDistributions)
distp<-fit.cont(edad1)

