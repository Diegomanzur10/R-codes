#install.packages("xlsx")
# setwd("F:/JLSC/EIA/Educación continua/Diplomado Analítica/Modelos matemáticos y estadísticos para la Toma de Decisiones")
library(xlsx)
datos1<-read.xlsx("0.DatosScore(MSV).xlsx",sheetIndex=1)
datos2<-read.xlsx("0.DatosScore(MSV).xlsx",sheetIndex=2)
edad1<-datos1$Edad
table(edad1)
hist(edad1)
summary(edad1)
stem(edad1)
install.packages("modeest"); library(modeest)

mlv(edad1,method="discrete") 

cuartiles<-quantile(edad1, seq(0,1,0.25))  #cuartiles
deciles<-quantile(edad1, seq(0,1,0.1))   #deciles
rangoIC<-cuartiles[4]-cuartiles[2]

edad2<-datos2$Edad
cuartiles2<-quantile(edad2, seq(0,1,0.25))

edad_norm<-rnorm(length(edad1),mean=mean(edad1),sd=sd(edad1))
Qn1<-quantile(edad_norm,seq(0,1,0.25))
plot(Qn1,cuartiles)
regresion<-lm(cuartiles~Qn1)
abline(regresion,col="red")


plot(cuartiles2,cuartiles)
regresion<-lm(cuartiles~cuartiles2)
abline(regresion,col="red")



boxplot(cuartiles,cuartiles2,main="Comparación entre bancos",ylab="Edades",names=c("banco 1", "banco 2"))
ingresos1<-datos1$Ingresos
ingresos2<-datos2$Ingresos
q1<-quantile(ingresos1, seq(0,1,0.25))
q2<-quantile(ingresos2, seq(0,1,0.25))
boxplot(ingresos1,ingresos2,main="Comparación entre bancos",ylab="Ingresos",names=c("banco 1", "banco 2"))


boxplot.stats(edad1)
boxplot.stats(ingresos1)

ls<-mean(ingresos1)+1*sd(ingresos1)
mcls<-ingresos1>ls
ingresos1[mcls]
ingresos1[-mcls]
