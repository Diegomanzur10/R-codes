# curtosis=function(x){
# n <- length(x)
# ma=n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
# ma-3}
# cuartiles<-quantile(datos$Edad, seq(0,1,0.25))  #cuartiles
# deciles<-quantile(datos$Edad, seq(0,1,0.1))   #deciles
# rangoIC<-cuartiles[4]-cuartiles[2]

library(xlsx)
datos1 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 1)
datos2 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 2)
# QQ1<-datos1$Edad 
# QQ2<- datos2$Edad 
# QQ1v<-quantile(QQ1,seq(0,1,0.25))
# QQ2v<-quantile(QQ2,seq(0,1,0.25))
# plot(QQ1v,QQ2v)
# regresion<-lm(QQ1v~QQ2v)
# abline(regresion,col="red")
# 
# boxplot(QQ1,QQ2,main="Edades",ylab="valores de edades",names=c("banco 1", "banco 2"))
# summary(QQ1)
# summary(QQ2)

QQ1<-datos1$Ingresos 
QQ2<- datos2$Ingresos
boxplot(QQ1,QQ2,main="Edades",ylab="valores de edades",names=c("banco 1", "banco 2"))
summary(QQ1)
summary(QQ2)

# ls<-mean(QQ1)+sd(QQ1)
# mcls<-QQ1>ls


# boxplot(QQ1,QQ2,main="Comparación entre bancos",ylab="Ingresos",names=c("banco 1", "banco 2"))

library(PerformanceAnalytics)
datosnum<-c(datos1$Edad, datos1$Perscargo, datos1$Ingresos)
datos_cor <- read.xlsx("DatosNorm.xlsx", sheetIndex = 1)
chart.Correlation(datos_cor, histogram=TRUE, pch="+")
# chart.Correlation(datos1, histogram=TRUE, method = "pearson")

edad<-datos1$Edad
ingresos<-datos1$Ingresos
x2<-data.frame(edad,ingresos)
score<-datos1$Score
regm<-lm(score ~ edad+ingresos)
summary(regm)

# predicciones3<-predict(object=regm,newdata=data.frame(ingresos=c(edad=c(50))),interval='confidence',level=0.95)


x1<-data.frame(edad,ingresos)
pairs(x1)
lineal<-lm(ingresos ~ edad, data=x1) #ingresos variable dependiente
print(lineal)
summary(lineal)
plot(edad,ingresos,main = "Relación de ingresos y edades", pch = 20, col = "green")
abline(lineal,lwd = 3, col = "red")#lwd para hacer más gruesa la línea
# residuales<-residuals(lineal)
# par(mfrow=c(2,2))
# plot(lineal)
# hist(residuales)


predicciones<-predict(object=lineal,newdata=data.frame(edad=c(50)),interval='confidence',level=0.95) #debe tener el mismo nombre la variable independiente declarada en lm, coloca los intervalos de confianza en las dos columnas siguientes
predicciones2<-predict(object=lineal,newdata=data.frame(edad=c(seq(20,40))),interval='confidence',level=0.95) # con un vector de datos de entrada

