library(xlsx)
pronostico<- read.xlsx("Pronóstico.xlsx",sheetIndex = 1)
xk<-pronostico$Open_k
xks<-ts(xk) #como serie de tiempo
acf(xks)#diagrama de autocorrelación
pacf(xks)#diagramas de autocorrelación parcial

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
pronxks<-ts(pronostico)
chart.Correlation(pronxks, histogram=TRUE, pch=19)

xk1<-pronostico$Closek_1
xk2<-pronostico$Closek_2
xk3<-pronostico$Closek_3
xk4<-pronostico$Closek_4
x1<-data.frame(xk4,xk3,xk2,xk1,xk)
regres<-lm(xk~xk4+xk2+xk3+xk1,data=x1)#x1 guarda todos los datos
summary(regres)
predict(object=regres,interval='confidence',level=0.95,newdata=data.frame(xk4=c(1.2),xk3=c(1.2),xk2=c(1.2),xk1=c(1.2))) 
par(mfrow=c(1,2))#filas y columnas
plot(regres)

  
#Arima
ajuste<-arima(xks,order=c(4,3,3))#colocando 0,4,0 queda el modelo autorregresivo con 4 retardos.
predict(ajuste,n.ahead=12)#predice 12 días adelante
summary(ajuste)
ajuste

#Regresiones no lineales
library(xlsx)
rgnl<- read.xlsx("RegresiónNoLineal.xlsx",sheetIndex = 1)
x2<-rgnl$x
x2<-x2[1:24]
y2<-rgnl$y
y2<-y2[1:24]
plot(x2,y2)
#Sin ecuación
f<-approxfun(x2,y2)
curve(f(x), -10, 10, col = "green2")
points(x2,y2)
f(-5.1)

#con ecuaciones
fit  <- lm(y2~x2)
fit2 <- lm(y2~poly(x2,2,raw=TRUE))
fit3 <- lm(y2~poly(x2,3,raw=TRUE))
fit4 <- lm(y2~poly(x2,4,raw=TRUE))
plot(x2,y2,pch=19,ylim=c(-1,1))
xx<-seq(-10,10,length=50)
#polinomiales
lines(xx, predict(fit, data.frame(x2=xx)), col="red")
lines(xx, predict(fit2, data.frame(x2=xx)), col="green")
lines(xx, predict(fit3, data.frame(x2=xx)), col="orange")
lines(xx, predict(fit4, data.frame(x2=xx)), col="blue")
summary(fit4)

#Regresión logística o sigmoidal
fitrlxy<-glm(y2~x2)#glm es para modelos lineales generalizados
plot(x2,y2,pch=19,ylim=c(-1,1))
xx<-seq(-10,10,length=50)
lines(xx, predict(fitrlxy, data.frame(x2=xx)), col="red")
curvaLogistica = function(x){
  b0 = as.numeric(fitrlxy$coefficients[1])
  b1 = as.numeric(fitrlxy$coefficients[2])
  return(exp(b0 + b1 * x)/(1 + exp(b0 + b1 * x)))
}
curve(curvaLogistica,from = -100, to = 100)#se pierde información al linealizar el problema.
summary(fitrlxy)
plot(fitrlxy$residuals)
plot(fitrlxy)
Hit <Return>

#para no lineales
fitrl <- nls(y2 ~ a*(1+exp(-x2/b))^c,  start = list( a=.8,b=.8,c=-1.8),algorithm="port")#nls es para modelos no lineales
plot(x2,y2,pch=19,ylim=c(-1,1))
xx<-seq(-10,10,length=50)
lines(xx, predict(fitrl, data.frame(x2=xx)), col="orange")
summary(fitrl)
