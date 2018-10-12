#install.packages("xlsx")  #instalamos la libreria
library(xlsx) #llamamos la libreria


datos = read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 1) #EF #1

edad= datos$Edad

summary(datos)

stem(edad)

# hist(edad,breaks=5)
hist(edad)

# datos1 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 1)
# datos2 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 2)
# QQ1<-datos1$Edad 
# QQ2<- datos2$Edad 
# QQ1v<-quantile(QQ1,seq(0,1,0.25))
# QQ2v<-quantile(QQ2,seq(0,1,0.25))
# edad_norm<-rnorm(length(edad1),mean=mean(edad1),sd=sd(edad1))
# Qn1<-quantile(edad_norm,seq(0,1,0.25))
# plot(Qn1,cuartiles)
# regresion<-lm(cuartiles~Qn1)
# abline(regresion,col="red")


