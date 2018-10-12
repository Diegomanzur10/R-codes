# install.packages("xlsx")
# 
library(xlsx)
# 
 datos1 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 1)
 datos2 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 2)
 edad1=datos1$Edad
 edad2=datos2$Edad

# edad=datos$Edad




cuartiles1<-quantile(datos1$Edad, seq(0,1,0.25)) 
cuartiles2<-quantile(datos2$Edad, seq(0,1,0.25)) 

deciles<-quantile(datos1$Edad, seq(0,1,0.1))   

rangoIC<-cuartiles[4]-cuartiles[2]


edad_norm<-rnorm(length(edad1),mean=mean(edad1),sd=sd(edad1))


Qn1<-quantile(edad_norm,seq(0,1,0.25))

# plot(Qn1,cuartiles)
# 
# regresion<-lm(cuartiles~Qn1)
# 
# abline(regresion,col="red")


QQ1=datos1$Ingresos
QQ2=datos2$Ingresos

# boxplot(QQ1,QQ2,main="Comparación entre bancos",ylab="Ingresos",names=c("banco 1", "banco 2"))
# 

library(PerformanceAnalytics)

curt_e=kurtosis(QQ1, na.rm = FALSE, method ="excess")
curt_m=kurtosis(QQ1, na.rm = FALSE, method ="moment")
curt_f=kurtosis(QQ1, na.rm = FALSE, method ="fisher")
curt_s=kurtosis(QQ1, na.rm = FALSE, method ="sample")





coe_asi_m=skewness(QQ1, na.rm = FALSE, method ="moment")
coe_asi_f=skewness(QQ1, na.rm = FALSE, method ="fishe")
coe_asi_s=skewness(QQ1, na.rm = FALSE, method ="sample")

library(PerformanceAnalytics)
chart.Correlation(datos1, histogram=TRUE, pch=19)
