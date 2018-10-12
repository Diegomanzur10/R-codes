edad_norm1<-rnorm(length(edad1),mean=mean(edad1),sd=sd(edad1))
Qn1<-quantile(edad_norm1,seq(0,1,0.25))
plot(Qn1,cuartiles)
regresion<-lm(cuartiles~Qn1)
abline(regresion,col="red")
hist(edad_norm1)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
kurtosis(edad1, method = "fisher")
kurtosis(edad1, method = "sample_excess")
kurtosis(edad1, method = "sample")
skewness(edad1, na.rm = FALSE, method = "fisher")
subset()

datos_cor <- read.xlsx("Corr_Cov.xlsx", sheetIndex = 1)
chart.Correlation(datos_cor, histogram=TRUE, pch=19)

length(datos_cor)

p<-cor(datos_cor)
p1<-cov(datos_cor)

ef1ec<-datos1$Estado.civil
ef1tc<- datos1$Tipo.de.contrato
table(ef1ec,ef1tc)

edad<-datos1$Edad
ingresos<-datos1$Ingresos
score<-datos1$Score
regm<-lm(score ~ edad+ingresos)
summary(regm)

predicciones<-predict(object=regm,newdata=data.frame(edad=c(50,50,30),ingresos=c(1000000,2000000,5000000)),interval='confidence',level=0.95) 
predicciones
