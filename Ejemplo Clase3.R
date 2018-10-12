# install.packages("xlsx") 
library(xlsx)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(x, histogram=TRUE, pch=19)



datos_cor <- read.xlsx("Corr_Cov.xlsx", sheetIndex = 1)
datos_cor<-datos_cor[1:44,1:8]#para evitar errores de lecturas
p<-cor(datos_cor)
p1<-cov(datos_cor)
pairs(datos_cor[,1:7])





# datos1 <- read.xlsx("0.DatosScore(MSV).xlsx", sheetIndex = 1) #la hoja de datos completa para la entidad financiera 1, la 1ra hoja
# ef1ec<-datos1$Estado.civil
# ef1tc<- datos1$Tipo.de.contrato
# table(ef1ec,ef1tc)






edad<-datos1$Edad
ingresos<-datos1$Ingresos
egresos<-datos1$Egresos
score<-datos1$Score
regm<-lm(score ~ edad+ingresos+egresos)
summary(regm)
# Para colocar todas las variables en el mismo orden de magnitud, se procede a la normalización de las variables:
scn<-(score-min(score))/(max(score)-min(score)) #normaliza centrada
scn<-score/max(score)  #normalización basada en el máximo
scn<-score/10^2  


x1<-data.frame(edad,ingresos)
pairs(x1)
lineal<-lm(ingresos ~ edad, data=x1) #ingresos variable dependiente
print(lineal)
summary(lineal)
plot(edad,ingresos,main = "Relación de ingresos y edades", pch = 20, col = "green")
abline(lineal,lwd = 3, col = "red") #lwd para hacer más gruesa la línea
residuales<-residuals(lineal)




plot(lineal)
hist(residuales)


