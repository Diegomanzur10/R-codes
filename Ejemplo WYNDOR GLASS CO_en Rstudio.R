x1<-seq(0,4,length=100)
x2<-seq(0,6,length=100)
costo<-function(x1,x2) {3*x1+5*x2} #se construye la función
z<-outer(x1,x2, costo)#aplicamos la función a todos los puntos de la malla,La función outer evalua la función f en cada punto(xi,yj)
persp(x1,x2,z)#con esta función se dibujan los puntos
persp(x1,x2,z,theta = 30, phi=30, col="orange")#con esto se gira en perspectiva y color
#también se puede agreagr título y otra información necesaria
ecuacion <- expression(z == 3*x[1]+5*x[2])# otra forma de obtener el vector Z que guarda la ganancia
persp(x1,x2,z,theta = 30, phi=30, col="orange",main="Evaluación de costo/producción", sub=ecuacion, col.main="blue")

#colocando la restricción de la tercera planta
#install.packages("rgl")#instalar rgl si no se tiene esa libreria
library( rgl )
persp3d(x1,x2,z,color="orange")
planes3d(3,2,0,-18,alpha=0.5,xlim = NULL, ylim = NULL, zlim = NULL) #ej:planes3d(a, b = NULL, c = NULL, d = 0, ...)draw planes using the parameter ax + by + cz + d = 0.
play3d( spin3d( axis = c(0, 0, 1), rpm = 7), duration = 10 )
#Para exportar y guardar el .gif
#install.packages("magick")
#library(magick)
movie3d(spin3d( axis = c(0, 0, 1), rpm = 3),duration = 15,  movie = "gif_wyndor Galss",dir = getwd(), type = "gif", clean =  TRUE) #getwd() es el directorio de trabajo, se puede cambiar en la ventana del lado, en la pestaña Files (buscar en los puntos suspensivos donde lo desea ubicar y se fija como directorio de trabajo)