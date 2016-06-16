### TEMA 1 ###

  ##DEMOSTRAR QUE UNA MATRIZ ES SIMÉTRICA E IDEMPOTENTE

#En primer lugar se cargan los datos con R
datos = read.csv("../datos.dat", header = FALSE)

#Después construimos los datos en una matriz, y miramos cuál es su dimensión:
X = as.matrix(datos)
n = dim(X)[1]
p = dim(X)[2]

#Una vez hecho esto pasamos a construir la matriz H por filas:
onesn = matrix(1,n,1)
In = diag(n)
Hn = In-(1/n)*onesn%*%t(onesn)

#y por columnas:
Ip = diag(p)
onesp = matrix(1,p,1)
Hp = Ip-(1/p)*onesp%*%t(onesp)

#Por último comprobamos que es simétrica e idempotente tanto para las:
round(Hn%*%Hn)==round(Hn)
Hn==t(Hn)

#como para columnas:
round(Hp%*%Hp)==round(Hp)
Hp==t(Hp)

  ##CALCULAR EL VECTOR DE MEDIAS

#Los datos y las matrices H se cargaron tal y como se explica en el apartado anterior.
media = (1/n)*t(X)%*%onesn

  ##CALCULAR LA MATRIZ DE COVARIANZAS Y DE CORRELACIONES

#Esto es relativamente sencillo pues existe una funcion para cada una
S = cov(X)
cor(X)

#Y las matrices H:
X =as.matrix(datos)
n =dim(X)[1]
p =dim(X)[2]
onesn =matrix(1,n,1)
In =diag(n)
Hn =In-(1/n)*onesn%*%t(onesn)
Ip =diag(p)
onesp =matrix(1,p,1)
Hp =Ip-(1/p)*onesp%*%t(onesp)

  ##COMPROBAR QUE DEFINIENDO ADECUADAMENTE H PARA CADA CASO, HX CENTRA LA MATRIZ POR FILAS MIENTRAS QUE XH LA CENTRA POR COLUMNAS
colMeans(Hn%*%X)
rowMeans(X%*%Hp)

### TEMA 2 ###

#Lo primero que hacemos es definir la matriz como X:
X<-read.csv("Ejercicio2.csv",header=T,sep=",")
X<-as.matrix(x)

#para proseguir hallamos la n dimensión de la matriz
n = dim(as.matrix(X))[1]

#con lo que podremos hallar H
H = diag(n)-n^-1*rep(1,n)%*%t(rep(1,n))

#a continuación obtenemos A
A = (-2^-1)*X^2

#con esto ya tenemos los valores de la constante aditiva necesarios para poder tener la matriz B
B = H%*%A%*%H

#mediante las siguientes órdenes obtenemos los valores de la constante aditiva y el mínimo de dichos valores
eigen(B)
m<-min(eigen(B)$values)

#Seguimos volviendo a calcular X pero restándole la mínima constante aditiva a todos los valores menos la diagonal
X<-(X-2*m)+2*m*diag(n)

#volvemos a repetir lo de antes hallando la A y B
A<-(-2^-1)*x^2
B<-H%*%A%*%H

#y para terminar con este apartado obtenemos los vectores propios normalizados de B mediante:
eigen(B)


  ##CREAR UN FICHERO PARA QUE SE EJECUTE UN MDS

#Obtenemos X, n, H, A y B como en el ejercicio anterior
X<-read.csv("Ejercicio2.csv",header=T,sep=",")
X<-as.matrix(x)
n=dim(as.matrix(x))[1]
H<-diag(n)-n^-1*rep(1,n) %*% t(rep(1,n))
A<-(-2^-1)*x^2
B<-H % * % A % * % H
eigen(B)

#entonces la función siguiente calcula la representatividad de la matriz con dos variables
X=cmdscale(X, k = 2, eig = TRUE,add = FALSE, x.ret = FALSE)


  ##CREAR UN FICHERO PARA QUE SE EJECUTE UN MDS TRAS UNA TRANSFORMACIÓN DE LA CONSTANTE ADITIVA

eurodist
x<-as.matrix(eurodist)

n=dim(as.matrix(x))[1]
H<-diag(n)-n^-1*rep(1,n) %*% t(rep(1,n))
A<-(-2^-1)*x^2
B<-H %*% A %*% H
eigen(B)

m<-cmdscale(x,k=n-1,add=T,eig=T)$AC
m

x<-x-*m+*m*diag(n)
A<-(-2^-1)*x^2
B<-H %*%A %*%H
eigen(B)

m<-cmdscale(x,k=16,add=T,eig=T)$GOF
m


### TEMA 3 ###

  ##Realizar un ACP
datos<-read.csv("Alimentos.csv",header=TRUE)

attach(datos)
cor(datos)

round(cor(datos),2)

plot(datos)
detach(datos)

datos_pca=prcomp(datos,scale=T)

summary(datos_pca)

predict(datos_pca)[,1]

plot(datos_pca,ylim=c(0,5))


### TEMA 4 ###

HairEyeColor

hombres=HairEyeColor[,,1]
mujeres=HairEyeColor[,,2]
conjunto=hombres+mujeres

library(ca)
ca(hombres)
plot(ca(hombres))
ca(mujeres)
plot(ca(mujeres))
ca(conjunto)
plot(ca(conjunto))


### TEMA 5 ###

  ##Realizar un AC de los datos anteriores
library(scatterplot3d)
library(cluster)

data("planets", package = "HSAUR2")

rango=apply(planets,2,max)-apply(planets,2,min)
planetas=scale(planets,center=FALSE,scale=rango)

scatterplot3d(log(planetas[,"mass"]),
              log(planetas[,"period"]), log(planetas[,"eccen"]),
              type = "h", highlight.3d = TRUE, angle = 55,
              scale.y = 0.7, pch = 16)

planet_kmeans3=kmeans(planetas, centers = 3,
                      iter.max = 50,
                      nstart = 10, algorithm = "Hartigan-Wong")

table(planet_kmeans3$cluster)

planet_kmeans3$centers

scatterplot3d(log(planetas[,"mass"]),
              log(planetas[,"period"]), log(planetas[,"eccen"]),
              color=planet_kmeans3$cluster,
              type = "h", highlight.3d = FALSE, angle = 55,
              scale.y = 0.7, pch = 16)








