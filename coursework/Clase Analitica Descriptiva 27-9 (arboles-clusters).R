install.packages("rpart")
install.packages("rpart.plot")
install.packages("factoextra")
library("rpart.plot")
library(rpart)
library(dplyr)
library(cluster)
library(factoextra)

summary(iris)

#Creamos un arbol de decision con rpart
arbol1 <- rpart(Species~., data = iris, method = "class")

#graficamos el arbol que acabamos de crear usando rpart.plot
rpart.plot(arbol1, main = "Arbol de Clasificacion: Flores")

plot(iris$Petal.Length)

#clusterizando con el kmeans
data_iris <- scale(iris[,-5])
set.seed(301020)
fit_k <- kmeans(data_iris, centers = 3)
fviz_cluster(object = fit_k, data_iris)

fit_k$centers #tira los centros de los 3 clusters para cada variable
fit_k$withinss #la distancia entre los puntos dentro de un cluster.  Nuestro cluster ideal es el que tiene menor withinss.
fit_k$tot.withinss #la suma de los números de las distancias de arriba.

#graficando como cambian la poblacion en los clusters

plot(1:k.max, wss,
     type = "b", pch = 19, 
     xlab = "Number of clusters",
     ylab = "between ss/ total ss")

