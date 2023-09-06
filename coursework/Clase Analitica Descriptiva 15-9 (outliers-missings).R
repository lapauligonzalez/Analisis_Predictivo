#Practica de Outliers y Missings

#---- OUTLIERS ----
dim(diamonds)
colnames(diamonds)

#analizamos los outliers de una variable en particular "carat"
summary(diamonds$carat) #summary hace un resumen de un vector numerico de datos
#como la mediana y el promedio son distintos, la distribución no es simetrica,
#cuanta mayor diferencia haya entre la mediana y promedio menos simetrico es


#graficamos el histograma para ver hacia donde están distribuidos los datos
hist(diamonds$carat)
#vemos cuantos outliers hay
boxplot(diamonds$carat)

#analizamos los outliers de una variable en particular "x"
summary(diamonds$x)
hist(diamonds$x)
boxplot(diamonds$x)

#con la funcion quantile nos devuelve los percentiles que queremos
percent <- quantile(diamonds$carat, c(0.01,0.05,0.10,0.50,0.95,0.99))
percent
#en los resultados vemos que el 99% de los casos tienen hasta 2.18, se puede ver en el boxplot
#todo lo que este por encima de este valor, van a ser outliers.  Todos aquellos que esten fuera de los bigotes.

#los outliers afectan a todos los grupos o es un solo grupo, por eso analizamos si tienen que ver con las variables categoricas

diamonds %>% 
  group_by(cut) %>% 
  summarize(media=mean(carat),mediaT=mean(carat,trim =0.02 ,na.rm = T),maxi=max(carat),minim=min(carat), mediana=median(carat))%>% ungroup()
#como cut es una variable categórica, podemos ver en cual de los cortes hay outliers (el corte Fair tiene un outlier mayor)
#podemos graficar un boxplot de esta informacion de la siguiente manera: 
boxplot(diamonds$carat ~ diamonds$cut, main="Diamond carat by cut", ylab = "Carat", xlab = "Cut")

diamonds %>% 
  group_by(color) %>% 
  summarize(media=mean(carat),mediaT=mean(carat,trim =0.02 ,na.rm = T),maxi=max(carat),minim=min(carat), mediana=median(carat))%>% ungroup()
#como color es una variabla categórica, podemos analizar si los maximos tienen que ver con el color
#el color J tiene el mayor outlier

diamonds %>% 
  group_by(clarity) %>% 
  summarize(media=mean(carat),mediaT=mean(carat,trim =0.02 ,na.rm = T),maxi=max(carat),minim=min(carat), mediana=median(carat))%>% ungroup()
#como color es una variabla categórica, podemos analizar si los maximos tienen que ver con la claridad


#Aca agregamos una nueva columna
diamonds1 = diamonds1%>%mutate(caratsqr=carat*carat)

#Y agarramos solo los valores con corte Fair
diamonds2 = diamonds1 %>% filter(cut == "Fair")
View(diamonds2)

#promedio de todos los diamantes
mT=mean(diamonds1$carat,trim =0.02 ,na.rm = T)
mT
#promedio de los diamantes con corte Fair
media=mean(diamonds2$carat ,na.rm = T)
media

#comparando los resultados, podemos decir que el promedio de los diamantes Fair es mayor al 
#promedio del total.  Pero para eso lo garantizamos con un test de hipotesis de medias
#verificamos si existe diferencias entre promedios
#abajo decimos que el promedio de la distribucion de la base de datos de los Fair, que las medias son iguales
t.test(diamonds2$carat,
       alternative = "two.sided",
       mu = mT, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

#2da forma de hacerlo:
#abajo decimos que el promedio de la distribucion de la base de datos totales, que las medias son iguales
t.test(diamonds1$carat,
       alternative = "two.sided",
       mu = mT, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

#3era forma de hacerlo:
#podemos las dos distribuciones al principio como dos variables
#en este caso no hace falta calcular promedios de antemano
t.test(diamonds1$carat, diamonds2$carat)


# Otra forma de ver outliers es calculando Z-scores
#el valor z muestra a cuantos desvios estandar esta un valor del promedio
install.packages("outliers")
library(dplyr)
library("outliers")
boxplot(iris$Sepal.Width)
summary(iris$Sepal.Width)
z.scores <- iris$Sepal.Width %>% scores(type = "z")
z.scores %>% summary()
#la media de los z es 0 siempre, y podemos ver que el valor máximo de 4.4, esta a 3.08 desvios de la media.
#Utilizando esta forma, todos aquellos z cuyo valor absoluto estén por encima de 3.5, son outliers.

outlier_s <- c(which(abs(z.scores) > 3.5)) #los outliers
length(outlier_s) #cantidad de outliers


#Mahalanobis: Esta es la metrica de distancia mas utilizada para detectar valores atipicos para la configuracion multivariada.
# Es una extension del z-score univariado, que tambien da cuenta de la estructura de correlacion entre todas las variables.
# Sigue una distribucion de chi-cuadrado con n (numero de variables) 
# grados de libertad, por lo tanto, cualquier distancia de Mahalanobis mayor que el valor crotico de chi-cuadrado se trata como un valor atipico.

install.packages("MVN")
library('MVN')

#lo probamos con iris
iris2=iris[101:150,] %>% select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)
virginica <- iris[101:150, 1:3]
result <- mvn(data = iris2, mvnTest = "hz", 
              multivariateOutlierMethod = "quan", showOutliers = TRUE)

result$multivariateOutliers$Observation


# Una vez que encontramos valores atipicos, hay que decidir que hacemos con ellos, si excluirlos, eliminarlos o imputarlos.

# Eliminandolos: 
z.scores <- diamonds$depth %>%  scores(type = "z")

Carat_clean<- diamonds[-which( abs(z.scores) >3.5 ),] #aca elimina los valores atipicos

# Imputandolos:
#los valores atipicos se reemplazan con la media de ese grupo de datos, para que no afecten tanto
diamonds$carat[ abs(z.scores) >3.5 ] <- mean(diamonds$carat, na.rm = TRUE)

# Hacer Capping (aka Winsorsing)
#aquellos valores que valen mas que el 99% o 95% de los casos, los reemplazamos con el valor mayor del 95% o 99%.
#Se hace cuando se sabe que ciertos valores no pueden ser mayores a tal.

percent <- quantile(diamonds$carat, c(0.01,0.05,0.10,0.50,0.95,0.99))
percent
#en este caso hacer Capping seria: a los valores mayores a 2.17 los imputo con 2.17
#y tambien a los valores que sean menores a 0.24 los imputamos con ese valor.



#funcion para extraer los outliers fuera de los bigotes
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}
extraer_outliers(diamonds$table)

#funcion para imputar los outliers

cap <- function(x){
  quantiles <- quantile( x, c(.05, 0.25, 0.75, .95 ) )
  x[ x < quantiles[2] - 1.5*IQR(x) ] <- quantiles[1]
  x[ x > quantiles[3] + 1.5*IQR(x) ] <- quantiles[4]
  x
}

#---- MISSING ----
#en esta seccion se trabaja con los NA de una muestra

#para verificar cuales son NA dentro de una columna podemos hacer:
species_na <- is.na(iris$Species) 
mean(species_na) #calcula el porcentaje de NAs de la muestra

#faltante aleatorio: es random
#faltante sistematico: hay una razon, un patron dentras de porque faltan datos

#Para saber si el valor faltante es sistematico o aleatorio, creamos un modelo GLM
#La funcion glm() evalua la independencia de una variable con las otras de su mismo dataframe.
#Se puede usar para determinar si dependen o no las variables de mi variable objetivo.

#duplicamos la base datos en otra variable
iris_a <- iris
#la columna con la que queremos trabajar le aplicamos el is.na
iris_a$Species <- is.na(iris_a$Species)

#el modelo para saber si los faltantes son independientes:
m_imp = glm(as.factor(Species)~., #adentro del parentesis va la variable objetivo que queremos ver is depende que falte o no de otra
  data = iris_a , family = "binomial")
m_imp %>% summary #viendo el p-valor (la ultima columna) del resultado este
#si el p-valor es muy pequeño entonces que los datos falten va a depender de esa variable con el p-valor muy chico.  Los marca con el asterisco.

#Ahora, sabiendo esto, vamos a crear otro modelo con glm() pero esta vez sin las variables que dependen de la inicial, entonces creamos otro dataframe pero sin ellas
#hacemos el mismo summary de antes y vizualizamos ahora cuales son las variables que mas dependen de la inicial.

#- Imputando NAs linealmente usando el modelo glm()
#en la clase Lucas usa una tabla en donde una columna marca si una perosna esta casada o no.
# Lucas usando estos modelos, crea una tabla que evalua las probabilidades de que esté casada la persona o no (en los casos donde dice NA).
#igual no lo entendi como funciona entonces ni lo copie.


#- Imputando NAs usando algoritmo missForest()
#Lo que hace missForest es crear un modelo predictivo a partir de arboles de decision para determinar el valor posible de variables
install.packages("missForest")
library("missForest")


#EJERCICIO) Con la base de datos de properati para zona norte realice el analisis de missing y outliers:
#Realizar el analisis de missing y outliers:  
#- a) Detectar cuales son las variables que tiene outliers y missings. Recuerde utilizar histogramas, boxplots, transformaciones
#- b) Determinar si los outliers son datos reales o faltantes
#- c) Imputar faltantes de Superficie con regresion lineal
#- d) Imputar faltantes de tipo de propiedad con regresion logistica
#- e) imputar los demas con randomforest

library(missForest)
library(tidyverse)
setwd("C:/Users/paula/Desktop/Facultad/Analitica Descriptiva")
zprop = read_csv('znorte_properati.csv')

#analizando aspectos generales de price_aprox_usd
colnames(zprop)
View(zprop)
summary(zprop)

#vemos de forma general los outliers
summary(zprop$price_aprox_usd)
hist(zprop$price_aprox_usd)
boxplot(zprop$price_aprox_usd)

#extraer los outliers
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm=T)+IQR(x, na.rm=T)*1.5
  inf = quantile(x,0.25, na.rm=T)-IQR(x, na.rm=T)*1.5
  x[(x > sup) | (x<inf)]
}
outliers<- extraer_outliers(zprop$price_aprox_usd)

#como se comportan los outliers
outliers[!is.na(outliers)] %>% hist() #graficamos el histograma de los outliers 
boxplot(outliers) #para ver los outliers de los outliers

#hacemos un capping: que los valores que valen mas del 99%, imputarlos al valor del 99%.
percent <- quantile(zprop$price_aprox_usd, c(0.01, 0.99), na.rm= T)
percent
zprop %>% 
  filter(price_aprox_usd < percent[1]) %>%
  arrange(-surface_in_m2) #ordenamos de forma descendente por los m2




