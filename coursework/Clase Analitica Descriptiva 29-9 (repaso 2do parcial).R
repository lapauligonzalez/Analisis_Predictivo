install.packages("MVN")
library("MVN")

#Permite detectar outliers multivariados, muestra cuales son outliers y cuales no
iris2 <- iris[101:150,] %>% #agarro solo los virginia pero podriamos poner todos
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

result <- mvn(data = iris2,
              mvnTest = "hz",
              multivariateOutlierMethod = "quan",
              showOutliers = TRUE)

#---- PRACTICA PARA EL EXAMEN ----
library(tidyverse)
library(readr)
setwd("C:/Users/paula/Desktop/Facultad/Analitica Descriptiva")
znprop <- read_csv('propznorte.csv')

#-----1) Missings y Outliers
#--A)Los faltantes de surface_in_m2 son aleatorios respecto a las habitaciones?

#para evaluar la independencia entre dos variables se usa el chi-cuadrado.
#para usar ese test debemos crear una tabla con las dos variables a analizar.

t <- table(is.na(znprop$surface_in_m2), znprop$rooms)
t2 <- table(is.na(znprop$surface_in_m2), is.na(znprop$rooms)) #nos dice si el faltante de uno dependen del faltante del otro
#el problema con la tabla de t2 es que rooms no tiene faltantes.

chisq.test(t)
#podemos ver en el resultado que el p-valor es mayor a 0.05, por ende no se rechaza la hipotesis de que son independientes.
#Por ende inferimos que las variables son independientes.

#El p-valor es la probabilidad de obtener estos datos, dado que H0 es correcta. 
#La probabilidad de obtener estos datos, si no hay relacion entre el faltante de superficie y el valor de las habitaciones es de 29%.
#Como mi nivel de significancia es del 5% y 29% es > 5%, rechazo H0, es decir que no hay relacion. 


#aca vemos que la distribucion de las superficies que faltan vs. los que no faltan son parecidas
znprop %>% ggplot(aes(x = rooms, y = is.na(surface_in_m2)))+
  geom_boxplot()

faltan <- znprop[is.na(znprop$surface_in_m2),]$rooms #me hice un vector de rooms en los cuales faltan un valor de superficie
no_faltan <- znprop[!is.na(znprop$surface_in_m2),]$rooms #me hice un vector de rooms en los cuales no faltan un valor de superficie

#Ahora vamos a comparar la funcion de distribucion acumulativa entre las 2 variables para ver si provienen de la misma distribucion.
#Para eso usamos el ks.test

ks.test(faltan, no_faltan)

#en este caso comparamos el promedio de ambas variables (la de las superficies que faltan y de las que no).  Como los promedios dan distinto, 
#podemos decir que las variables son independientes (porque si fuesen dependientes serían iguales).
t.test(faltan, no_faltan)

#--B)Hay outliers en la variable surface_in_m2? Estos valores atipicos, se corresponden con el precio?

boxplot(znprop$surface_in_m2)
summary(znprop$surface_in_m2)

znprop %>% ggplot(aes(surface_in_m2, price_aprox_usd, col = property_type)) + 
  geom_point() + scale_x_log10() #le agregamos esto de log para que se distribuyan mejor pos puntos
#en el grafico de arriba podemos visualizar como se distribuyen los puntos segun el precio y m2.

#en esta funcion se almacenan en TRUE los outliers
es_outlier <- function(x){
  sup <- quantile(x, 0.75, na.rm = T) + IQR(x, na.rm = T)* 1.5
  inf <- quantile(x, 0.25, na.rm = T) - IQR(x, na.rm = T)* 1.5
  es <- (x > sup) | (x<inf)
  return(es)
}
es_outlier(znprop$surface_in_m2)

#en esta funcion devuelve directamente los outliers
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}
extraer_outliers(znprop$surface_in_m2)

#Primera forma de pensarlo es ver si los outliers de los m2 se corresponden con la variable price_aprox_usd.
#Una perspectiva es comparar si los outliers de los m2, son los mismos que los outliers del precio. 
#Para eso, vamos a usar un test de chi-cuadrado, y para eso necesitamos una tabla de contingencias.

precio_tamano <- table(es_outlier(znprop$surface_in_m2), es_outlier(log(znprop$price_aprox_usd))) #le metimos un log porque sino nos 1 daba el p-valor 
chisq.test(precio_tamano)
#En este caso, como el p-valor es mayor NO ENDTENDI ESTA PRIMERA FORMA PORQUE DA MAL EL TEST


#Segunda forma de pensarlo es viendo si los precios de los terrenos con mas m2 son mayores a los precios de los que no son outliers.
outliers2 <- znprop[es_outlier(znprop$surface_in_m2),]$price_aprox_usd #muestra los precios de los outliers
no_outliers2 <- znprop[!es_outlier(znprop$surface_in_m2),]$price_aprox_usd #muestra los precios de los outliers
#El el siguiente test de hipotesis quiero probar que la media de los precios de los outliers es mayor a la media de los precios de los que no son outliers.
#El H0 es que la media de los outliers es mayor a la media de los no_outliers
t.test(outliers2, no_outliers2, alternative = "greater") 
#comparamos las medias y podemos ver como el p-valor es mayor a 0.05, entonces los precios de los que son outliers de m2 son mayores a los precios de los que no son outliers.


#Tercera forma de pensarlo es ver los 10 de mayor precio se corresponde con los 3 de mayor superficie
t3 <- table(ntile(znprop$surface_in_m2, 10), ntile(znprop$price_aprox_usd, 3)) #ntile crea la cantidad de agrupaciones que vos le especificas
chisq.test(t3)



#--C)Seleccionar una metodologia de imputacion y aplicarla
#La imputacion se hace sobre los missings. 

#La primera forma para imputar es con el promedio, trimmeando los outliers, o con la mediana:
promedio_s <- mean(znprop$surface_in_m2, na.rm = T, trim = 0.2)
mediana_s <- median(znprop$surface_in_m2, na.rm = T)

znprop$surface_in_m2[is.na(znprop$surface_in_m2)] = promedio_s


#-----2) Test de Hipotesis

#Generar una hipotesis, determinar el tipo de test aplicable, e interpretar resultados de
##Las que sean no-numericas hay que discretizarlas.

#--A)  Una variable numerica y una no-numerica

#Quiero ver si los precios de los departamentos en promedio con mas caros que las casas.
#Numerico son los precios y categórico es el tipo de propiedad.
precio_depto <- znprop$price_aprox_usd[znprop$property_type == "apartment"]
precio_casa <- znprop$price_aprox_usd[znprop$property_type == "house"]
t.test(precio_depto, precio_casa, alternative = "greater")


#--B)  Una variable no-numerica y una no-numerica

#Queremos probar si el tipo de propiedad es independiende de la cantidxad de habitaciones.
chisq.test(table(znprop$property_type, znprop$rooms))

#--C)  Una variable numerica y una numerica

#Quiero hacer un test de correlacion para ver si el precio tiene correlación con los m2
cor.test(znprop$price_aprox_usd, znprop$surface_in_m2)

#3) Procesamiento de Texto
#A) Generar una variable nueva que indique si la preparacion de la bebida
# lleva leche de algun tipo usando t?cnicas de procesamiento de texto
# vistas en clase
star$Beverage_prep
star = read_csv('starbucks.csv')
View(star)
#4) Arboles y Cluster
#A) Realizar un analisis exploratorio usando un arbol de decisi?n para los tipos de bebida,
c("Coffee","Smoothies","Tazo Tea Drinks")
#Seleccione 4 variables predictivas
#B) Con la misma base de datos seleccionada anteriormente, realice un analisis de clusters

znprop$surface_in_m2