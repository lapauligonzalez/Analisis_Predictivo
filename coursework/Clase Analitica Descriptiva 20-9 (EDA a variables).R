#tendencia para los grades

#media, mediana y moda
summary(grades_editado)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

library(dplyr)
colnames(grades_editado)
grades_editado$gpaall

Mode(grades_editado$gpaall)

#analizando outliers
boxplot(grades_editado$gpaall) #hay un solo outlier

mean(grades_editado$gpaall)
mean(grades_editado$gpaall, trim = 0.1)
#el trim 0.1 te saca los valores por debajo del 5% y por encima del 95%
#si les cortamos las puntas, la media es mayor
#trimmeamos los datos para ver cuantos outliers tenemos
#si cambia mucho el promedio trimmeando, es porque tenemos una banda de outliers

#evaluando el outlier con el valor z
z <- (grades_editado$gpaall-mean(grades_editado$gpaall))/sd(grades_editado$gpaall)
z #cuantos desvios estandar están los valores respecto a la media
z > 3
z[abs(z)>2] #devuelve los valores que tienen una desviacion mayor a 2
which(abs(z) > 2) #indices de los que tienen el z mayor a 2
