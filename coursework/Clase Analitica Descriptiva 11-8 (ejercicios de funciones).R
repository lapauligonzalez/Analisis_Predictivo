#Data Frames - dplyr - Funciones

#   ---------- Ejercicio 1: El dataset datasets::ToothGrowth tiene los resultados de un experimiento que busca 
#   evaluar que ocurre con los dientes de conejillos de indias al agregar dos tipos de vitaminas a su dieta 
#Columnas
library("datasets")
library("tidyverse")
datos <- datasets :: ToothGrowth

#[,1]	len	numeric: longitud de los dientes
#[,2]	supp	factor: Que tipo vitamina se agrego (VC or OJ) 
#[,3]	dose	numeric	: Dosis, Cuanta vitamina se agrego en milligramos/dia

#Responder:
#----a) ¿Cual es la longitud promedio de los dientes por cada tipo de vitamina?

#cada vez que queremos ejecutar una función (suma, promedio, etc) por cada grupo, hacemos la función group by y luego un summarise
#el summarise va a contener la función que le vamos a aplicar a los grupos
promedios <- datos%>% group_by(supp) %>% summarise(promedio_largo = mean(len))
promedios

datos%>% group_by(supp) %>% sample(3) #tambien le podemos pedir que nos 3 samples de cada grupo

#----b) Obtener la fila correspondiente al conejillo de indias con el diente mas largo.
#opcion 1:
nro_fila <- which.max(datos[,1]) #la función which.max devuelve el numero de fila que tiene el valor maximo de un vector
datos[nro_fila,] #nos muestra la fila, con todas sus columnas. Cuando no especificamos la fila, nos devuelve todas

#opcion 2:
datos %>% filter(len == max(len))

#----c) Ordernar el dataset de menor a mayor de acuerdo a la longitud de los dientes.
datos_ordenados <- arrange(datos, len) 
datos_ordenados

#----d) Seleccionar los registros en el cual "len" toma valores entre 5 y 10
datos %>% filter(len >= 5 & len <= 10)

#   ---------- Ejercicio 2: Usando el dataset de `ChickWeight` brinda información acerca de un experimento que probó distintas dietas en distintas pollos:
#  ```{r}
#datasets::ChickWeight

datos2 <- datasets::ChickWeight
datos2

#1) ¿Cual es el peso promedio de los pollos? ¿Cual es el peso por promedio en cada grupo?
promedio <- mean(datos2[,1])
promedio
grupos <- datos2 %>% group_by(Chick) %>% summarise(promedio = mean(weight))
grupos %>% View() #se usa para abrir el dataframe en una pestaña

#2) ¿Cuanto pesan los 10 pollos más liviandos que comieron de la dieta `1`? ¿Cuales son los mas pesados de esa dieta?
dieta1 <- datos2 %>% filter(Diet == 1)
d1a <- head(arrange(dieta1, weight), 10)
pesos_min <- d1a[,1]
pesos_min

d1t <- tail(arrange(dieta1, weight), 10)
pesos_max <- d1t[,1]
pesos_max

#3) De la base total, obtener los registros con indice impar.
indice <- 1:length(datos2$Chick)

datos2 %>% mutate(datos2, indice) %>% filter(indice %% 2 == 1)#le agrego la columna nueva con los indices y le filtro los impares

#4) Discretizar  la variable `weight`: ¿Cuantos pollos pesan mas de 50 lbs? ¿Cuantos pollos pesan mas de 50 lbs? ¿cuantos menos? ¿cuantos entre 50 y 100?¿cuantos más de 100?
#discretizar es sumarle una columna asignandole una categoria...?

datos2 %>% mutate(
  cuanto_pesan = case_when(
    weight < 50 ~ "menor a 50",
    weight < 100 ~ "menor a 100",
    weight >= 100 ~ "mayor a 100"
  )
) %>% group_by(cuanto_pesan) %>% summarise(n=n())#otra alternativa seria poner count() sin el summarise(..)

#5) Del punto anterior se generaron categorías, para cada categoría, ¿cuantos pollos comieron cada dieta?
datos2 %>% mutate(
  cuanto_pesan = case_when(
    weight < 50 ~ "menor a 50",
    weight < 100 ~ "menor a 100",
    weight >= 100 ~ "mayor a 100"
  )
) %>% group_by(cuanto_pesan, Diet) %>% summarise(n=n())
  
#   ---------- Ejercicio 3:Funciones
#a) Crear una funcion que tome como parametro un numero e indique si es divisible por 5.
div_5 <- function(x){
  divisible <- "no es divisible por 5"
  if (x %% 5 == 0){
    divisible <- "es divisible por 5"
  }
  return(divisible)
}

div_5(32)

#b) Crear una funcion que tome como parametro una nota del 1 al 10 e indique si recursa (1 al 3), aprueba (4 a 6), o promociona (7 al 10)
calcular_estado <- function(nota){
  estado <- case_when(
    nota <= 3 ~ "recursa",
    nota <= 6 ~ "aprueba",
    nota <= 10 ~ "promociona"
  )
  return(estado)
}
calcular_estado(4)

#c) Creer una funcion que tome como parametro un vector e indique cuales son los valores mayor al promedio y cuales menores
#el output debe ser un vector de la misma longitud que el original y que cada elemento 
#sea "TRUE" si es mayor al promedio y "FALSE" si es menor al promedio

#no me da este
calcular <- function(v){
  promedio <- mean(v)
  nuevo <- c()
  for (i in v){
    if (i < promedio){
      nuevo <- append(nuevo, FALSE)
    } else {
      nuevo <- append(nuevo, TRUE)
    }
  return(nuevo)
  }

x <- c(10,20,30,40)    
calcular(x)

  






