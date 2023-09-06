#funcion ifelse
esta_lloviendo = c(1,0,0,1,0,1,0,0,1,1)
llevar_paraguas = ifelse(esta_lloviendo == 1, yes = T, no = F)


#funcion case_when: se usa cuando hay mas de una condición.  Se usa recorriendo vectores.
nota.del.final = c(7,5,4,9,2,8,1,6,8,6)
condicion = dplyr :: case_when(
  nota.del.final < 4 ~ "recursa"
  nota.del.final < 7 ~ "recupera"
  T ~ "promociona"
)

datasets::ChickWeight$weight #seleccionamos una sola columna, para recorrerla como un vector
categoria_peso = case_when(
  datasets::ChickWeight$weight < 70 ~ "bajo",
  datasets::ChickWeight$weight < 120 ~ "mediano",
  datasets::ChickWeight$weight >= 120 ~ "alto",
  T ~ "error"
)
unique(categoria_peso) #nos quedamos con una variable categorica.
#unique te muestra las categorias de un vector que no es lo mismo que un as.factor
#as.factor setea que los valores de un vector solo sean de las categorias que hay, no se puede cambiar o agregar un dato nuevo que no sea dentro de una de esas categorias


datos <- datasets::ChickWeight
#forma 1
datos$categoria = ifelse(weight > mean(weight, na.rm = T), "mayor_promedio", "menor_promedio") #el na.rm = T es remove los NA, evitarlos para contar el promedio
datos %>% mutate(weight_x_time =
                   case_when(
                     weight > mena(weight, na.rm = T) ~ "mayor_promedio",
                     T ~ "menor_promedio"
                   )
                 ) %>% group_by(categoria) %>% summarise(n = n()) %>%
  ungroup() %>%
  mutate(p = n/sum(n))

#forma 2
datos %>% mutate(
  categoria = ifelse(weight > mean(weight, na.rm = T), "mayor_promedio", "menor_promedio"))%>%
  group_by(categoria) %>% summarise(n=n()) %>% 
  ungroup() %>% 
  mutate(p = n/sum(n))
)

#funcion For

#funcion While

#---- Ejercicios de For-Loop

#1) 456267 es un numero primo?

es_primo <- "primo"
divisores <- c()
for (i in 2:456266){
  if (456267%%i == 0){
    divisores <- append(divisores, i)
    es_primo <- "no es primo"
  }
}

rta <- ifelse(es_primo=='primo', "primo", "no es primo")
rta

if (rta == "no es primo"){
  divisores
}

#2) Obtener los numeros primos del 1 al 10000
n_primos <- c(1,2)
for (i in 3:10000){
  divisores <- c()
  for (j in 2:i-1){
    if (i%%j == 0){
      divisores <- append(divisores, i)
    }
  }
  if(length(divisores)==0){
    n_primos <-append(n_primos,i)
  }
}
n_primos

#3) Convertir el detector de primos del punto 1) con un while
es_primo <- "primo"
divisores <- c()
i = 2
while (es_primo == "primo" & i <= 10000){
  if (456267%%i == 0){
    divisores <- append(divisores, i)
    es_primo <- "no es primo"
  }
  i <- i + 1
}

rta <- ifelse(es_primo=='primo', "primo", "no es primo")
rta

#4) Realize un muestreo aleatorio entre 0 y 1,000 (numeros enteros) hasta obtener 5 numeros divisibles por 4.
resultados = c()
while(4>length(resultados)){
  s = round(runif(1,1,1000),0)
  if(s%%4==0){
    resultados = c(resultados,s)
  }
}
#5) Crea un for-loop que imprima este patron:
#  * 
#  * * 
#  * * * 
#  * * * * 
#  * * * * * 
#  * * * * 
#  * * * 
#  * * 
#  *
for(i in 1:9){
  if(i > 5){
    print(rep("*",10-i))
  }
  else{
    print(rep("*",i))
  }
  
}
#6) Usando while, encontre los primeros 100 numeros de la serie de fibonacci
numeros_fibo = c(0)
i = 1
while(length(numeros_fibo)<100){
  if(length(numeros_fibo)<=1){
    numeros_fibo=append(numeros_fibo,1)    
  }
  else{
    next_fibo = numeros_fibo[i-1]+numeros_fibo[i-2]
    numeros_fibo=append(numeros_fibo,next_fibo)
  }
  i = i +1
}

#7) Del 1 al 1000, imprimi los divisibles por 3 que no sean divisibles por 2
for(i in 1:1000){
  if(i%%3==0 & i%%2!=0){
    print(i)
  }
}







