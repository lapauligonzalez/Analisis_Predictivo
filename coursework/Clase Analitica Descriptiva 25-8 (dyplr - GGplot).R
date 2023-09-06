a = iris$Species
b = iris$Sepal.Length < 5
a[b] #nos devuelve 


iris %>% ggplot(aes(Sepal.Length, col = Species)) + geom_bar() #colorea los bordes
iris %>% ggplot(aes(Sepal.Length, fill = Species)) + geom_bar() #colorea todo
iris %>% ggplot(aes(Sepal.Length, fill = Species)) + geom_bar(alpha = 0.3) #le agrega una transparencia en la geometria

#grafico heterogéneo cuando las variables a las geometrías son distintas
iris %>% ggplot()+ #si no se declaran parametros aca, tenemos que si o si declararlas en las geometrias
  geom_point(aes(Sepal.Length, Sepal.Width))+ #si las geometrias no tienen variables sale error (si en el ggplot no pusimos nada de aes())
  geom_line(aes(Petal.Length, Sepal.Width), col = "red") #si no pusimos una aes(), va a tomar la aes() que pusimos en los () del ggplot

#grafico homogeneo cuando las variables a las geometrías son iguales
iris %>% ggplot()+
  geom_point(aes(Sepal.Length, Sepal.Width))+
  geom_line(aes(Sepal.Length, Sepal.Width))

#contamos cuantos pollos hicieron cada dieta, y el promedio de weight y graficamos
datasets::ChickWeight %>% group_by(Diet) %>%
  summarise(count = n(), mean_weight = mean(weight)) %>%
  ggplot()+geom_bar(aes(x = Diet, y = count, fill = mean_weight), stat = "identity") #ponemos el stat este cuando agregamos dos variables (x e y) en geom_bar

datasets::ChickWeight %>% ggplot(aes(weight)) + geom_histogram(bins = 5) #bins cambia la cantidad de barras
datasets::ChickWeight %>% ggplot(aes(weight)) + geom_bar()

#grafico que muestra la densidad de los pollos por tipo de dieta
datasets::ChickWeight %>% 
  ggplot(aes(weight, fill = Diet))+
  geom_density(alpha = 0.4)
#los pollos con la dieta 4 son los mas gordos
#la frecuencia más alta de la dieta 1 y 2 esta en 50 casi

# nos acercamos mas en la dieta 1 y 2
datasets::ChickWeight %>% filter(Diet ==1 | Diet == 2) %>% # el | es un or
  ggplot(aes(weight, fill = Diet))+
  geom_density(alpha = 0.4)

#mostramos como fue cambiando el weight en el tiempo por dieta
dt <- datasets::ChickWeight %>%
  group_by(Time, Diet) %>%
  summarise(peso = mean(weight))

dt %>%
  ggplot(aes(x = Time, y =peso, col = Diet))+
  geom_line()+
  geom_point()
  
  ggplot()+
  geom_line(aes(x = Time, y = weight, col = Diet))

#te crea una linea smooth con todos los registros
datasets::ChickWeight %>%
  ggplot(aes(Time, weight))+
  geom_smooth()

#otra opcion es hacer todo esto:
datasets::ChickWeight %>% group_by(Time) %>% 
  summarise(weight = mean(weight)) %>%
  ggplot(aes(Time, weight))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm") #linear model
  
install.packages("ggthemes")  
library(ggthemes)

#histograma de ggplot
datasets::ChickWeight %>%
  ggplot()+
  geom_histogram(aes(weight, fill = Diet), bins = 10)+
  #theme_classic() #cambia el fondo del grafico
  #ggthemes::theme_fivethirtyeight()
  ggthemes::theme_stata()
  
#histograma nativo de R
hist(datasets::ChickWeight$weight)

#crea un boxplot
datasets::ChickWeight %>%
  ggplot()+
  geom_boxplot(aes(weight, col = Diet))
  theme_classic() 
# la dieta 3 es la que mas peso tiene pero varia un monton los pesos