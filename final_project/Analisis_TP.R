install.packages("studentlife")
library(tidyverse)
library(studentlife)
install.packages("anytime")
library(anytime)
library(ggplot2)
library(data.table)
library(dplyr)

#almacenar la db en la variable d
d <- tempdir()
download_studentlife(location = d, url = "rdata")

#me devuelve una muestra de 150 registros
s <- studentlife::load_SL_tibble(location = d)
View(s)

#me devuelve todos los alumnos pero sin la columna "level"
schema <- "EMA"
table <- "Stress"
stress <- readRDS(paste0(d, "/dataset_rds/", schema, "/", table, ".Rds"))
View(stress)

#Analizando las notas
grades <- studentlife::load_SL_tibble(location = d)
View(grades)

summary(grades)

grades <- grades %>% 
  mutate(gpa_allvs13s = gpa_13s-gpa_all)

#scatter
grades1 <- ggplot(data = grades)+
  geom_point(aes(uid,gpa_all), color = "black")+
  geom_point(aes(uid,gpa_13s), color = "blue")+
  geom_point(aes(uid,cs_65), color = "green")
  
grades1 + labs(title = "Notas por Alumno", x = "ID", y = "GPA")

#scatter con el progreso
progreso <- ggplot(data = grades)+
  geom_point(aes(gpa_allvs13s, uid), color = "black")+
  geom_vline(xintercept = 0,
             linetype = 1,
             color = 2)
progreso + labs(title = "Contraste entre promedio cuatrimetral y promedio total de su carrera",
                x = "Diferencia entre promedios", y = "ID")

#cuento cuantos mejoraron o empeoraron su promedio 
empeoraron <- sum(grades$gpa_allvs13s < 0)
mejoraron <- sum(grades$gpa_allvs13s > 0)

#boxplot del progreso
progreso2 <- ggplot(data = grades)+
  geom_boxplot(aes(gpa_allvs13s), color = "black")
progreso2 + labs(title = "Analisis del Contraste entre promedio cuatrimetral y promedio total de su carrera",
                 x = "Diferencia entre promedios")

#3 boxplot juntos
par(mfrow=c(1,3), mar=c(1,1,1,1))
boxplot(grades$gpa_all, col = "gray", main = "Promedio total")
boxplot(grades$gpa_13s, col = "blue", main = "Promedio semestral")
boxplot(grades$cs_65, col = "green", main = "Promedio materia CS_65")


#Analizando deadlines/tareas
deadline <- studentlife::load_SL_tibble(location = d)
View(deadline)

#cantidad de tareas por alumno en un cuatrimestre

tareas <- deadline %>% group_by(uid) %>% summarise(freq = sum(deadlines))
View(tareas)


#Analizando la materias que cursa cada alumno

#cantidad de materias 
nrow(agrupacion_materias)

#contar cuantas personas hacen cada materia y el porcentaje
agrupacion_materias <- class1 %>% group_by(subjet) %>% summarise(cantidad= n()) 
agrupacion_materias <- agrupacion_materias %>% filter(is.na(subjet) == FALSE)
agrupacion_materias <- arrange(agrupacion_materias, desc(cantidad))
agrupacion_materias <- mutate(agrupacion_materias, porcentaje = (cantidad/sum(cantidad))*100)
View(agrupacion_materias)

summary(agrupacion_materias)
hist(x = agrupacion_materias$cantidad, main = "Histograma de la cantidad de veces que", 
     xlab = "Cantidad", ylab = "Frecuencia")
boxplot(agrupacion_materias$cantidad)

#contar cuantas materias cursa cada alumno
sin_na <- na.omit(class1)
sin_na
sin_na %>% group_by(uid) %>% summarise(cant_materias = n())
class1[,1]


#Analizando el piazza
piazza <- studentlife::load_SL_tibble(location = d)
View(piazza)
summary(piazza)

#6 boxplot juntos
par(mfrow=c(1,6), mar=c(1,1,1,1))
boxplot(piazza$days_online, col = "gray", main = "Dias en linea")
boxplot(piazza$views, col = "blue", main = "Visitas")
boxplot(piazza$contributions, col = "green", main = "Contribuciones")
boxplot(piazza$questions, col = "light blue", main = "Preguntas")
boxplot(piazza$notes, col = "darkgoldenrod1", main = "Notas")
boxplot(piazza$answers, col = "darkmagenta", main = "Respuestas")

#2 scatter juntos de las visitas y days_online
ggplot(data = piazza)+
  geom_point(aes(uid,piazza$days_online), color = "azure4")+
  geom_point(aes(uid,piazza$views), color = "blue")+
  labs(title = "Visitas y dias en linea por alumno",
       x = "ID", y = "Visitas / Dias en linea")+
  geom_hline(yintercept = mean(piazza$days_online),
             linetype = 1,
             color = "azure4")+
  geom_hline(yintercept = mean(piazza$views),
             linetype = 1,
             color = "blue")

cor.test(piazza$days_online, piazza$views)

#valores atipicos
head(arrange(piazza, desc(contributions)), 2) #outliers contribuciones
head(arrange(piazza, desc(questions)), 4) #outliers preguntas
head(arrange(piazza, desc(notes)), 5) #outliers notes
head(arrange(piazza, desc(answers)), 5) #outliers respuestas

#fijandome algo
a <- studentlife::load_SL_tibble(location = d)
View(a)


#stress
summary(stress$level)
length(stress$level)
promedio_stress <- mean(stress$level, na.rm = T)

#imputando faltantes
stress$level[is.na(stress$level)] = promedio_stress

#scatter con el estres
cantidad_rtas <- stress %>% group_by(uid) %>% summarise(cantidad=n())
promedio_s_sumado <- mean(cantidad_rtas$cantidad)
respuestas <- ggplot(data = cantidad_rtas)+
  geom_point(aes(cantidad_rtas$cantidad, uid), color = "black")+
  geom_vline(xintercept = promedio_s_sumado,
             linetype = 1,
             color = 2)
respuestas + labs(title = "Cantidad de veces que respondieron la encuesta de estres",
                x = "Cantidad de respuestas", y = "ID")

#calculando el estres promedio por estudiante
cantidad_estres <- stress %>% group_by(uid) %>% summarise(promedio = mean(level))

setwd("C:/Users/paula/Desktop/Facultad/Analitica Descriptiva/studentslife_dataset/EMA/response/Stress")
write.csv(cantidad_estres, file="preomedios_stress.csv")

promedio_a_sumado <- mean(cantidad_estres$promedio)
boxplot(cantidad_estres$promedio)
estres <- ggplot(data = cantidad_estres)+
  geom_point(aes(cantidad_estres$promedio, uid), color = "black")+
  geom_vline(xintercept = promedio_a_sumado,
             linetype = 1,
             color = 2)
estres + labs(title = "Promedio de estrés por estudiante",
                  x = "Nivel de estres promedio", y = "ID")

boxplot(cantidad_estres$promedio,
        main="Boxplot de los promedios de estres de los estudiantes",
        xlab="Promedio de nivel de estres",
        horizontal=TRUE)


#depresion
depresion <- PHQ.9

#le cambio los strings a numeros
for(i in 3:ncol(depresion)){
  for(j in 1:nrow(depresion)){
    if(depresion[j,i] == "Not at all"){
      depresion[j,i] = "0"
    }
    else if(depresion[j,i] == "Several days"){
      depresion[j,i] = "1"
    }
    else if(depresion[j,i] == "More than half the days"){
      depresion[j,i] = "2"
    }
    else{
      depresion[j,i] = "3"
    }
  }
}

#elimino la ultima columna y agrego otra calculando el score
depresion <- depresion[,-12]
#le cambio el nombre a las columnas
colnames(depresion)[3] = "Q1"
colnames(depresion)[4] = "Q2"
colnames(depresion)[5] = "Q3"
colnames(depresion)[6] = "Q4"
colnames(depresion)[7] = "Q5"
colnames(depresion)[8] = "Q6"
colnames(depresion)[9] = "Q7"
colnames(depresion)[10] = "Q8"
colnames(depresion)[11] = "Q9"

#calcular el score
depresion$Q1 <- as.numeric(as.character(depresion$Q1))
depresion$Q2 <- as.numeric(as.character(depresion$Q2))
depresion$Q3 <- as.numeric(as.character(depresion$Q3))
depresion$Q4 <- as.numeric(as.character(depresion$Q4))
depresion$Q5 <- as.numeric(as.character(depresion$Q5))
depresion$Q6 <- as.numeric(as.character(depresion$Q6))
depresion$Q7 <- as.numeric(as.character(depresion$Q7))
depresion$Q8 <- as.numeric(as.character(depresion$Q8))
depresion$Q9 <- as.numeric(as.character(depresion$Q9))

solo_valores_depr <- depresion %>% select(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9)
score <- rowSums(solo_valores_depr)

score_depresion <- depresion$uid
score_depresion <- cbind(score_depresion, depresion$type)
score_depresion <- cbind(score_depresion, score)
colnames(score_depresion)[2] = "type"

score_depresion <- as.data.frame(score_depresion)
score_depresion <- score_depresion %>% mutate(Categoria_depresion =
                                   case_when(score <= 4 ~ "Minimal[1-4]", 
                                             score <= 9 ~ "Minor[5-9]",
                                             score <= 14 ~ "Moderate[10-14]", 
                                             score <= 19 ~ "Moderately Severe[15-19]",
                                             score > 19 ~ "Severe[19-27]")
)
colnames(score_depresion)[1] = "uid"

#creo boxplots de la depresion
boxplot(as.numeric(score_depresion$score))

boxplot(as.numeric(score)~type,
        data=score_depresion,
        main="Boxplot de depresion pre y post estudio",
        xlab="Score de depresion",
        ylab="Previo y posterior al estudio",
        col="gray",
        border="black",
        horizontal=TRUE
)

#saco outliers
pre <- score_depresion %>% filter(type == "pre")
boxplot(as.numeric(pre$score))
post <- score_depresion %>% filter(type == "post")

extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}

extraer_outliers(as.numeric(pre$score))
extraer_outliers(as.numeric(post$score))

setwd("C:/Users/paula/Desktop/Facultad/Analitica Descriptiva/studentslife_dataset/survey")
write.csv(score_depresion, file="score_depresion.csv")

#scatter de la depresion - no se puede hacer porque hay valores que faltan en el post


#analizando el sleep
install.packages('missForest')
library(missForest)

install.packages('missForest')
library(missForest)

#imputamos faltantes
df_sleep = SleepRegu %>% select(-uid, -location, -epoch, -weekday, -month, -date)
df_sleep = as.data.frame(df_sleep)

imp = missForest(df_sleep, verbose = TRUE, variablewise = FALSE)
imp

df_sleep_SinNa <- as.data.frame(imp$ximp) #aca cargamos los valores que encontro el algoritmo a un df
df_sleep_SinNa$hour = as.integer(df_sleep_SinNa$hour)

SleepRegu$hour <- df_sleep_SinNa$hour

sleep_sun = Sleep %>% filter(weekday == "sun")

#analizamos el sleep
summary(SleepRegu$hour)
boxplot(SleepRegu$hour,
        main = "Boxplot de las horas de sueño de los alumnos en el semestre", 
        xlab = "Horas de sueño",
        horizontal = TRUE)

#sacamos outliers
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}

out_sleep <- extraer_outliers(as.numeric(SleepRegu$hour))
unique(out_sleep)
cant_out_sleep <- length(extraer_outliers(as.numeric(SleepRegu$hour)))

#queremos saber cuales alumnos fueron los que tuvieron horas de sueño outliers
alumnos_out <- SleepRegu %>% filter(hour == 14 | hour == 15 | hour == 12 | hour == 1 | hour == 13 | hour == 0 | hour == 2 | hour == 16 | hour == 17 | hour == 19 ) %>%
  select(hour, uid)

#promedios de horas de sueño por alumno
promedios_sleep <- SleepRegu %>% group_by(uid) %>% summarise(promedio = mean(hour))
boxplot(promedios_sleep$promedio,
        main = "Promedios de hora de sueño de los alumnos",
        xlab = "Horas de sueño",
        horizontal = TRUE)

setwd("C:/Users/paula/Desktop/Facultad/Analitica Descriptiva/studentslife_dataset/survey")
write.csv(promedios_sleep, file="promedios_sleep.csv")

out_prom_sleep <- extraer_outliers(as.numeric(promedios_sleep$promedio))
uid_out <- promedios_sleep %>% filter(promedio == 5.043478 | promedio == 8.841270)


boxplot(hour~weekday,
        data=SleepRegu,
        main="Boxplot de horas de sueno por dia de semana",
        xlab="Hours",
        ylab="Weekday",
        col="gray",
        border="brown",
        horizontal=TRUE
)

depresion_OG <- provisorio %>% select(-promedio)
setwd("C:/Users/paula/Desktop/Facultad/Analitica Descriptiva/studentslife_dataset/survey")
write.csv(depresion_OG, file="depresion_OG.csv")

#sleep vs desempeño

cor.test(as.numeric(as.character(sleep_vs_desempeño$promedio)), as.numeric(as.character(sleep_vs_desempeño$gpa13s)))

#soledad vs desempeño

cor.test(as.numeric(as.character(soledad_vs_desempeño$soledad_post)), as.numeric(as.character(soledad_vs_desempeño$gpa13s)))


#sleep vs depresion

#imputamos faltantes
sleep_vs_depresion <- as.data.frame(sleep_vs_depresion)

imp = missForest(sleep_vs_depresion, verbose = TRUE, variablewise = FALSE)
imp

provisorio <- as.data.frame(imp$ximp) #aca cargamos los valores que encontro el algoritmo a un df

cor.test(provisorio$promedio, provisorio$score_post)
cor.test(provisorio$promedio, provisorio$score_post, method = "spearman")
cor.test(provisorio$promedio, provisorio$score_post, method = "kendall")

#Conversaciones
summary(Conversation.regu)
colnames(Conversation.regu)

boxplot(Conversation.regu$Tiempo_conversacion_prom, 
        xlab = "Promedio de minutos conversados", 
        main = "Promedios de minutos conversados por estudiante por día",
        col = "tomato", 
        horizontal = TRUE)

boxplot(Conversation.regu$Freq_prom_conversacion, 
        xlab = "Promedio de frecuencias de conversaciones", 
        main = "Promedios de la frecuencia de conversaciones por estudiante por día",
        col = "tomato", 
        horizontal = TRUE)

out_conve <- extraer_outliers(Conversation.regu$Tiempo_conversacion_prom)
extraer_outliers(Conversation.regu$Freq_prom_conversacion)

#correlacion entre conversacion y depresion

conv_vs_depr <- as.data.frame(conversation_vs_depresion)

imp = missForest(conv_vs_depr, verbose = TRUE, variablewise = FALSE)
imp

conv_vs_depr <- as.data.frame(imp$ximp) #aca cargamos los valores que encontro el algoritmo a un df

cor.test(conv_vs_depr$score_post, conv_vs_depr$Tiempo_conversacion_prom)
cor.test(conv_vs_depr$score_post, conv_vs_depr$Freq_prom_conversacion)


#desempeño vs depresion
desem_vs_depr <- as.data.frame(desempeño_vs_depresion)
imp = missForest(desem_vs_depr, verbose = TRUE, variablewise = FALSE)
imp
desem_vs_depr <- as.data.frame(imp$ximp) #aca cargamos los valores que encontro el algoritmo a un df

#con gpaall
cor.test(desem_vs_depr$gpaall, desem_vs_depr$depre_score_post)
cor.test(desem_vs_depr$gpaall, desem_vs_depr$depre_score_post, method = "spearman")

x=lm(formula = log10(desem_vs_depr$depre_score_post) ~ desem_vs_depr$gpaall)
summary(x)

#con gpa13s
cor.test(desem_vs_depr$gpa13s, desem_vs_depr$depre_score_post)
cor.test(desem_vs_depr$gpa13s, desem_vs_depr$depre_score_post, method = "kendall")

x=lm(formula = log10(desem_vs_depr$gpa13s) ~ desem_vs_depr$depre_score_post)
summary(x)


#desempeño vs conversaciones
colnames(desempeño_vs_conversacion)
cor.test(desempeño_vs_conversacion$gpa13s, desempeño_vs_conversacion$Freq_prom_conversacion)
cor.test(desempeño_vs_conversacion$gpa13s, desempeño_vs_conversacion$Tiempo_conversacion_prom)

summary(desempeño_vs_conversacion$Freq_prom_conversacion)

#conversaciones vs soledad
colnames(conversaciones_vs_soledad)
cor.test(conversaciones_vs_soledad$Tiempo_conversacion_prom, conversaciones_vs_soledad$soledad_post)
cor.test(conversaciones_vs_soledad$Freq_prom_conversacion, conversaciones_vs_soledad$soledad_post)

