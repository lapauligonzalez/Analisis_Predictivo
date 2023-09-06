#creamos vectores y los concatenamos
x <- c(2,3,4)
sec_5_20 <- 5:20
numeros <- c(sec_5_20,x)
concatenacion <-c(numeros, sec_5_20, 4:1, "a", "b")
concatenacion[c(3,5,7)] #nos tira el tercer, quinto y septimo elemento dentro del vector
x[-2] #selecciona todo menos el segundo elemento del vector
sec_5_20[c(0:3)] #selecciona los primeros 3 elementos del vector

#se pueden realizar operaciones aritmeticas con vectores
y <- c(0,7,8)
y - 4 #nos devuelve los elementos del vector restados por 4
y*3 #nos devuelve los elementos del vector multiplicado por 3


#creamos matrices
m <- matrix(1:6, nrow=2, ncol=3) 
m[1,2]#nos da la primer fila, segunda columna
m[4] #nos trae el cuarto elemento

matrix(data=c(1,2,3,4,5,6),nrow=2,ncol=3, byrow=FALSE) #va agregando los datos por columnas
matrix(data=c(1,2,3,4,5,6),nrow=2,ncol=3, byrow=TRUE) #va agregando los datos por filas


#rbins nos permite anexar filas (crear matrices)
r <- rbind(1:3, 4:6)
r
mymat <- rbind(c(1,3,4), 5:3, c(100,20,90), 11:13)
mymat
#rbins nos permite anexar columnas (crear matrices)
c <- cbind(1:3, 4:6)
c

#estas funciones nos sirven para ir llenando una matriz en un loop

#calcula filas y columnas de una matriz
dim(mymat)
dim(mymat)[1] #calcula la cantidad de filas de una matriz
nrow(mymat) #calcula la cantidad de filas de una matriz
ncol(mymat) #calcula la cantidad de columnas de una matriz

#un ejercicio random con iris
dim(iris)
iris1 <- iris
iris2 <- iris
iris3<- rbind(iris1, iris2)
dim(iris3)

#extrayendo valores de una matriz
A <- matrix(c(0.3,4.5,55.3,91,0.1,105.5,-4.2,8.2,27.9) ,nrow=3,ncol=3)
A[2:3, ] #nos devuelve la segunda y tercera fila de A y como dejamos un espacio en blanco, nos devuelve todas las columnas
A[,c(3,1)] #nos devuelve todas las filas de la tercera y primera columnas de A
A[c(3,1),2:3] #nos devuelve la tercera y primera fila de las columnas 2 y 3
diag(A) #nos trae la diagonal de la matriz
A[-1, 3:2] #nos elimina la fila 1, y nos da las columnas 3 y 2 en ese orden.

#sobreescribiendo una matriz
A[2,] <- 1:3 #agarra la segunda fila y la reemplaza por un 1,2 y 3.
A
A[c(1,3),2] <- 900 #sobreescribe los elementos de la segunda columna, filas 2 y 3 con un 900
A

B <- matrix(data = c(3,4,1,2), nrow=2, ncol=2, byrow = FALSE)
B
B*2
solve(B) #calcula la inversa de la matriz
t(B) #calcula la traspuesta de la matriz
t(t(B)) #la matriz original


#----ejercicio medio random pero bueno----
x0 <- rep(1,50) #crea un vector de unos repetidos 50 veces
x1 <- rnorm(50, mean = 5, sd = 2) #crea un vector de 50 valores, con una
#distibucion normal, media de 5 y desvio estandar de 2
x2 <- rnorm(50, mean = 50, sd = 200)
E <- rnorm(50, mean = 0, sd = 1)

Y <- 15 + 3*x1 + 0.4*x2 + E
XX <- cbind(x0,x1,x2)
B <- (t(XX)%*%(XX))^(-1)%*%(t(XX))%*%Y #cuando uno multiplica matrices se usa %*% en vez de *, y tienen que coincidir la dimensiones
dim(B)

#para crear un array
AR <- array(data = 1:24, dim = c(3,4,2))
AR

BR <- array(1:24, dim = c(3,4,2,3)) #filas, columnas (de las matrices de los canales), canales(hojas) y bloques
BR #nos da 2 matrices por cada bloque, que son 3

#estrayendo valores de un array
AR[2,,2] #nos da la segunda fila de la segunda capa
BR[2,1,1,3] #nos da la segunda fila de la primera columna de la primera capa del tercer bloque
BR[1,,,1] #nos da la primeras filas del primer bloque
BR[,,2,] #nos devuelve todos los valores de la segundas capas 

#creando listas

foo <- list(matrix(data=1:4,nrow=2,ncol=2),c(T,F,T,T),"hello")
foo
length(foo)
foo[[1]] #nos devuelve el primer elemento de una lista
foo[[3]]
foo[[1]] + 5.5 #a los elementos del primer elemento de la lista sumarles 5.5
foo[[1]][1,2] #del primer elemento, selecciona la primera fila y segunda columna


#manejo de dataframes
mydata <- data.frame(person = c("Peter", "Lois", "Meg", "Chris", "Stewie"), age = c(42,40,17,14,1), sex = factor(c("M", "F", "F", "M", "M")))
mydata
mydata[2,2] #se obtienen los datos como una matriz
mydata[3:5,3]

#agregamos un nuevo registro
newrecord <- data.frame(person = "Brian", age = 7, sex(factor("M",levels = levels(mydata$sex)))
mydata <- rbind(mydata, newrecord)

#agregamos una nueva columna
funny <- c("high", "high", "low", "med", "high")
funny <- factor(x = funny, levels = c("low", "med", "high")) #aca le decimos que el vector tiene niveles que puede tomar
mydata <- cbind(mydata, funny)
mydata
mydata$edad_al_cuadrado = data$age^2

