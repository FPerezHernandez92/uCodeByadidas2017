#Cargamos las librerías necesarias
library(tidyr)
library(dplyr)

path <- "C:/Users/fvilchez/Dropbox/uCode"
setwd(path)
files <- list.files(path)
aerolineas <- read.csv(files[1])

#Eliminamos las variables que no nos aportan infromación
aerolineas$X <- NULL
aerolineas$Year <- NULL

aerolineas$DepTime <- as.character(aerolineas$DepTime)

tiempo_4_digitos_indices <- which(nchar(aerolineas$DepTime)==4)
tiempo_4_digitos <- aerolineas$DepTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(aerolineas$DepTime)==3)
tiempo_3_digitos <- aerolineas$DepTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")

aerolineas$DepTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
aerolineas$DepTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora


aerolineas$CRSDepTime <- as.character(aerolineas$CRSDepTime)

tiempo_4_digitos_indices <- which(nchar(aerolineas$CRSDepTime)==4)
tiempo_4_digitos <- aerolineas$CRSDepTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(aerolineas$CRSDepTime)==3)
tiempo_3_digitos <- aerolineas$CRSDepTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")

aerolineas$CRSDepTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
aerolineas$CRSDepTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora


aerolineas$ArrTime <- as.character(aerolineas$ArrTime)

tiempo_4_digitos_indices <- which(nchar(aerolineas$ArrTime)==4)
tiempo_4_digitos <- aerolineas$ArrTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(aerolineas$ArrTime)==3)
tiempo_3_digitos <- aerolineas$ArrTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")

aerolineas$ArrTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
aerolineas$ArrTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora




aerolineas$CRSArrTime <- as.character(aerolineas$CRSArrTime)

tiempo_4_digitos_indices <- which(nchar(aerolineas$CRSArrTime)==4)
tiempo_4_digitos <- aerolineas$CRSArrTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(aerolineas$CRSArrTime)==3)
tiempo_3_digitos <- aerolineas$CRSArrTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")

aerolineas$CRSArrTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
aerolineas$CRSArrTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora










path2 <- "C:/Users/fvilchez/Desktop"
setwd(path2)
files2 <- list.files(path2)
aeropuertos_info <- read.csv(files2[3])
