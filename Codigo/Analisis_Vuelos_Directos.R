####################################### ANÁLISIS DE LOS VUELOS DIRECTOS ####################################

#Cargamos el dataset
path <- "C:/Users/fvilchez/Dropbox/uCode"
setwd(path)
files <- list.files(path)
Vuelos_Directos <- read.csv(files[4])
#Nos creamos una nueva columna donde lo que vamos hacer crearnos la fecha y la hora en formato tipo 
#date para ello vamos a realizar una serie de cambios. El primero de los cambios va ser el seguiente.

#En primer lugar nos creamos la variable año
Vuelos_Directos$Anyo <- 2008

#Ahora procedemos a poner la hhmm en formato hh:mm

Vuelos_Directos$DepTime <- as.character(Vuelos_Directos$DepTime)

tiempo_4_digitos_indices <- which(nchar(Vuelos_Directos$DepTime)==4)
tiempo_4_digitos <- Vuelos_Directos$DepTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(Vuelos_Directos$DepTime)==3)
tiempo_3_digitos <- Vuelos_Directos$DepTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")

tiempo_2_digitos_indices <- which(nchar(Vuelos_Directos$DepTime)==2)
tiempo_2_digitos <- Vuelos_Directos$DepTime[tiempo_2_digitos_indices]
Horas_2_Digitos <- "00"
Minutos_2_Digitos <- sapply(tiempo_2_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
df_Tiempo_2_Digitos <- data.frame(Horas_2_Digitos, Minutos_2_Digitos)
df_Tiempo_2_Digitos <- unite(df_Tiempo_2_Digitos,"Hora", Horas_2_Digitos, Minutos_2_Digitos, sep = ":")

tiempo_1_digitos_indices <- which(nchar(Vuelos_Directos$DepTime)==1)
tiempo_1_digitos <- Vuelos_Directos$DepTime[tiempo_1_digitos_indices]
Horas_1_Digitos <- "00"
Minutos_1_Digitos <- paste0("0",tiempo_1_digitos)
df_Tiempo_1_Digitos <- data.frame(Horas_1_Digitos, Minutos_1_Digitos)
df_Tiempo_1_Digitos <- unite(df_Tiempo_1_Digitos,"Hora", Horas_1_Digitos, Minutos_1_Digitos, sep = ":")

Vuelos_Directos$DepTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
Vuelos_Directos$DepTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora
Vuelos_Directos$DepTime[tiempo_2_digitos_indices] <- df_Tiempo_2_Digitos$Hora
Vuelos_Directos$DepTime[tiempo_1_digitos_indices] <- df_Tiempo_1_Digitos$Hora

########################################################################################################
Vuelos_Directos$CRSDepTime <- as.character(Vuelos_Directos$CRSDepTime)

tiempo_4_digitos_indices <- which(nchar(Vuelos_Directos$CRSDepTime)==4)
tiempo_4_digitos <- Vuelos_Directos$CRSDepTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(Vuelos_Directos$CRSDepTime)==3)
tiempo_3_digitos <- Vuelos_Directos$CRSDepTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")


tiempo_2_digitos_indices <- which(nchar(Vuelos_Directos$CRSDepTime)==2)
tiempo_2_digitos <- Vuelos_Directos$CRSDepTime[tiempo_2_digitos_indices]
Horas_2_Digitos <- "00"
Minutos_2_Digitos <- sapply(tiempo_2_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
df_Tiempo_2_Digitos <- data.frame(Horas_2_Digitos, Minutos_2_Digitos)
df_Tiempo_2_Digitos <- unite(df_Tiempo_2_Digitos,"Hora", Horas_2_Digitos, Minutos_2_Digitos, sep = ":")

tiempo_1_digitos_indices <- which(nchar(Vuelos_Directos$CRSDepTime)==1)
tiempo_1_digitos <- Vuelos_Directos$CRSDepTime[tiempo_1_digitos_indices]
Horas_1_Digitos <- "00"
Minutos_1_Digitos <- paste0("0",tiempo_1_digitos)
df_Tiempo_1_Digitos <- data.frame(Horas_1_Digitos, Minutos_1_Digitos)
df_Tiempo_1_Digitos <- unite(df_Tiempo_1_Digitos,"Hora", Horas_1_Digitos, Minutos_1_Digitos, sep = ":")

Vuelos_Directos$CRSDepTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
Vuelos_Directos$CRSDepTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora
Vuelos_Directos$CRSDepTime[tiempo_2_digitos_indices] <- df_Tiempo_2_Digitos$Hora
Vuelos_Directos$CRSDepTime[tiempo_1_digitos_indices] <- df_Tiempo_1_Digitos$Hora

#######################################################################################################
Vuelos_Directos$ArrTime <- as.character(Vuelos_Directos$ArrTime)

tiempo_4_digitos_indices <- which(nchar(Vuelos_Directos$ArrTime)==4)
tiempo_4_digitos <- Vuelos_Directos$ArrTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(Vuelos_Directos$ArrTime)==3)
tiempo_3_digitos <- Vuelos_Directos$ArrTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")

tiempo_2_digitos_indices <- which(nchar(Vuelos_Directos$ArrTime)==2)
tiempo_2_digitos <- Vuelos_Directos$ArrTime[tiempo_2_digitos_indices]
Horas_2_Digitos <- "00"
Minutos_2_Digitos <- sapply(tiempo_2_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
df_Tiempo_2_Digitos <- data.frame(Horas_2_Digitos, Minutos_2_Digitos)
df_Tiempo_2_Digitos <- unite(df_Tiempo_2_Digitos,"Hora", Horas_2_Digitos, Minutos_2_Digitos, sep = ":")

tiempo_1_digitos_indices <- which(nchar(Vuelos_Directos$ArrTime)==1)
tiempo_1_digitos <- Vuelos_Directos$ArrTime[tiempo_1_digitos_indices]
Horas_1_Digitos <- "00"
Minutos_1_Digitos <- paste0("0",tiempo_1_digitos)
df_Tiempo_1_Digitos <- data.frame(Horas_1_Digitos, Minutos_1_Digitos)
df_Tiempo_1_Digitos <- unite(df_Tiempo_1_Digitos,"Hora", Horas_1_Digitos, Minutos_1_Digitos, sep = ":")

Vuelos_Directos$ArrTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
Vuelos_Directos$ArrTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora
Vuelos_Directos$ArrTime[tiempo_2_digitos_indices] <- df_Tiempo_2_Digitos$Hora
Vuelos_Directos$ArrTime[tiempo_1_digitos_indices] <- df_Tiempo_1_Digitos$Hora

#######################################################################################################

Vuelos_Directos$CRSArrTime <- as.character(Vuelos_Directos$CRSArrTime)

tiempo_4_digitos_indices <- which(nchar(Vuelos_Directos$CRSArrTime)==4)
tiempo_4_digitos <- Vuelos_Directos$CRSArrTime[tiempo_4_digitos_indices]
Horas_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
Minutos_4_Digitos <- sapply(tiempo_4_digitos, function(x) substring(x, 3,4), USE.NAMES = FALSE)
df_Tiempo_4_Digitos <- data.frame(Horas_4_Digitos, Minutos_4_Digitos)
df_Tiempo <- unite(df_Tiempo_4_Digitos,"Hora", Horas_4_Digitos, Minutos_4_Digitos, sep = ":")

tiempo_3_digitos_indices <- which(nchar(Vuelos_Directos$CRSArrTime)==3)
tiempo_3_digitos <- Vuelos_Directos$CRSArrTime[tiempo_3_digitos_indices]
Horas_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 1,1), USE.NAMES = FALSE)
Minutos_3_Digitos <- sapply(tiempo_3_digitos, function(x) substring(x, 2,3), USE.NAMES = FALSE)
df_Tiempo_3_Digitos <- data.frame(Horas_3_Digitos, Minutos_3_Digitos)
df_Tiempo_3_Digitos <- unite(df_Tiempo_3_Digitos,"Hora", Horas_3_Digitos, Minutos_3_Digitos, sep = ":")


tiempo_2_digitos_indices <- which(nchar(Vuelos_Directos$CRSArrTime)==2)
tiempo_2_digitos <- Vuelos_Directos$CRSArrTime[tiempo_2_digitos_indices]
Horas_2_Digitos <- "00"
Minutos_2_Digitos <- sapply(tiempo_2_digitos, function(x) substring(x, 1,2), USE.NAMES = FALSE)
df_Tiempo_2_Digitos <- data.frame(Horas_2_Digitos, Minutos_2_Digitos)
df_Tiempo_2_Digitos <- unite(df_Tiempo_2_Digitos,"Hora", Horas_2_Digitos, Minutos_2_Digitos, sep = ":")

tiempo_1_digitos_indices <- which(nchar(Vuelos_Directos$CRSArrTime)==1)
tiempo_1_digitos <- Vuelos_Directos$CRSArrTime[tiempo_1_digitos_indices]
Horas_1_Digitos <- "00"
Minutos_1_Digitos <- paste0("0",tiempo_1_digitos)
df_Tiempo_1_Digitos <- data.frame(Horas_1_Digitos, Minutos_1_Digitos)
df_Tiempo_1_Digitos <- unite(df_Tiempo_1_Digitos,"Hora", Horas_1_Digitos, Minutos_1_Digitos, sep = ":")

Vuelos_Directos$CRSArrTime[tiempo_4_digitos_indices] <- df_Tiempo$Hora
Vuelos_Directos$CRSArrTime[tiempo_3_digitos_indices] <- df_Tiempo_3_Digitos$Hora
Vuelos_Directos$CRSArrTime[tiempo_2_digitos_indices] <- df_Tiempo_2_Digitos$Hora
Vuelos_Directos$CRSArrTime[tiempo_1_digitos_indices] <- df_Tiempo_1_Digitos$Hora


#A continuacion pasamos a tipo character el mes, día.

Vuelos_Directos$Month <- as.character(Vuelos_Directos$Month)
Vuelos_Directos$Month <- paste0("0", Vuelos_Directos$Month)
Vuelos_Directos$DayofMonth <- as.character(Vuelos_Directos$DayofMonth)
Vuelos_Directos$DayofMonth <- ifelse(nchar(Vuelos_Directos$DayofMonth)==1, paste0("0", Vuelos_Directos$DayofMonth), Vuelos_Directos$DayofMonth)
Vuelos_Directos$DayOfWeek <- paste0("0", Vuelos_Directos$DayOfWeek)

#Añadimos los segundos puesto que no nos dicen los pondremos a 00 para poder pasar a formato fecha de 
#forma adecuada.

Vuelos_Directos$DepTime <- paste0(Vuelos_Directos$DepTime, ":00")
Vuelos_Directos$CRSDepTime <- paste0(Vuelos_Directos$CRSDepTime, ":00")
Vuelos_Directos$ArrTime <- paste0(Vuelos_Directos$ArrTime, ":00")
Vuelos_Directos$CRSArrTime <- paste0(Vuelos_Directos$CRSArrTime, ":00")


#Nos creamos columnas de tipo fecha
Vuelos_Directos <- unite(Vuelos_Directos, Fecha, Anyo, Month, DayofMonth, sep = "-")
a <- data.frame( Fecha = Vuelos_Directos$Fecha, stringsAsFactors = FALSE)
Vuelos_Directos <- unite(Vuelos_Directos, FechaDepTime ,Fecha, DepTime, sep = " ")
Vuelos_Directos <- cbind(Vuelos_Directos, a)
Vuelos_Directos <- unite(Vuelos_Directos, FechaCRSDepTime ,Fecha, CRSDepTime, sep = " ")
Vuelos_Directos <- cbind(Vuelos_Directos, a)
Vuelos_Directos <- unite(Vuelos_Directos, FechaArrTime ,Fecha, ArrTime, sep = " ")
Vuelos_Directos <- cbind(Vuelos_Directos, a)
Vuelos_Directos <- unite(Vuelos_Directos, FechaCRSArrTime ,Fecha, CRSArrTime, sep = " ")


#Pasamos las fechas a tipo Date
Vuelos_Directos$FechaDepTime <- as.POSIXct(Vuelos_Directos$FechaDepTime, format = "%Y-%m-%d %T")
Vuelos_Directos$FechaCRSDepTime <- as.POSIXct(Vuelos_Directos$FechaCRSDepTime, format = "%Y-%m-%d %T")
Vuelos_Directos$FechaArrTime <- as.POSIXct(Vuelos_Directos$FechaArrTime, format = "%Y-%m-%d %T")
Vuelos_Directos$FechaCRSArrTime <- as.POSIXct(Vuelos_Directos$FechaCRSArrTime, format = "%Y-%m-%d %T")

#Ordenamos las fechas de Llegada

a <- FechaArrTime[order(FechaArrTime)]
plot.ts(a, Vuelos_Directos$CarrierDelay)


ggplot(WN, aes(FechaDepTime, WeatherDelay)) + geom_line() +
   xlab("") + ylab("Daily Views")


#Calculamos el retardo total por día de la semana del año 2008

Vuelos_Directos$RetardoTotal <- rowSums(Vuelos_Directos[,23:27])
Dia_1 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "01")
Dia_2 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "02")
Dia_3 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "03")
Dia_4 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "04")
Dia_5 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "05")
Dia_6 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "06")
Dia_7 <- filter(Vuelos_Directos, Vuelos_Directos$DayOfWeek == "07")

Dia_1_RetardoTotal <- sum(Dia_1$RetardoTotal)
Dia_2_RetardoTotal <- sum(Dia_2$RetardoTotal)
Dia_3_RetardoTotal <- sum(Dia_3$RetardoTotal)
Dia_4_RetardoTotal <- sum(Dia_4$RetardoTotal)
Dia_5_RetardoTotal <- sum(Dia_5$RetardoTotal)
Dia_6_RetardoTotal <- sum(Dia_6$RetardoTotal)
Dia_7_RetardoTotal <- sum(Dia_7$RetardoTotal)


Dia_Semana <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")
Retardos <- c(Dia_1_RetardoTotal,Dia_2_RetardoTotal,Dia_3_RetardoTotal, Dia_4_RetardoTotal,Dia_5_RetardoTotal,
              Dia_6_RetardoTotal, Dia_7_RetardoTotal)
names(Retardos) <- Dia_Semana

barplot(Retardos, main = "Retardo vs Día Semana")

Vuelos_Directos$Horas <- hour(Vuelos_Directos$FechaDepTime)

Vuelos_Directos$Rango <- ifelse(Vuelos_Directos$Horas < 4, 1, Vuelos_Directos$Horas)
Vuelos_Directos$Rango <- ifelse(Vuelos_Directos$Rango >3 & Vuelos_Directos$Rango < 8, 2, Vuelos_Directos$Rango)
Vuelos_Directos$Rango <- ifelse(Vuelos_Directos$Rango > 7 & Vuelos_Directos$Rango < 12, 3, Vuelos_Directos$Rango)
Vuelos_Directos$Rango <- ifelse(Vuelos_Directos$Rango > 11 & Vuelos_Directos$Rango < 16, 4, Vuelos_Directos$Rango)
Vuelos_Directos$Rango <- ifelse(Vuelos_Directos$Rango > 15 & Vuelos_Directos$Rango < 20, 5, Vuelos_Directos$Rango)
Vuelos_Directos$Rango <- ifelse(Vuelos_Directos$Rango > 19 , 6, Vuelos_Directos$Rango)

tbl <- table(Vuelos_Directos$Rango, Vuelos_Directos$Uniquecarrier)
IS_COLORS <- c ("#000000", "#00CC00", "#CC0000", "#FFFF00","#00FF66", "#336666")
barplot(tbl, col = IS_COLORS, main = "Retardos vs Compañía")
legend("topleft", legend = c("[00-03]", "[04-07]", "[08-11]", "[12-15]", "[16-19]", "[20-23]"),
       fill = IS_COLORS)

#Calculamos y representamos mediante un barplot el porcentaje de vuelos directos de cada una de las 
# de las aerolíneas por día
IS_COLORS <- c ("#000000", "#00CC00", "#CC0000", "#FFFF00", "#00FF66", "#336666", "#660066")

Vuelos_Directos$Company <- as.character(Vuelos_Directos$Company)
proporcion_vuelos_company <- prop.table(table(Vuelos_Directos$Company))*100
barplot(proporcion_vuelos_company, las = 2, cex.names = 0.5)

tbl <- table(Vuelos_Directos$DayOfWeek,Vuelos_Directos$Uniquecarrier)
barplot(tbl, col = IS_COLORS, main = "Vuelos Directos vs Compañía")
legend("topleft", legend = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
       fill = IS_COLORS)








