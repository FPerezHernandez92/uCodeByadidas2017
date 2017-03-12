############################################################################################
#######################                 uCode: Cancelados            #######################
############################################################################################

#---------------------------------------------------------------------------------------#
#----------------------             LECTURA DE DATOS                   -----------------#
#---------------------------------------------------------------------------------------#

setwd("~/Dropbox/uCode/")
#completo <- read.csv("Aerolineas_Parseado.csv")

# Para el ejemplo de vuelos en CARTO
# airport <- read.csv("~/Desktop/airports.csv")
# routes <- read.csv("~/Desktop/routes.csv")

data.cancelados <- read.csv("VuelosCancelados.csv")

# Eliminados las columnas que solo contienen NAs.
cancelados <- data.cancelados[,-c(4,6,11,12,14:16,20,21,24:29)] 
cancelados$Month <- ifelse(cancelados$Month==1, "Enero", "Febrero")

cancelados$DayOfWeek <- ifelse(cancelados$DayOfWeek==1, "Lunes", ifelse(cancelados$DayOfWeek==2, "Martes",ifelse(cancelados$DayOfWeek==3, "Miércoles",ifelse(cancelados$DayOfWeek==4, "Jueves",ifelse(cancelados$DayOfWeek==5, "Viernes",ifelse(cancelados$DayOfWeek==6, "Sábado","Domingo"))))))

attach(cancelados)

# Realizamos un análisis exploratorio de las variables que forman parte del data set.
barplot(table(Month), main = "Nº vuelos cancelados por meses")
prop.table(table(Month))

barplot(table(DayofMonth), main = "Nº vuelos cancelados por día de mes", las=2)
sort(prop.table(table(DayofMonth)), decreasing = TRUE)

barplot(table(DayOfWeek), main = "Nº vuelos cancelados por día de la semana", las=2)
sort(prop.table(table(DayOfWeek)), decreasing = TRUE)

barplot(table(Uniquecarrier), main = "Nº vuelos cancelados por Compañia", las=2)
head(sort(prop.table(summary(Company)), decreasing = TRUE))
head(sort(prop.table(summary(Company))))

barplot(table(Origin), main = "Nº vuelos cancelados por ciudad")
head(sort(prop.table(table(Origin)), decreasing = TRUE))
head(sort(prop.table(table(Origin))))

barplot(table(Distance), main = "Nº vuelos cancelados en base a la distancia")
head(sort(prop.table(table(Distance)), decreasing = TRUE))

barplot(table(CancellationCode), main = "Nº vuelos cancelados debido a una situación")
head(sort(prop.table(summary(CancellationCode)), decreasing = TRUE))

# Con este análisis extraemos una serie de conclusiones:
# 2- El Sábado es el día que menos cancelaciones se registran. Mientras que en Martes y Miércoles se 
# producen la mayoría de las cancelaciones.
# 3- Las tres compañías que tienen un mayor porcentaje de cancelaciones son: American Eagle Airlines Inc., 
# Skywest Airlines Inc. y Mesa Airlines Inc. Mientras que la compañía que apena realiza cancelaciones es
# Aloha Airlines Inc.
# 4- El aeropuerto que más cancelaciones realiza es el ORD - Chicago, IL, USA - O'hare International Airport. 
# 5- Por último, la razón por la que más se cancela los vuelos es por el clima.

IS_COLORS <- c ("#000000", "#00CC00", "#CC0000", "#FFFF00", "#00FF66", "#336666", "#660066")
cancelados$Company <- as.character(cancelados$Company)
proporcion_vuelos_company <- prop.table(table(cancelados$Company))*100
barplot(proporcion_vuelos_company, las = 2, cex.names = 0.5)

tbl <- table(cancelados$DayOfWeek,cancelados$Uniquecarrier)
barplot(tbl, col = IS_COLORS, main = "Vuelos Cancelados vs Compañia")
legend("topleft", legend = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
       fill = IS_COLORS)
