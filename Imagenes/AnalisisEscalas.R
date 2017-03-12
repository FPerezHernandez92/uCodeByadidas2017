## -------------------------------- ##
##    ANALISI VUELOS CON ESCALAS    ##
## -------------------------------- ##

# Limpiamos y cargamos los datos
rm(list = ls())
dt.escalas <- read.csv("FinalVuelosConEscalas.csv")
dt.regreso <- read.csv("FinalVuelosRegreso.csv")
dt.union <- rbind(dt.escalas,dt.regreso)
aerolineas2008 <- read.csv("~/Downloads/Datasets/Aerolineas/2008.csv", header=FALSE)
# Añadimos el nombre de cada variable
colnames(aerolineas2008) <- c("Year", "Month", "DayofMonth", "DayOfWeek", "DepTime", 
                              "CRSDepTime", "ArrTime", "CRSArrTime", "UniqueCarrier", "FlightNum",
                              "TailNum", "ActualElapsedTime", "CRSElapsedTime", "AirTime", "ArrDelay",
                              "DepDelay", "Origin", "Dest", "Distance", "TaxiIn",
                              "TaxiOut", "Cancelled", "CancellationCode", "Diverted", "CarrierDelay",
                              "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")

# Veamos que operadora es la que más escala realiza
library(ggplot2)
IS_COLORS <- c ("#000000", "#00CC00", "#CC0000", "#FFFF00", "#00FF66", "#336666", "#660066")
dt.union$Company <- as.character(dt.union$Company)
proporcion_vuelos_company <- prop.table(table(dt.union$Company))*100
barplot(proporcion_vuelos_company, las = 2, cex.names = 0.5)

tbl <- table(dt.union$DayOfWeekA,dt.union$Uniquecarrier)
barplot(tbl, col = IS_COLORS,main = "Vuelos con escala vs Compañia")
legend("topleft", legend = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
       fill = IS_COLORS)

