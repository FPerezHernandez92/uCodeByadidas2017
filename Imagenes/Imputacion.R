## ----------------------------- ##
##     IMPUTACIÓN DE VALORES     ##
## ----------------------------- ##

# Tomamos el dataset inicial
aerolineas2008 <- read.csv("~/Downloads/Datasets/Aerolineas/2008.csv", header=FALSE)
# Añadimos el nombre de cada variable
colnames(aerolineas2008) <- c("Year", "Month", "DayofMonth", "DayOfWeek", "DepTime", 
                              "CRSDepTime", "ArrTime", "CRSArrTime", "UniqueCarrier", "FlightNum",
                              "TailNum", "ActualElapsedTime", "CRSElapsedTime", "AirTime", "ArrDelay",
                              "DepDelay", "Origin", "Dest", "Distance", "TaxiIn",
                              "TaxiOut", "Cancelled", "CancellationCode", "Diverted", "CarrierDelay",
                              "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")
write.csv(aerolineas2008[1:20,],file="Aerolineas_Parseado.csv", row.names=FALSE)

# Vemos el dataset
str(aerolineas2008)
# Leemos el dataset complementario de operadoras
carriers <- read.csv("Datasets/carriers.csv")

# Sacamos el porcentaje de valores perdidos por columna
porcentaje.de.valores.perdidos.por.columna.train <- apply(aerolineas2008,2,function(x) sum(is.na(x))) / nrow(aerolineas2008) * 100
porcentaje.de.valores.perdidos.por.columna.train

# Usando la librería dplyr podemos realizar filtros rápidamente
library(dplyr)
attach(aerolineas2008)
unique(UniqueCarrier)
# Eliminamos la variable Year ya que siempre su valor es 2008
aerolineas2008$Year = NULL
OP9E <-filter(aerolineas2008,UniqueCarrier=="9E")
OPAA <-filter(aerolineas2008,UniqueCarrier=="AA")
OPAQ <-filter(aerolineas2008,UniqueCarrier=="AQ")
OPAS <-filter(aerolineas2008,UniqueCarrier=="AS")
OPB6 <-filter(aerolineas2008,UniqueCarrier=="B6")
OPCO <-filter(aerolineas2008,UniqueCarrier=="CO")
OPDL <-filter(aerolineas2008,UniqueCarrier=="DL")
OPEV <-filter(aerolineas2008,UniqueCarrier=="EV")
OPF9 <-filter(aerolineas2008,UniqueCarrier=="F9")
OPFL <-filter(aerolineas2008,UniqueCarrier=="FL")
OPHA <-filter(aerolineas2008,UniqueCarrier=="HA")
OPMQ <-filter(aerolineas2008,UniqueCarrier=="MQ")
OPNW <-filter(aerolineas2008,UniqueCarrier=="NW")
OPOH <-filter(aerolineas2008,UniqueCarrier=="OH")
OPOO <-filter(aerolineas2008,UniqueCarrier=="OO")
OPUA <-filter(aerolineas2008,UniqueCarrier=="UA")
OPUS <-filter(aerolineas2008,UniqueCarrier=="US")
OPWN <-filter(aerolineas2008,UniqueCarrier=="WN")
OPXE <-filter(aerolineas2008,UniqueCarrier=="XE")
OPYV <-filter(aerolineas2008,UniqueCarrier=="YV")

# Obtenemos la información más relevante para obtener más conocimiento
OP9E.truncado <- OP9E[1:7,c("DepTime", 
                         "CRSDepTime", "ArrTime", "CRSArrTime", 
                          "ActualElapsedTime", "CRSElapsedTime", "AirTime", "ArrDelay",
                         "DepDelay","CarrierDelay",
                         "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")]

# Imputamos valores de la variable DEPTIME
valores.DEPTIME.na <- sapply(aerolineas2008$DepTime,function(x) is.na(x))
filas.DEPTIME.na <- aerolineas2008[valores.DEPTIME.na,]
donde.hay.valores.perdidos.DEPTIME <- apply(filas.DEPTIME.na,2,function(x) sum(is.na(x))) / nrow(filas.DEPTIME.na) * 100
donde.hay.valores.perdidos.DEPTIME
filas.no.DEPTIME.na <- aerolineas2008[!valores.DEPTIME.na,]

# Vemos como esta variable tiene valor na ya que son vuelos que han sido cancelados. Limipiamos vuelos cancelados
write.csv(filas.DEPTIME.na,file="VuelosCancelados.csv", row.names=FALSE)
write.csv(filas.no.DEPTIME.na,file="NoCancelados.csv", row.names=FALSE)
aerolineas2 <- aerolineas2008[!valores.DEPTIME.na,]
donde.hay.valores.perdidos.aerolineas2 <- apply(aerolineas2,2,function(x) sum(is.na(x))) / nrow(aerolineas2) * 100
donde.hay.valores.perdidos.aerolineas2

# Imputación de la variable ARRTIME
valores.ARRTIME.na <- sapply(aerolineas2$ArrTime,function(x) is.na(x))
filas.ARRTIME.na <- aerolineas2[valores.ARRTIME.na,]
filas.no.ARRTIME.na <- aerolineas2[!valores.ARRTIME.na,]
donde.hay.valores.perdidos.no.ARRTIME.na <- apply(filas.no.ARRTIME.na,2,function(x) sum(is.na(x))) / nrow(filas.no.ARRTIME.na) * 100
donde.hay.valores.perdidos.no.ARRTIME.na

# Vemos que los valores na se refieren a vuelos que han sido cmabiados de aeropuerto. Limpiamos vuelos cambiados
write.csv(filas.ARRTIME.na,file="VuelosCambiadosDeAeropuerto.csv", row.names=FALSE)
write.csv(filas.no.ARRTIME.na,file="NoCambiadosNiCancelados.csv", row.names=FALSE)

# Imputación de las variables *Delay. Viendo que todas son NA, y que en las columnas en las que hay valores se corresponden a la columna ARRDELAY, vamos a crear una nueva variable que tome el valor de ARRDELAY y los valores na los transformaremos a 0
dataset.imputado <- filas.no.ARRTIME.na
dataset.imputado$OtherDelay <- ifelse(is.na(dataset.imputado$CarrierDelay),dataset.imputado$ArrDelay,0)
dataset.imputado$CarrierDelay <- ifelse(is.na(dataset.imputado$CarrierDelay),0,dataset.imputado$CarrierDelay)
dataset.imputado$WeatherDelay <- ifelse(is.na(dataset.imputado$WeatherDelay),0,dataset.imputado$WeatherDelay)
dataset.imputado$NASDelay <- ifelse(is.na(dataset.imputado$NASDelay),0,dataset.imputado$NASDelay)
dataset.imputado$SecurityDelay <- ifelse(is.na(dataset.imputado$SecurityDelay),0,dataset.imputado$SecurityDelay)
dataset.imputado$LateAircraftDelay <- ifelse(is.na(dataset.imputado$LateAircraftDelay),0,dataset.imputado$LateAircraftDelay)

# Guardamos el dataset con los datos imputados
write.csv(dataset.imputado,file="DataSetImputado.csv", row.names=FALSE)
