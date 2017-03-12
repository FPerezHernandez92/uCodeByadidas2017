## ----------------------------- ##
##       VUELOS CON ESCALAS      ##
## ----------------------------- ##

# Limpiamos el espacio de trabajo y cargamos los dataset relevantes
rm(list = ls())
dt.imputados <- read.csv(file = "DataSetImputado.csv")
dt.cambiados <- read.csv(file = "VuelosCambiadosDeAeropuerto.csv") 
dt.cambiados$OtherDelay = 0
dt.cambiados$nueva.variable.cambiados = NULL

# Pasamos a character algunas variables ya que con factor no se pueden comparar al tener más niveles algunos que otros
dt.imputados$Dest <- as.character(dt.imputados$Dest)
dt.imputados$Origin <- as.character(dt.imputados$Origin)
dt.cambiados$Dest <- as.character(dt.cambiados$Dest)
dt.cambiados$Origin <- as.character(dt.cambiados$Origin)
dt.imputados$TailNum <- as.character(dt.imputados$TailNum)
dt.cambiados$TailNum <- as.character(dt.cambiados$TailNum)

# Vamos a sacar el minuto en el que se hace el viaje para ordenar los viajes y sacar el anterior a alguno con escala
TiempoTotal <- apply(dt.cambiados,1,function(x) { 
  resultado = 0
  if (x[[1]]==2)
    resultado = 31*24*60
  resultado = resultado + ((as.numeric(x[[2]]))*24*60)
  auxiliar.deptime = as.numeric(x[[4]])
  minutos <- auxiliar.deptime %% 100
  horas <- auxiliar.deptime / 100
  horas <- round(horas,0)
  auxiliar.deptime = (horas*60)+minutos
  resultado = resultado + auxiliar.deptime
})
dt.cambiados <- cbind(dt.cambiados,TiempoTotal)

TiempoTotal <- apply(dt.imputados,1,function(x) { 
  resultado = 0
  if (x[[1]]==2)
    resultado = 31*24*60
  resultado = resultado + ((as.numeric(x[[2]]))*24*60)
  auxiliar.deptime = as.numeric(x[[4]])
  minutos <- auxiliar.deptime %% 100
  horas <- auxiliar.deptime / 100
  horas <- round(horas,0)
  auxiliar.deptime = (horas*60)+minutos
  resultado = resultado + auxiliar.deptime
})
dt.imputados <- cbind(dt.imputados,TiempoTotal)

# Vamos a buscar para cada vuelo cambiado, si tiene uno anterior o si no tienen ninguno anterior
final <- NULL;salida <- NULL;i=1; final.no.encontrados=NULL
for (i in 1:dim(dt.cambiados)[1]){
  dt.filtrado <- dt.imputados[dt.imputados[,11] == dt.cambiados[i,11] ,]
  dt.filtrado.menor <- dt.filtrado[dt.filtrado[,31] < dt.cambiados[i,31] ,]
  if ((dim(dt.filtrado.menor)[1]) < 1){
    print("no hay ninguno menor")
    print(dt.cambiados[i,c(1,2,3,4,5,31)])
    final.no.encontrados <- rbind(final.no.encontrados,dt.cambiados[i,])
  } else {
    elemento.anterior <- dt.filtrado.menor[dt.filtrado.menor[[31]] == max(dt.filtrado.menor[[31]]),]
    final <- rbind(final,dt.cambiados[i,],elemento.anterior)
  }
}
 
# Escribimos los datasets 
write.csv(final.no.encontrados, file = "EscalasSinVueloAnterior.csv", row.names=FALSE)
write.csv(final, file = "EscalasConVueloAnterior.csv", row.names=FALSE)

# Limpiamos el espacio de trabajo y cargamos el dataset al que vamos a unificar
rm(list = ls())
dt.escala.anterior <- read.csv(file = "EscalasConVueloAnterior.csv")
dt.escala.anterior$Dest = as.character(dt.escala.anterior$Dest)
dt.escala.anterior$Origin = as.character(dt.escala.anterior$Origin)

# Separamos los vuelos que son escala de los que empiezan y acaban en el mismo aeropuerto
dt.vuelta.mismo =NULL; dt.escalas =NULL
recorrido <- seq(1,dim(dt.escala.anterior)[1],by=2)
for (i in recorrido){
  if(dt.escala.anterior[i,"Dest"]==dt.escala.anterior[(i+1),"Origin"]){
    dt.vuelta.mismo <- rbind(dt.vuelta.mismo,dt.escala.anterior[i,],dt.escala.anterior[(i+1),])
  } else {
    dt.escalas <- rbind(dt.escalas,dt.escala.anterior[i,],dt.escala.anterior[(i+1),])
  }
}

# Escribimos los dataset diferenciados
write.csv(dt.escalas, file = "EscalasSinUnir.csv", row.names=FALSE)
write.csv(dt.vuelta.mismo, file = "VuelosQueEmpiezanYAcabanEnElMismoAeropuerto.csv", row.names=FALSE)

# Limpiamos el espacio de trabajo y cargamos el dataset al que vamos a unificar
rm(list = ls())
dt.escalas <- read.csv(file = "EscalasSinUnir.csv")
dt.vuelta.mismo <- read.csv(file = "VuelosQueEmpiezanYAcabanEnElMismoAeropuerto.csv")

# En esta función seleccionaremos únicamente los parámetros adecuados para hacer una instancia de escala
transformar.escalas <- function(dataset){
  dataset$Uniquecarrier <- as.character(dataset$Uniquecarrier)
  dataset$Company <- as.character(dataset$Company)
  dataset$TailNum <- as.character(dataset$TailNum)
  dataset$Origin <- as.character(dataset$Origin)
  dataset$Dest <- as.character(dataset$Dest)
  
  dt.escalado <- NULL; 
  recorrido <- seq(3,dim(dataset)[1],by=2)
  i=1
  instancia.aux1 <- (c(dataset[(i+1),"Month"],dataset[(i+1),"DayofMonth"],dataset[(i+1),"DayOfWeek"],dataset[(i+1),"DepTime"],
                       dataset[(i+1),"CRSDepTime"],dataset[(i+1),"ArrTime"],dataset[(i+1),"CRSArrTime"],
                       dataset[(i),"Month"],dataset[(i),"DayofMonth"],dataset[(i),"DayOfWeek"],dataset[(i),"DepTime"],
                       dataset[(i),"CRSDepTime"],dataset[(i),"CRSArrTime"],
                       dataset[i,"Uniquecarrier"],dataset[i,"Company"],dataset[i,"FlightNum"],dataset[i,"TailNum"],
                       dataset[(i+1),"ActualElapsedTime"],dataset[(i+1),"CRSElapsedTime"],dataset[(i+1),"AirTime"],
                       dataset[(i+1),"ArrDelay"],dataset[(i+1),"DepDelay"],dataset[i,"DepDelay"],
                       dataset[(i+1),"Origin"],dataset[(i),"Origin"],dataset[i,"Dest"],dataset[(i+1),"Distance"],dataset[i,"Distance"],
                       dataset[(i+1),"TaxiIn"],dataset[(i+1),"TaxiOut"],dataset[(i),"TaxiOut"],
                       dataset[(i+1),"CarrierDelay"],dataset[(i+1),"WeatherDelay"],dataset[(i+1),"NASDelay"],dataset[(i+1),"SecurityDelay"],
                       dataset[(i+1),"LateAircraftDelay"],dataset[(i+1),"OtherDelay"]))
  final <- (instancia.aux1)
  for (i in recorrido){
    instancia.aux2 <- (c(dataset[(i+1),"Month"],dataset[(i+1),"DayofMonth"],dataset[(i+1),"DayOfWeek"],dataset[(i+1),"DepTime"],
                         dataset[(i+1),"CRSDepTime"],dataset[(i+1),"ArrTime"],dataset[(i+1),"CRSArrTime"],
                         dataset[(i),"Month"],dataset[(i),"DayofMonth"],dataset[(i),"DayOfWeek"],dataset[(i),"DepTime"],
                         dataset[(i),"CRSDepTime"],dataset[(i),"CRSArrTime"],
                         dataset[i,"Uniquecarrier"],dataset[i,"Company"],dataset[i,"FlightNum"],dataset[i,"TailNum"],
                         dataset[(i+1),"ActualElapsedTime"],dataset[(i+1),"CRSElapsedTime"],dataset[(i+1),"AirTime"],
                         dataset[(i+1),"ArrDelay"],dataset[(i+1),"DepDelay"],dataset[i,"DepDelay"],
                         dataset[(i+1),"Origin"],dataset[(i),"Origin"],dataset[i,"Dest"],dataset[(i+1),"Distance"],dataset[i,"Distance"],
                         dataset[(i+1),"TaxiIn"],dataset[(i+1),"TaxiOut"],dataset[(i),"TaxiOut"],
                         dataset[(i+1),"CarrierDelay"],dataset[(i+1),"WeatherDelay"],dataset[(i+1),"NASDelay"],dataset[(i+1),"SecurityDelay"],
                         dataset[(i+1),"LateAircraftDelay"],dataset[(i+1),"OtherDelay"]))
    final <- rbind(final,instancia.aux2)
  }
  final <- as.data.frame(final)
  colnames(final) <- c("MonthA","DayofMonthA","DayOfWeekA","DepTimeA",
                       "CRSDepTimeA","ArrTimeB","CRSArrTimeB",
                       "MonthB","DayofMonthB","DayOfWeekB","DepTimeB",
                       "CRSDepTimeB","CRSArrTimeC",
                       "Uniquecarrier","Company","FlightNum","TailNum",
                       "ActualElapsedTimeA","CRSElapsedTimeA","AirTimeA",
                       "ArrDelayA","DepDelayA","DepDelayB",
                       "Origin","Scale","Dest","DistanceAB","DistanceBC",
                       "TaxiInA","TaxiOutB","TaxiOutC",
                       "CarrierDelayA","WeatherDelayA","NasDelayA","SecurityDelayA","LateAircraftDelayA","OtherDelayA")
  rownames(final) <- NULL
  return(final)
}

# Aplicamos la función a los dataset
dataset.escalas <- transformar.escalas(dt.escalas)
dataset.vuelta.mismo <- transformar.escalas(dt.vuelta.mismo)
  
# Guardamos los datasets
write.csv(dataset.escalas, file = "FinalVuelosConEscalas.csv", row.names=FALSE)
write.csv(dataset.vuelta.mismo, file = "FinalVuelosRegreso.csv", row.names=FALSE)

  
  
  
  
  
  
  
  
  
  
  
