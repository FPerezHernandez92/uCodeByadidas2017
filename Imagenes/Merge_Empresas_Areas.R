#########################################      VUELOS CANCELADOS     ########################################
library(tibble)
path <- "C:/Users/fvilchez/Dropbox/uCode"
setwd(path)
files <- list.files(path)
Vuelos_Cancelados <- read.csv(files[15])
empresas <- read.csv(files[2])
names(empresas) <- c("UniqueCarrier", "Description")
Vuelos_Cancelados <- merge(Vuelos_Cancelados, empresas, by = "UniqueCarrier")
Vuelos_Cancelados <- add_column(Vuelos_Cancelados, Uniquecarrier = Vuelos_Cancelados$UniqueCarrier, .after = 8)
Vuelos_Cancelados$UniqueCarrier <- NULL
Vuelos_Cancelados <- add_column(Vuelos_Cancelados, Company = Vuelos_Cancelados$Description, .after = 8)
Vuelos_Cancelados$Description<- NULL

write.csv(Vuelos_Cancelados, file = "VuelosCancelados.csv", quote = FALSE, row.names = FALSE)




######################################## VUELOS CON ESCALA ################################################

Vuelos_Escala <- read.csv(files[13])
Vuelos_Escala <- merge(Vuelos_Escala, empresas, by = "UniqueCarrier")
Vuelos_Escala <- add_column(Vuelos_Escala, Uniquecarrier = Vuelos_Escala$UniqueCarrier, .after = 8)
Vuelos_Escala$UniqueCarrier <- NULL
Vuelos_Escala <- add_column(Vuelos_Escala, Company = Vuelos_Escala$Description, .after = 8)
Vuelos_Escala$Description<- NULL

write.csv(Vuelos_Escala, file = "VuelosCambiadosDeAeropuerto.csv", quote = FALSE, row.names = FALSE)



#######################################    VUELOS DIRECTOS     ##########################################

Vuelos_Directos <- read.csv(files[3])
Vuelos_Directos <- merge(Vuelos_Directos, empresas, by = "UniqueCarrier")
Vuelos_Directos <- add_column(Vuelos_Directos, Uniquecarrier = Vuelos_Directos$UniqueCarrier, .after = 8)
Vuelos_Directos$UniqueCarrier <- NULL
Vuelos_Directos <- add_column(Vuelos_Directos, Company = Vuelos_Directos$Description, .after = 8)
Vuelos_Directos$Description<- NULL

write.csv(Vuelos_Directos, file = "DataSetImputado.csv", quote = FALSE, row.names = FALSE)
