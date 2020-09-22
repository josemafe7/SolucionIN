##PAQUETES NECESARIOS
install.packages("reshape")
install.packages("devtools")
install.packages("Rtools")
devtools::install_github ("dkahle/ggmap")
devtools::install_github ("hadley/ggplot2")
install.packages("dplyr")
install.packages("ISLR")
install.packages("cluster")
install.packages("Rtsne")
install.packages("ggplot2")

##IMPORTAMOS LIBRERIAS
library(dplyr)
library(ISLR)
library(cluster) 
library(Rtsne) 
library(ggplot2) 
library(ggmap)


## CLAVE DE LA API DE GOOGLE, NECESARIA PARA MOSTRAR MAPAS
register_google(key="AIzaSyD-PCG3JFFUNux2K6WBLLPEv-_HsuWtvdc")

## CARGA DEL DATASET
data <- read.table("C:/Users/Manuel/Google Drive/Universidad/Tercero/2º Cuatrimestre/Inteligencia Negocio/ProyectoFinal/Resultados/datasetPreprocesado.csv",sep=",",header=TRUE)


#############################
##### VISUALIZAMOS MAPA #####
#############################

# BUSQUEDA DEL MAPA QUE QUEREMOS MOSTRAR
map <- get_map(location = "tanzania", zoom = 6, source = "google", maptype = "terrain")


# MOSTRAR MAPA -- CANTIDAD DE CADA BOMBA DE AGUA
ggmap(map, extent  = TRUE) +  geom_point(aes(longitud, latitud, colour=cantidad), alpha=0.3, size=5, shape=19, data=data)

# MOSTRAR MAPA -- REGIÓN DE CADA BOMBA DE AGUA
ggmap(map, extent  = TRUE) +  geom_point(aes(longitud, latitud, colour=region), alpha=0.3, size=5, shape=19, data=data)

# MOSTRAR MAPA -- CUENCA DE CADA BOMBA DE AGUA
ggmap(map, extent = TRUE) +  geom_point(aes(longitud, latitud, colour=cuenca), alpha=0.3, size=5, shape=19, data=data)

# MOSTRAR MAPA --  CALIDAD AGUA DE CADA BOMBA DE AGUA
ggmap(map, extent = TRUE) +  geom_point(aes(longitud, latitud, colour=calidad_agua), alpha=0.3, size=5, shape=19, data=data)




######################
##### CLUSTERING #####
######################

set.seed(1680)


# ========================================================
# ===== CALCULAMOS DISTANCIAS CON EL METODO DE GOWER =====
# ========================================================

distancias <- daisy(data,metric = "gower",type = list(logratio = 3))

summary(distancias)

# MATRIZ DE DISTANCIAS
mat_distancias <- as.matrix(distancias)


# IMPREMIMOS EL PAR DE INSTANCIAS MAS CERCANAS Y EL MAS LEJANO
# INSTANCIAS MÁS CERCANAS
data[ which(mat_distancias == min(mat_distancias[mat_distancias != min(mat_distancias)]),arr.ind = TRUE)[1, ], ]
# INSTANCIAS MÁS LEJANAS
data[which(mat_distancias == max(mat_distancias[mat_distancias != max(mat_distancias)]), arr.ind = TRUE)[1, ], ]

# ===================================================================================
# ===== COMPROBAR EL NUMERO OPTIMO DE CLUSTERES -- METRICA DEL ANCHO DE SILUETA =====
# ===================================================================================

sil_width <- c(NA)

for(i in 2:5){
  
  pam_fit <- pam(distancias, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}


# PINTAMOS GRAFICA PARA COMPOBAR EL NUMERO OPTIMO DE CLUSTERES
plot(1:5, sil_width, xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:5, sil_width)

# ============================================================================
# ===== EJECUTAMOS EL ALGORITMO PAM, CON EL NUMERO DE CLUSTERES OBTENIDO =====
# ============================================================================

pam_fit <- pam(distancias, diss = TRUE, k = 3)

pam_results <- data %>%
  dplyr::select(-tipo_PuntoAgua) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

# INSERTAMOS NUEVA COLUMNA CLUSTER , PARA SABER A QUE CLUSTER PERTENECE CADA INSTANCIA
data$cluster <-pam_fit$clustering

# VISUALIZACIÓN DE LOS DATOS
tsne_obj <- Rtsne(distancias, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = data$tipo_PuntoAgua)

ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))


# =============================================================
# ===== MOSTRAMOS MAPA -- DISTRIBUCION CLUSTERES OBTENIDO =====
# =============================================================

ggmap(map, extent = TRUE) +  geom_point(aes(longitud, latitud, colour=cluster), alpha=1, size=5, shape=19, data=data)



