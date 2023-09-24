#Trabajo Práctico 2

#1. Preparación espacio de trabajo
#Para dar inicio al TP2, se instalan los packages y librerías necesarias para operar sobre los datos.

#Tidyverse
install.packages("tidyverse")
library(tidyverse)

#Dplyr
install.packages("dplyr")
library(dplyr)

#Ggplot
install.packages("ggplot2")
library(ggplot2)

#Esquisse (se utilizó para el armado de las bases de los gráficos de barras y puntos)
install.packages("esquisse")
esquisse::esquisser()

#2. Carga de datos
# En segunda instancia, se procede a importar la base de datos del Relevamiento Nacional de Barrios Populares 2022.

renabap_2022 <- read.csv ("datos/2022-07-13_info_publica.csv")

#3. Análisis preliminar de la base de datos
# Antes de comenzar a trabajar con la información provista por el .csv, se realiza un análisis exploratorio de los datos.

dim(renabap_2022)
# El .csv contiene 5687 filas y 77 columnas.

names(renabap_2022)
# Aquí se puede observar en mayor detalle el contenido de las 77 columnas. De manera preliminar, se definen como variables de interés: departamento, nombre_barrio, cantidad_viviendas_aproximadas, y cantidad_familias_aproximadas.


#4. Filtro y selección de datos
# A continuación, se procede a filtrar las filas correspondientes al AMBA, a saber, los 40 departamentos compuestos por Almirante Brown, Avellaneda, Berazategui, Berisso, Brandsen, Campana, Cañuelas, Ensenada, Escobar, Esteban Echeverría, Exaltación, Ezeiza, Florencio Varela, Gral. Las Heras, Gral. Rodríguez, Gral. San Martín, Hurlingham, Ituzaingó, José C. Paz, La Matanza, La Plata, Lanús, Lomas de Zamora, Luján, Malvinas Argentinas, Marcos Paz, Merlo, Moreno, Morón, Quilmes, Pilar, Presidente Perón, San Fernando, San Isidro, San Miguel, San Vicente, Tigre, Tres de Febrero, Vicente López, Zárate. Por otro lado, también se seleccionan las variables de interés previamente mencionadas.

departamentos_de_interes <- c("Almirante Brown", "Avellaneda", "Berazategui", "Berisso", "Brandsen", "Campana", "Cañuelas",
                              "Ensenada", "Escobar", "Esteban Echeverría", "Exaltación de la Cruz", "Ezeiza", "Florencio Varela",
                              "General Las Heras", "General Rodríguez", "General San Martín", "Hurlingham", "Ituzaingó",
                              "José C. Paz", "La Matanza", "La Plata", "Lanús", "Lomas de Zamora", "Luján",
                              "Malvinas Argentinas", "Marcos Paz", "Merlo", "Moreno", "Morón", "Quilmes", "Pilar",
                              "Presidente Perón", "San Fernando", "San Isidro", "San Miguel", "San Vicente", "Tigre",
                              "Tres de Febrero", "Vicente López", "Zárate", "Comuna 1", "Comuna 2", "Comuna 3", "Comuna 4", "Comuna 5",
                              "Comuna 6", "Comuna 7", "Comuna 8", "Comuna 9", "Comuna 10", "Comuna 11", "Comuna 12", "Comuna 13",
                              "Comuna 14", "Comuna 15")

columnas_de_interes <- c("departamento", "nombre_barrio", "cantidad_viviendas_aproximadas", 
                         "cantidad_familias_aproximada")

renabap_2022_amba <- renabap_2022 %>%
  filter(departamento %in% departamentos_de_interes)%>%
  select(all_of(columnas_de_interes))

summary(renabap_2022_amba)

# En el TP1, con estas primeras operaciones se pudo responder la primera pregunta (¿cuántos barrios populares se encuentran en el Área Metropolitana de Buenos Aires y, en promedio, cuántas viviendas y familias contienen?).

# Se observaron 1821 barrios populares existentes en el AMBA, que en promedio cuentan con 314 viviendas y 345 familias. 


#5. Agrupar y contar información
# Para poder proseguir con las siguientes preguntas y su visualización, se realizan las siguientes operaciones:

barrios_por_departamentos <- renabap_2022_amba %>%
  group_by(departamento) %>%
  summarise(cantidad_barrios = n_distinct(nombre_barrio)) %>%
  arrange(desc(cantidad_barrios))

top_10_departamentos <- head(barrios_por_departamentos, 10)

# En el siguiente gráfico de barras se puede visualizar la cantidad de barrios por departamento, y los 10 departamentos con mayor cantidad.

ggplot(barrios_por_departamentos) +
  aes(x = reorder(departamento, cantidad_barrios), y = cantidad_barrios) +
  geom_col(fill = ifelse(barrios_por_departamentos$departamento %in% top_10_departamentos$departamento, "#B22222", "grey")) +
  labs(
    x = "Departamentos",
    y = "Cantidad de barrios",
    title = "Cantidad de barrios por departamento",
    subtitle = "Fuente: RENABAP 2022"
  ) +
  geom_text(aes(label = cantidad_barrios), hjust = 1.5, vjust = 0.2, color = "white", size = 2, fontface = "bold") +  # Establecer el texto en negrita
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),  
        plot.subtitle = element_text(size = 10))

# En síntesis, se agrupan los datos por el valor de la columna "departamento". Luego, se cuenta la cantidad de barrios populares en cada uno. Finalmente, se ordenan los resultados de manera descendente, para así poder extraer el top 10 de departamentos con mayor cantidad de BP. 

# A partir de este análisis, se constata que los departamentos con mayor cantidad de BP son San Fernando, La Matanza, La Plata, Moreno, Almirante Brown, Merlo, Florencio Varela, Quilmes, Tigre y Lomas de Zamora.

# No obstante, si retomamos la información de cantidad de viviendas y familias, podremos observar que los departamentos con mayor cantidad de barrios no coinciden necesariamente con los que presentan mayor cantidad de viviendas:

viviendas_y_familias_por_departamento <- renabap_2022_amba %>%
  group_by(departamento) %>%
  summarise(cantidad_barrios = n_distinct(nombre_barrio),
            total_viviendas = sum(cantidad_viviendas_aproximadas),
            total_familias = sum(cantidad_familias_aproximada))

# En el siguiente gráfico se puede observar la relación entre departamentos, cantidad de barrios, cantidad de viviendas y cantidad de familias:

ggplot(viviendas_y_familias_por_departamento) +
  aes(
    x = cantidad_barrios,
    y = departamento,
    size = total_viviendas,
    color = total_familias
  ) +
  geom_point(shape = 21, aes(fill = total_familias), alpha = 0.8) +  
  scale_size_continuous(range = c(2, 10)) +  
  scale_color_distiller(palette = "OrRd", direction = 1) +  
  scale_fill_distiller(palette = "OrRd", direction = 1) +  
  labs(
    x = "Cantidad de barrios",
    y = "Departamentos",
    title = "Cantidad de barrios, viviendas y familias por departamento",
    subtitle = "Fuente: RENABAP 2022",
    size = "Cantidad de viviendas",
    color = "Cantidad de familias",
    fill = "Cantidad de familias"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        plot.subtitle = element_text(size = 10))

# En ambos casos se puede observar que La Matanza, Lomas de Zamora y Quilmes cuentan con la mayor cantidad de viviendas y familias. Resulta de interés que la Comuna 8 en CABA, si bien cuenta con una cantidad menor de BP (18), le sigue a los departamentos mencionados en cantidad de familias y viviendas.

# Aclaración: se ha probado reproducir esta información utilizando geofacet*. Luego de varios intentos, se llegó a la conclusión de que debido a la diversidad de tamaños y la intrincada distribución de los departamentos en el AMBA, la grilla espacial no ayuda a la visualización de la información, por lo cual se ha decidido descartar este método para representar los datos directamente en mapas coropléticos.

# *Aquí se puede visualizar la grilla fallida:

grilla_amba <- data.frame(
  name = c("Campana", "Zárate", "Escobar", "Exaltación de la Cruz", "Tigre", "San Fernando", "San Isidro", "Vicente López", "Pilar", "Malvinas Argentinas", "San Miguel", "Hurlingham", "General San Martín", "CABA", "Avellaneda", "Berisso", "Ensenada", "Berazategui", "Quilmes", "Luján", "General Rodríguez", "José C. Paz", "Ituzaingó", "Morón", "Tres de Febrero", "La Matanza", "Lanús", "Moreno", "Merlo", "Almirante Brown", "Florencio Varela", "La Plata", "Ezeiza", "Lomas de Zamora", "Marcos Paz", "Cañuelas", "Esteban Echeverría", "Brandsen", "General Las Heras", "Presidente Perón", "San Vicente"),
  code = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41),
  row = c(1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 8),
  col = c(2, 1, 2, 1, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7, 8, 12, 11, 10, 9, 1, 2, 3, 4, 5, 6, 7, 8, 2, 6, 9, 10, 11, 7, 8, 6, 7, 8, 9, 6, 8, 8),
  stringsAsFactors = FALSE
)

geofacet::grid_preview(grilla_amba)


#6. Visualizar la información en mapas
# A continuación, se instalan unas librerías adicionales para ayudar a la visualización de la información.

#Sf
install.packages("sf")
library(sf)

#GeoAr
install.packages("geoAr")
library(geoAr)

# Se utiliza GeoAr para poder extraer la geometría del AMBA (nota: podría ser más directo utilizar un shapefile)

# Se buscan los códigos de la Provincia de Buenos Aires y CABA, visualizando sus respectivos departamentos y combinándolos en una misma tabla.

show_arg_codes()

print(show_arg_codes(), n = 26)

buenos_aires <- get_geo(geo = "BUENOS AIRES")

(buenos_aires_departamentos <- buenos_aires %>%
    add_geo_codes())

caba <- get_geo(geo = "CABA")

(caba_departamentos <- caba %>%
    add_geo_codes())

datos_combinados_amba <- bind_rows(buenos_aires_departamentos, caba_departamentos)

# Luego, se procede a transformar el listado de "departamentos_de_interes" para hacerlo compatible con los códigos de GeoAr. Se utiliza ese listado y el de las comunas de CABA para filtrar los polígonos de la geometría provista por GeoAr.

departamentos_de_interes <- sapply(departamentos_de_interes, function(x) toupper(chartr("áéíóúñ", "AEIOUN", x)))

datos_combinados_amba <- datos_combinados_amba %>%
  mutate(nomdepto_censo = gsub("\\b0+(\\d+)", "\\1", nomdepto_censo))

base_amba <- datos_combinados_amba %>%
  filter(nomdepto_censo %in% departamentos_de_interes)

# Con estas operaciones se pueden observar los polígonos del AMBA.

ggplot()+ 
  geom_sf(data=base_amba)
  
base_amba <- base_amba %>%
  rename(departamento = nomdepto_censo)

# Para proceder a la visualización espacial de la información previamente estudiada, se transforman los datos de la columna "departamento" a fin de unirla con la base espacial del AMBA.

viviendas_y_familias_por_departamento <- viviendas_y_familias_por_departamento %>%
  mutate(departamento = toupper(chartr("ÁÉÍÓÚÑáéíóúñ", "AEIOUNaeioun", departamento)))


union_tablas_para_visualizacion <- left_join(base_amba, viviendas_y_familias_por_departamento, by = "departamento")

# Por último, se procede a visualizar los siguientes mapas:

# 1. Cantidad de barrios por departamento: 
ggplot()+ geom_sf(data=union_tablas_para_visualizacion, aes(fill=cantidad_barrios), color=
                    "white"
)+ labs(title =
          "Cantidad de barrios populares por departamento"
        , subtitle =
          "Área Metropolitana de Buenos Aires"
        , fill =
          "Cantidad de barrios"
        , caption=
          "Fuente: RENABAP 2022"
) + scale_fill_distiller(palette =
                           "OrRd"
                         , direction =
                           1
) + theme_minimal()


# 2. Cantidad de viviendas por departamento: 
ggplot()+ geom_sf(data=union_tablas_para_visualizacion, aes(fill=total_viviendas), color=
                    "white"
)+ labs(title =
          "Cantidad de viviendas por departamento"
        , subtitle =
          "Área Metropolitana de Buenos Aires"
        , fill =
          "Cantidad de viviendas"
        , caption=
          "Fuente: RENABAP 2022"
) + scale_fill_distiller(palette =
                           "OrRd"
                         , direction =
                           1
) + theme_minimal()


# 3. Cantidad de barrios por departamento: 
ggplot()+ geom_sf(data=union_tablas_para_visualizacion, aes(fill=total_familias), color=
                    "white"
)+ labs(title =
          "Cantidad de familias por departamento"
        , subtitle =
          "Área Metropolitana de Buenos Aires"
        , fill =
          "Cantidad de familias"
        , caption=
          "Fuente: RENABAP 2022"
) + scale_fill_distiller(palette =
                           "OrRd"
                         , direction =
                           1
) + theme_minimal()

# En primer lugar, estos mapas permiten observar que no hay un patrón uniforme en la distribución de barrios, hogares y familias, pero sí se puede observar menores valores en CABA, con excepción al sur de la ciudad. Por otro lado, los mapas permiten observar que hay 8 departamentos que no cuentan con BP (los polígonos grises), a saber: Ezeiza, General Las Heras, y dentro de CABA, las Comunas 3, 5, 10, 11, 12 y 13.