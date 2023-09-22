#Trabajo Práctico 2

#1. Preparación espacio de trabajo
#Para dar inicio al TP, se instalan los packages y librerías necesarias para operar sobre los datos

#Tidyverse
install.packages("tidyverse")
library(tidyverse)

#Dplyr
install.packages("dplyr")
library(dplyr)


#2. Carga de datos
# En segunda instancia, se procede a importar la base de datos del Relevamiento Nacional de Barrios Populares 2022.

renabap_2022 <- read.csv ("datos/2022-07-13_info_publica.csv")


#3. Análisis preliminar de la base de datos
# Antes de comenzar a trabajar con la información provista por el .csv, se realiza un análisis exploratorio de los datos.

dim(renabap_2022)
# El .csv contiene 5687 filas y 77 columnas.

names(renabap_2022)
# Aquí se puede observar en mayor detalle el contenido de las columnas. De manera preliminar, se definen como variables de interés: departamento, nombre_barrio, cantidad_viviendas_aproximadas, y cantidad_familias_aproximadas.

# A partir de esta selección, se definen las siguientes preguntas a responder: ¿Cuántos barrios populares (BP) se encuentran en el Área Metropolitana de Buenos Aires y, en promedio, cuántas viviendas y familias contienen? ¿Cuáles son los 10 departamentos con mayor cantidad de barrios populares? ¿Cuál es el departamento con mayor cantidad de viviendas y familias? ¿Cuál es el barrio popular con mayor cantidad de viviendas y familias y dónde se encuentra?


#4. Filtro y selección de datos
# A continuación, se procede a filtrar las filas correspondientes al AMBA, a saber, los 40 departamentos compuestos por Almirante Brown, Avellaneda, Berazategui, Berisso, Brandsen, Campana, Cañuelas, Ensenada, Escobar, Esteban Echeverría, Exaltación, Ezeiza, Florencio Varela, Gral. Las Heras, Gral. Rodríguez, Gral. San Martín, Hurlingham, Ituzaingó, José C. Paz, La Matanza, La Plata, Lanús, Lomas de Zamora, Luján, Malvinas Argentinas, Marcos Paz, Merlo, Moreno, Morón, Quilmes, Pilar, Presidente Perón, San Fernando, San Isidro, San Miguel, San Vicente, Tigre, Tres de Febrero, Vicente López, Zárate. Por otro lado, también se seleccionan las variables de interés previamente mencionadas.

departamentos_de_interes <- c("Almirante Brown", "Avellaneda", "Berazategui", "Berisso", "Brandsen", "Campana", "Cañuelas",
                              "Ensenada", "Escobar", "Esteban Echeverría", "Exaltación de la Cruz", "Ezeiza", "Florencio Varela",
                              "General Las Heras", "General Rodríguez", "General San Martín", "Hurlingham", "Ituzaingó",
                              "José C. Paz", "La Matanza", "La Plata", "Lanús", "Lomas de Zamora", "Luján",
                              "Malvinas Argentinas", "Marcos Paz", "Merlo", "Moreno", "Morón", "Quilmes", "Pilar",
                              "Presidente Perón", "San Fernando", "San Isidro", "San Miguel", "San Vicente", "Tigre",
                              "Tres de Febrero", "Vicente López", "Zárate")

columnas_de_interes <- c("departamento", "nombre_barrio", "cantidad_viviendas_aproximadas", 
                         "cantidad_familias_aproximada")

renabap_2022_amba <- renabap_2022 %>%
  filter(departamento %in% departamentos_de_interes)%>%
  select(all_of(columnas_de_interes))

summary(renabap_2022_amba)

# Con estas primeras operaciones se puede responder la primera pregunta (¿cuántos barrios populares se encuentran en el Área Metropolitana de Buenos Aires y, en promedio, cuántas viviendas y familias contienen?).

# Se observan 1771 barrios populares existentes en el AMBA, que en promedio cuentan con 281 viviendas y 309 familias. 


#5. Agrupar y contar información
# Para poder proseguir con las siguientes preguntas, se realizan las siguientes operaciones:

barrios_por_departamentos <- renabap_2022_amba %>%
  group_by(departamento) %>%
  summarise(cantidad_barrios = n_distinct(nombre_barrio)) %>%
  arrange(desc(cantidad_barrios))

top_10_departamentos <- head(barrios_por_departamentos, 10)

# En síntesis, se agrupan los datos por el valor de la columna "departamento". Luego, se cuenta la cantidad de barrios populares en cada uno. Finalmente, se ordenan los resultados de manera descendente, para así poder extraer el top 10 de departamentos con mayor cantidad de BP. 

# De este análisis, se extrae que los departamentos con mayor cantidad de BP son:

print(top_10_departamentos)

# No obstante, si retomamos la información de cantidad de viviendas y familias, podremos observar que los departamentos con mayor cantidad de barrios no coinciden necesariamente con los que presentan mayor cantidad de viviendas:

viviendas_por_departamento <- renabap_2022_amba %>%
  group_by(departamento) %>%
  summarise(total_viviendas = sum(cantidad_viviendas_aproximadas))

print(viviendas_por_departamento)

familias_por_departamento <- renabap_2022_amba %>%
  group_by(departamento) %>%
  summarise(total_familias = sum(cantidad_familias_aproximada))

print(familias_por_departamento)

# En ambos casos se puede observar que Almirante Brown, Avellaneda, Berazategui, Berisso, y Brandsen cuentan con la mayor cantidad de viviendas y familias.

# Cabe destacar que, de todos modos, el barrio popular con mayor cantidad de viviendas y familias es Santa Catalina en Lomas de Zamora, con 7850 viviendas aproximadas y 8635 familias.

renabap_2022_amba_top_10 <- renabap_2022_amba %>%
  filter(departamento %in% c("San Fernando", "La Matanza", "La Plata", "Moreno", "Almirante Brown", "Merlo", "Florencio Varela", "Quilmes", "Tigre", "Lomas de Zamora"))%>%
  arrange(desc(cantidad_viviendas_aproximadas))

top_10_barrios <- head(renabap_2022_amba_top_10, 10)

print(top_10_barrios)