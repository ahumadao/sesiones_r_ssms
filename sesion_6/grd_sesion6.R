

# https://www.fonasa.cl/sites/fonasa/datos-abiertos/bases-grd


pacman::p_load(
  readxl, #importar archivos excel
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  labelled,    # añadir 
  summarytools,
  rio,
  skimr,
  DataExplorer
)


data <- read.csv2(here('data','GRD_PUBLICO_EXTERNO_2022.txt'), sep = '|', fileEncoding = 'UTF-16') 
#el archivo tenía otra codificación, por lo que tuve que importarlo con read.csv2
cie10 <- import(here('data','CIE-10.xlsx'))
cie9 <- import(here('data','CIE-9.xlsx'))
diccionario <- import(here('data','Diccionario.xlsx'))
hospitales <- import(here('data','Tablas maestras bases GRD.xlsx'),which=1) 
# en import(data,which=1) which es para elegir el nombre o el número de la hoja de Excel.
# completar con el resto de las hojas del Excel si se necesita*


## Actividad ##

# 1. Explorar los datos.

#por ejemplo

names(data) #nombres de las columnas
a <- head(data) #mirar las primeras filas
unique(data$COMUNA) #valores únicos en columnas
skimr::skim(data) #reporte en consola
DataExplorer::create_report(data) #reporte html

# 2. Generar una pregunta de investigación de los datos.

data_procesada <- data %>%
  clean_names() %>%
  filter(servicio_salud == 'METROPOLITANO SUR') %>%
  mutate(
    tiempo_hospitalizacion = interval(fecha_ingreso, fechaalta) / days(1),
    prevision2 = ifelse(grepl("FONASA", prevision), "FONASA", prevision)  # Condición para prevision2
  ) %>%
  group_by(cod_hospital, tipo_ingreso) %>%
  summarise(
    n = n(),
    promedio = mean(tiempo_hospitalizacion, na.rm = TRUE)  # Incluyendo na.rm = TRUE para evitar errores con valores NA
  ) %>%
  left_join(hospitales, by = c('cod_hospital'='HOSPITALES')) %>%
  rename('hospital' = '...2') %>% 
  select('hospital', 'tipo_ingreso', 'n','promedio') %>%
  filter(n > 20)


diagnosticos <- data %>%
  clean_names() %>%
  filter(servicio_salud == 'METROPOLITANO SUR') %>%
  mutate(
    tiempo_hospitalizacion = interval(fecha_ingreso, fechaalta) / days(1),
  ) %>%
  group_by(cod_hospital, tipo_ingreso, diagnostico1) %>%
  summarise(
    n = n(),
    promedio = mean(tiempo_hospitalizacion, na.rm = TRUE)  # Incluyendo na.rm = TRUE para evitar errores con valores NA
  ) %>%
  ungroup() %>%
  left_join(hospitales, by = c('cod_hospital'='HOSPITALES')) %>%
  rename('hospital' = '...2') %>% 
  select('hospital', 'diagnostico1', 'n','promedio') %>%
  filter(n > 20) %>% 
  arrange(desc(promedio)) %>%
  slice(1:10) %>%
  left_join(cie10 %>%
              select(Código,Descripción)
            , by=c('diagnostico1'='Código'))



# 3. Obtener información de los datos.
# 4. Generar gráficos informativos. 