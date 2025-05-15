#------------------------------------------------------------------------------#
# 1. Instalar librerias
#install.packages("pacman")
library(pacman)

p_load(dplyr, gt, googledrive, gtsummary, googlesheets4, ggplot2,httr, haven, 
       jsonlite, kableExtra, labelled, lubridate, purrr, skimr, stringr, 
       tidyverse, tidyr, tidygeocoder, readxl, writexl)

#------------------------------------------------------------------------------#

# 2. Importar datos

load(paste0('2_Auditadas/contribucion_fiscal_audit_dashboard.RData'))
load(paste0('2_Auditadas/data.RData'))

#------------------------------------------------------------------------------#

contribucion_fiscal_audit_dashboard <- contribucion_fiscal_audit_dashboard %>%
  mutate(fecha = as.Date(end))

contribucion_fiscal_audit_dashboard <- contribucion_fiscal_audit_dashboard %>%
  filter(
    (encuesta_id == "29-190047" & total_ingreso_persona %in% c(600, 400, 420)) |
      (encuesta_id == "29-192158" & total_ingreso_persona %in% c(600, 0, 500)) |
      !(encuesta_id %in% c("29-190047", "29-192158")))

data_hogar <- contribucion_fiscal_audit_dashboard %>%
  group_by(encuesta_id) %>%
  slice(1) %>%
  ungroup()

table(data_hogar$ale_invalida)
table(data_hogar$tipo_hogar)
table(data_hogar$clasificacion_exceso)
table(data_hogar$distancia_categoria)
table(data_hogar$duration_minutes)

data_hogar_val <- data_hogar %>%
  filter(
    (ale_invalida == 0 & distancia_categoria != "> 5 km") | 
      (instanceID %in% c("uuid:047b8af9-e9e3-4bdb-a7e7-1c8db2acb9d6", 
                         "uuid:4dd0aae6-50e5-4832-a1d1-48c882132737")))

table(data_hogar_val$ale_invalida)
table(data_hogar_val$tipo_hogar)
table(data_hogar_val$clasificacion_exceso)
table(data_hogar_val$distancia_categoria)
table(data_hogar_val$duration_minutes)

write_xlsx(data_hogar_val,"data_val_hogar.xlsx")

data_hogar_val_mixtos <- data_hogar_val %>%
  filter(
    (tipo_hogar != "Solo venezolanos"))

conteo_df_mixtos <- data_hogar_val_mixtos %>%
  count(fecha, id_encuestador, nom_par,Nombre_del_mapa)

write_xlsx(conteo_df_mixtos,"conteo_df_mixtos.xlsx")

# sin personas venezolanas
# "84c70dc3-76df-4c26-8cc4-1501252b3505"
# cambiar 2025-04-23 19-113915 a 19-113916
# duplicada uuid:580e7efc-167e-4e23-a1e3-7129fac1fd5f    
## eliminar el segundo 29-190047 y el primero 29-192158

data_hogar_inv <- data_hogar %>%
  filter(ale_invalida == 1 )

data_hogar_inv <- data_hogar_inv %>%
  filter(
    ( instanceID != "uuid:047b8af9-e9e3-4bdb-a7e7-1c8db2acb9d6" &
      instanceID != "uuid:4dd0aae6-50e5-4832-a1d1-48c882132737"))

table(data_hogar_inv$ale_invalida)
table(data_hogar_inv$tipo_hogar)
table(data_hogar_inv$clasificacion_exceso)
table(data_hogar_inv$distancia_categoria)
table(data_hogar_inv$duration_minutes)

  conteo_df_inv <- data_hogar_inv %>%
    count(fecha, encuesta_id, nom_par, Nombre_del_mapa, id_encuestador, condicion,tipo_hogar, clasificacion_exceso, dist_min_m,duration_minutes ,ingreso_total, gasto_total, ind4,dep3)

write_xlsx(conteo_df_inv,"conteo_df_inv.xlsx")

  # Exploración de datos  

    # Sección A. Criterios de inclusión
    seccion_a <- c("a1","a2", "a3","a5","a6")
    
    criterios_inclusion <- data_hogar_labels_val%>%
      select(all_of(seccion_a)) %>%
      tbl_summary(
        by = NULL, # Sin desagregación
        statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
        missing = "ifany", # Muestra valores perdidos si existen
        digits = all_continuous() ~ 1
      ) %>%
      modify_caption("**Tabla de Estadística Descriptiva**")
    
    criterios_inclusion
    
    ## Sección B
    seccion_b <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")
    
    data_hogar <- data_hogar_labels_val %>%
      mutate(across(all_of(seccion_b), ~ str_trim(.))) %>%  # Eliminar espacios en blanco
      mutate(across(all_of(seccion_b), ~ as.numeric(.))) %>%  # Convertir a numérico
      mutate(across(all_of(seccion_b), ~ replace_na(., 0)))  # Reemplazar NA con 0
    
    caracteristicas_hogar <- seccion_b %>%
      map_df(~ {
        stats <- summary(data_hogar_labels[[.]])
        tibble(
          Variable = .,
          Min = stats[1],
          Q1 = stats[2],
          Mediana = stats[3],
          Media = stats[4],
          Q3 = stats[5],
          Max = stats[6]
        )
      })
    
    caracteristicas_hogar %>%
      gt() %>%
      tab_header(title = "Resumen Composición del Hogar")
    
  ## Base personas

    data_val <- data %>%
      filter(ale_invalida == 0 & distancia_categoria !="> 5 km")
    
    data_labels <- contribucion_fiscal_audit_dashboard

    data_labels_val <- data_labels %>%
      filter(ale_invalida == 0 & distancia_categoria !="> 5 km")
    
    ## Sección C
    
    seccion_c <- c("c1", "c2", "c3", "c5", "c6","c7","c9")
    
    caracterizacion <- data_labels_val %>%
      select(all_of(seccion_c)) %>%
      tbl_summary(
        by = NULL, # Sin desagregación
        statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
        missing = "ifany", # Muestra valores perdidos si existen
        digits = all_continuous() ~ 1
      ) %>%
      modify_caption("**Tabla de Caracterización**")
    
    caracterizacion
    
    ## 47% hombres y 53% mujeres
    ## edad 36 años
    ## 92% venezolanos
    ## Estudios superiores 
    
    # Filtrar los jefes de hogar y seleccionar la nacionalidad
    
    jefes_hogar <- data %>%
      filter(c5 == 1) %>%
      select(instanceID, nacionalidad_jefe = c3)
    
    table(jefes_hogar$nacionalidad_jefe)
    ## 566 venezolanos
    
data$ind1 <- as.Date(data$ind1, format = "%Y-%m-%d")
    
## Calcular los años de antigüedad
  data$antiguedad_anios <- as.numeric(difftime(Sys.Date(), data$ind1, units = "weeks")) / 52.25

  summary(data$antiguedad_anios)

  #revisar valor negativo

# Duplicados
#encuesta duplicada Guayaquil
  
# Valores faltantes

# Valores atípicos

# Normalización y estandarización

    
# Guardar
