# 1. Instalar librerias --------------------------------------------------------

#install.packages("pacman")
library(pacman)

p_load(dplyr, gt, googledrive, gtsummary, googlesheets4, ggplot2,httr, haven, 
       jsonlite, kableExtra, labelled, lubridate, purrr, skimr, stringr, 
       tidyverse, tidyr, tidygeocoder, readxl, writexl)

# 2. Importar datos ------------------------------------------------------------

load(paste0('1_Crudas/contribucion_fiscal_cruda_', Sys.Date(), '.RData'))

#

# Lista de variables o crear
vars_necesarias <- c("c4","d3e", "ind2e", "dep1e", "dep2e", "dep4e", "e_r1_2e", "g1e")

# Crear variables si no existen, con valor NA
for (var in vars_necesarias) {
  if (!var %in% names(data)) {
    data[[var]] <- NA
  }
}

data <- data %>%
  mutate(condicion = a1 == 1 & 
           a2 == 1 & 
           (a3 == 1 | a4 == 1) & 
           a5 != 1 & 
           a6 == 1)

# 3. Missings

## Todas las variables
variables <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7", 
               "c1", "c2", "c3", "c5", "c6", 
               "d1", 
               "e_a1","e_a2","e_a3","e_a4","gasto_alimentacion",
               "e_b1","e_b2","e_b3","gasto_bebidas",
               "e_t1","e_t2","e_t3","gasto_transporte",
               "e_v1","e_v2","e_v3","e_v4","e_v5","e_v6","e_v7","e_v8","e_v9","e_v10",
               "e_v11","e_v12","e_v13","e_v14","e_v15","e_v16","e_v17","e_v18","gasto_vivienda",
               "e_s1","e_s2","e_s3","e_e1","e_o1","e_o2","e_o3","e_o4","e_o5",
               "gasto_mensuales",
               "e_ah1", "e_d1","e_r1","e_reg1",
               "g1")

data <- data %>%
  mutate(across(all_of(variables),
                ~ if_else(condicion & is.na(.), 1, 0),
                .names = "ale_missing_{.col}"))

## Variables con restricciones

data <- data %>%
  mutate(
    ale_missing_c4   = if_else(condicion & c3 == 88 & is.na(c4), 1, 0),
    ale_missing_c7   = if_else(condicion & c6 %in% c(5, 7, 8, 9) & is.na(c7), 1, 0),
    ale_missing_c7_1 = if_else(condicion & c6 %in% c(5, 7, 8, 9) & c7 != 1 & is.na(c7_1), 1, 0),
    ale_missing_c8   = if_else(condicion & c3 != 2 & is.na(c8), 1, 0),
    ale_missing_c9   = if_else(condicion & c3 != 2 & is.na(c9), 1, 0),
    ale_missing_d2   = if_else(condicion & d1 == 1 & is.na(d2), 1, 0),
    ale_missing_d3   = if_else(condicion & d1 == 1 & is.na(d2), 1, 0),
    ale_missing_d3e  = if_else(condicion & d3 == 88 & is.na(d3e), 1, 0),
    ale_missing_d4   = if_else(condicion & d1 == 1 & is.na(d2), 1, 0),
    ale_missing_d5   = if_else(condicion & c6 %in% c(5, 7, 8, 9) & d1 == 1 & is.na(d5), 1, 0),
    ale_missing_d6   = if_else(condicion & d1 == 1 & is.na(d2), 1, 0),
    ale_missing_ind1  = if_else(condicion & d2 %in% c(5, 6, 7) & is.na(ind1), 1, 0),
    ale_missing_ind2  = if_else(condicion & d2 %in% c(5, 6, 7) & is.na(ind2), 1, 0),
    ale_missing_ind2e = if_else(condicion & d2 %in% c(5, 6, 7) & ind2 == 88 & is.na(ind2e), 1, 0),
    ale_missing_ind3  = if_else(condicion & d2 %in% c(5, 6, 7) & is.na(ind3), 1, 0),
    ale_missing_ind4  = if_else(condicion & d2 %in% c(5, 6, 7) & is.na(ind4), 1, 0),
    ale_missing_ind5  = if_else(condicion & d2 %in% c(5, 6, 7) & is.na(ind5), 1, 0),
    ale_missing_ind6  = if_else(condicion & d2 %in% c(5, 6, 7) & is.na(ind6), 1, 0),
    ale_missing_dep1  = if_else(condicion & d2 %in% c(1, 2, 3, 4) & is.na(dep1), 1, 0),
    ale_missing_dep1e = if_else(condicion & d2 %in% c(1, 2, 3, 4) & dep1 == 88 & is.na(dep1e), 1, 0),
    ale_missing_dep2  = if_else(condicion & d2 %in% c(1, 2, 3, 4) & is.na(dep2), 1, 0),
    ale_missing_dep2e = if_else(condicion & d2 %in% c(1, 2, 3, 4) & dep2 == 88 & is.na(dep2e), 1, 0),
    ale_missing_dep3  = if_else(condicion & d2 %in% c(1, 2, 3, 4) & is.na(dep3), 1, 0),
    ale_missing_dep4  = if_else(condicion & d2 %in% c(1, 2, 3, 4) & is.na(dep4), 1, 0),
    ale_missing_dep4e = if_else(condicion & d2 %in% c(1, 2, 3, 4) & dep4 == 88 & is.na(dep4e), 1, 0),
    ale_missing_dep4_1= if_else(condicion & d2 %in% c(1, 2, 3, 4) & dep4 != 9 & is.na(dep5), 1, 0),
    ale_missing_dep5  = if_else(condicion & d2 %in% c(1, 2, 3, 4) & is.na(dep5), 1, 0),
    ale_missing_e_t4 = if_else(condicion & (str_detect(e_t3, "1") | str_detect(e_t3, "2")) & is.na(e_t4), 1, 0),
    ale_missing_e_v2 = if_else(condicion & e_v1 %in% c(1, 3, 5) & is.na(e_v2), 1, 0),
    ale_missing_e_v3 = if_else(condicion & e_v1 %in% c(2, 4, 6) & is.na(e_v3), 1, 0),
    ale_missing_e_r1_1  = if_else(condicion & e_r1 == 1 & is.na(e_r1_1), 1, 0),
    ale_missing_e_r1_2  = if_else(condicion & e_r1 == 1 & is.na(e_r1_2), 1, 0),
    ale_missing_e_r1_2e = if_else(condicion & str_detect(e_r1_2, "88") & is.na(e_r1_2e), 1, 0),
    ale_missing_e_r1_3  = if_else(condicion & e_r1 == 1 & is.na(e_r1_3), 1, 0),
    ale_missing_e_r1_4  = if_else(condicion & e_r1 == 1 & is.na(e_r1_4), 1, 0),
    ale_missing_g1e = if_else(condicion & str_detect(g1, "88") & is.na(g1e), 1, 0))

# Crear un vector con los nombres de columnas de missings
alertas_missings <- grep("^ale_missing_", names(data), value = TRUE)

# Sumar las alertas con missings, reemplazando los NA con 0
data <- data %>%
  mutate(across(all_of(alertas_missings), ~ replace_na(., 0))) %>%
  mutate(ale_missing_tot = rowSums(across(all_of(alertas_missings))))

# 4. Outliers

# Lista de variables numéricas
variables_numericas <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7", 
                         "c2","d4","ind4", "ind5","dep3", "dep4_1", 
                         "e_a1", "e_a2", "e_a3", "e_a4","e_b1", "e_b2", "e_b3", 
                         "e_t1", "e_t2", "e_t4", "e_v2", "e_v3","e_v12", 
                         "e_v14", "e_v15", "e_v16", "e_v17", "e_v18", 
                         "e_s1", "e_s2","e_s3","e_e1", 
                         "e_o1", "e_o2", "e_o3", "e_o4", "e_o5", 
                         "e_ah1","e_d1", "e_r1_4", "e_reg1")

# Convertir a numérico si es necesario
data <- data %>%
  mutate(across(all_of(variables_numericas), ~ suppressWarnings(as.numeric(.))))

# Función modificada para ignorar 9999
detectar_outliers <- function(x) {
  if (is.numeric(x)) {
    x_valid <- ifelse(x == 9999, NA, x)  # Ignorar 9999
    Q1 <- quantile(x_valid, 0.25, na.rm = TRUE)
    Q3 <- quantile(x_valid, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    limite_inferior <- Q1 - 1.5 * IQR
    limite_superior <- Q3 + 1.5 * IQR
    return(ifelse(x_valid < limite_inferior | x_valid > limite_superior, 1, 0))
  } else {
    return(rep(0, length(x)))
  }
}

# Aplicar función de detección de outliers
data <- data %>%
  mutate(across(all_of(variables_numericas), detectar_outliers, .names = "ale_outlier_{.col}"))

# Crear un vector con los nombres de columnas de outliers
alertas_outlers <- grep("^ale_outlier_", names(data), value = TRUE)

# Sumar las alertas con outliers, reemplazando los NA con 0
data <- data %>%
  mutate(across(all_of(alertas_outlers), ~ replace_na(., 0))) %>%
  mutate(ale_outlier_tot = rowSums(across(all_of(alertas_outlers))))

# 5. Ex. prefiere no responder/no sabe/otro
variables_ing_gastos <- c("d4","ind4", "ind5","dep3", "dep4_1", 
                          "e_a1", "e_a2", "e_a3", "e_a4","e_b1", "e_b2", "e_b3", 
                          "e_t1", "e_t2", "e_t4", "e_v2", "e_v3","e_v12", 
                          "e_v14", "e_v15", "e_v16", "e_v17", "e_v18", 
                          "e_s1", "e_s2","e_s3","e_e1", 
                          "e_o1", "e_o2", "e_o3", "e_o4", "e_o5", 
                          "e_ah1","e_d1", "e_r1_4", "e_reg1")

# Crear columnas de alerta solo para valores 9999
data <- data %>%
  mutate(across(all_of(variables_ing_gastos), 
                ~ if_else(.x == 9999, 1, 0), 
                .names = "ale_no_respuesta_{.col}"))

# Identificar nombres de las columnas de alerta
alertas_no_respuesta <- grep("^ale_no_respuesta_", names(data), value = TRUE)

# Sumar alertas, reemplazando NA por 0
data <- data %>%
  mutate(across(all_of(alertas_no_respuesta), ~ replace_na(., 0))) %>%
  mutate(ale_no_respuesta_tot = rowSums(across(all_of(alertas_no_respuesta))))


# 6. Validar fechas
data <- data %>%
  mutate(
    start = ymd_hms(start, tz = "America/Lima"),
    end = ymd_hms(end, tz = "America/Lima"),
    difference_min = as.numeric(difftime(end, start, units = "mins")),
    duration_minutes = round(difference_min, 0)
  )
# Estadísticos de duración
sd_duracion <- sd(data$difference_min, na.rm = TRUE)
mediana_duracion <- median(data$difference_min, na.rm = TRUE)

# Añadir alertas por persona
data <- data %>%
  mutate(
    # Alertas de tiempo
    ale_incong_flag_duration_mas = if_else(((difference_min - mediana_duracion)/sd_duracion > 1) & a1 == 1 & difference_min > 30, 1, 0, missing = 0),
    ale_incong_flag_duration_menos = if_else(((difference_min - mediana_duracion)/sd_duracion < -1) & a1 == 1, 1, 0, missing = 0))

# 7. Incongruencias 
data <- data %>%
  group_by(encuesta_id) %>%
  mutate(
    # Personas venezolanas por hogar
    ale_incong_hogar = ifelse(sum(c3 == 1, na.rm = TRUE) == 0, 1, 0),
    
    # Jefe del hogar
    ale_incong_jefe = ifelse(sum(c5 == 1, na.rm = TRUE) > 1 | all(is.na(c5)), 1, 0),
    
    # Personas que dicen trabajar
    trabajan_ind = sum(d1 == 1, na.rm = TRUE),
    b7 = first(b7),
    ale_incong_trabajo = ifelse(!is.na(b7) & b7 > trabajan_ind, 1, 0)
  ) %>%
  ungroup()

data <- data %>%
  mutate(
    # Validación por negocio
#    ale_incong_negocio = case_when(
#      ind3 == 5 & (ind4 < 900 | ind5 < 900) ~ 1,
#      ind3 == 4 & (ind4 < 700 | ind5 < 700) ~ 1,
#      ind3 == 3 & (ind4 < 600 | ind5 < 600) ~ 1,
#      ind3 == 2 & (ind4 < 400 | ind5 < 400) ~ 1,
#      ind3 == 1 & (ind4 < 100 | ind5 < 100) ~ 1,
#      TRUE ~ 0
#    ),
    
    # Validación de adultos por hogar
    total_adultos = b4 + b5 + b6,  # Asegúrate de que estas columnas existan
    
    # Verificación de adultos (si la edad es mayor o igual a 18)
    adulto = ifelse(c2 >= 18, 1, 0),  # Asegúrate de que `c2` esté bien
  ) %>%
  group_by(encuesta_id) %>%
  mutate(
    # Calcular los adultos reportados en cada hogar
    adultos_reportados = sum(adulto, na.rm = TRUE),
    
    # Validación de que el número de adultos reportados coincide con el total de adultos
    ale_incong_adulto = ifelse(total_adultos != adultos_reportados, 1, 0)
  ) %>%
  ungroup()

# Crear un vector con los nombres de columnas de outliers
alerta_incongruencias <- grep("^ale_incong_", names(data), value = TRUE)

# Sumar las alertas con outliers, reemplazando los NA con 0
data <- data %>%
  mutate(across(all_of(alerta_incongruencias), ~ replace_na(., 0))) %>%
  mutate(ale_incongruencias_tot = rowSums(across(all_of(alerta_incongruencias))))

data <- data %>%
  mutate(
    alerta_general = if_else(
      rowSums(select(., starts_with("ale_")), na.rm = TRUE) > 0,
      1, 0
    )
  )

#Guardar en múltiples formatos ---------------------------------------------

ruta <- paste0("2_Auditadas/contribucion_fiscal_audit_", Sys.Date())

## RData
save(data, file = paste0(ruta, ".RData"))

## Excel
write_xlsx(data, path = paste0(ruta, ".xlsx"), col_names = TRUE)

## CSV
write.csv(data, file = paste0(ruta, ".csv"), row.names = FALSE)

# Limpieza del entorno ----------------------------------------------------------
rm(list = ls())

