# 1. Instalar librerias --------------------------------------------------------

#install.packages("pacman")
library(pacman)

p_load(dplyr, gt, googledrive, gtsummary, googlesheets4, ggplot2,httr, haven, 
       jsonlite, kableExtra, labelled, lubridate, purrr, skimr, stringr, 
       tidyverse, tidyr, tidygeocoder, readxl, writexl,
       sf, ggplot2, tmap,leaflet)

# 2. Importar datos ------------------------------------------------------------

load(paste0('1_Crudas/contribucion_fiscal_cruda_', Sys.Date(), '.RData'))
shp_sectores <- st_read("Otros/contribucion_fiscal_sectores.gpkg")

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

# Lista de variables y sus respectivas opciones

  variables_opciones <- list(
    c8 = 1:14,
    dep4 = 1:9,
    e_t3 = 1:4,
    e_v13 = 1:11 ,
    e_r1_2 = 1:9 ,
    g1 = 1:12
  )

# Función para expandir opciones de respuesta
  expandir_opciones <- function(data, var, opciones) {
    # Convertir las respuestas en una lista separada por espacios
    data <- data %>%
      mutate(across(all_of(var), ~ str_split(.x, " "), .names = "split_{col}"))
      # Crear las nuevas columnas (dep4/1, dep4/2, ...) con valor 1 si la opción fue seleccionada
      for (i in opciones) {
      data <- data %>%
      mutate(!!paste0(var, "/", i) := if_else(str_detect(paste(data[[paste0("split_", var)]]), as.character(i)), 1, 0))
      }
      # Eliminar la columna auxiliar "split_{var}"
      data <- data %>% select(-contains("split_"))
    return(data)
  }

# Aplicamos la función a cada variable
  for (var in names(variables_opciones)) {
    opciones <- variables_opciones[[var]]
    data <- expandir_opciones(data, var, opciones)
    }

# 3. Missings
## Todas las variables
variables <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7", 
               "c1", "c2", "c3", "c5", "c6", 
               "d1", 
               "e_a1","e_a2","e_a3","e_a4",
               "e_b1","e_b2","e_b3",
               "e_t1","e_t2","e_t3",
               "e_v1","e_v2","e_v3","e_v4","e_v5","e_v6","e_v7","e_v8","e_v9","e_v10",
               "e_v11","e_v12","e_v13","e_v14","e_v15","e_v16","e_v17","e_v18",
               "e_s1","e_s2","e_s3","e_e1","e_o1","e_o2","e_o3","e_o4","e_o5",
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

#table(data$ale_missing_tot)

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

#table(data$ale_outlier_tot)

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

#table(data$ale_no_respuesta_tot)

# 5. Exceso de 0

# Crear columnas de alerta solo para valores 0
data <- data %>%
  mutate(across(all_of(variables_ing_gastos), 
                ~ if_else(.x == 0, 1, 0), 
                .names = "ale_ex_cero_{.col}"))

# Identificar nombres de las columnas de alerta
alertas_ex_ceros <- grep("^ale_ex_cero_", names(data), value = TRUE)

# Sumar alertas, reemplazando NA por 0
data <- data %>%
  mutate(across(all_of(alertas_ex_ceros), ~ replace_na(., 0))) %>%
  mutate(ale_ex_cero_tot = rowSums(across(all_of(alertas_ex_ceros))))

#table(data$ale_ex_cero_tot)

# Revisar gastos totales

cols_a <- c("e_a1", "e_a2", "e_a3", "e_a4")
cols_b <- c("e_b1", "e_b2", "e_b3")
cols_t <- c("e_t1", "e_t2", "e_t4")
cols_v <- c("e_v3", "e_v14", "e_v15", "e_v16", "e_v17", "e_v18")
cols_otros <- c("e_s3", "e_e1", "e_o1", "e_o2", "e_o3")

data <- data %>%
  mutate(
    total_alimentacion = as.numeric(as.character(total_alimentacion)),
    total_bebidas = as.numeric(as.character(total_bebidas)),
    total_transporte = as.numeric(as.character(total_transporte)),
    total_vivienda = as.numeric(as.character(total_vivienda)),
    total_gastos_mensuales = as.numeric(as.character(total_gastos_mensuales)),)


data <- data %>%
  mutate(across(
    all_of(c(cols_a, cols_b, cols_t, cols_v, cols_otros)),
    ~ as.numeric(as.character(.))
  ))

data <- data %>%
  mutate(
    total_alimentacion = if_else(
      is.na(total_alimentacion),
      rowSums(across(all_of(cols_a), ~ ifelse(. != 9999, ., 0)), na.rm = TRUE),
      total_alimentacion
    ),
    
    total_bebidas = if_else(
      is.na(total_bebidas),
      rowSums(across(all_of(cols_b), ~ ifelse(. != 9999, ., 0)), na.rm = TRUE),
      total_bebidas
    ),
    
    total_transporte = if_else(
      is.na(total_transporte),
      rowSums(across(all_of(cols_t), ~ ifelse(. != 9999, ., 0)), na.rm = TRUE),
      total_transporte
    ),
    
    total_vivienda = if_else(
      is.na(total_vivienda),
      rowSums(across(all_of(cols_v), ~ ifelse(. != 9999, ., 0)), na.rm = TRUE),
      total_vivienda
    ),
    
    total_gastos_mensuales = if_else(
      is.na(total_gastos_mensuales),
      rowSums(across(all_of(c(cols_a, cols_b, cols_t, cols_v, cols_otros)), ~ ifelse(. != 9999, ., 0)), na.rm = TRUE),
      total_gastos_mensuales
    )
  )

# cuales tienen alertas por tipo: outliers, NS/NR, ingresos vs gastos -> alerta cuando la diferencia no es normal  

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

#table(data$ale_incongruencias_tot)

valores_invalidos <- c(999, 9999, 99999, 999999, 888, 8888, 88888, 888888)

data_hogar <- data %>%
  group_by(instanceID) %>%
  mutate(
    # Ingreso ind4: si todos son inválidos, poner 0; si hay válidos, sumarlos
    ingreso_ind4 = if (all(ind4 %in% valores_invalidos | is.na(ind4))) {
      0
    } else {
      sum(coalesce(if_else(ind4 %in% valores_invalidos, NA_real_, ind4), 0), na.rm = TRUE)
    },
      ingreso_dep3 = if (all(dep3 %in% valores_invalidos | is.na(dep3))) {
      0
    } else {
      sum(coalesce(if_else(dep3 %in% valores_invalidos, NA_real_, dep3), 0), na.rm = TRUE)
    }
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    alerta_sin_ingresos = if_else(ingreso_ind4 == 0 & ingreso_dep3 == 0, 1, 0)
  )

data_hogar <- data_hogar %>%
  mutate(across(c(e_s1, e_s2, e_s3, e_o4), 
                ~ ifelse(. %in% valores_invalidos, NA, . / 6), 
                .names = "mensual_{.col}"))

data_hogar <- data_hogar %>%
  mutate(mensual_e_o5 = ifelse(e_o5 %in% valores_invalidos, NA, e_o5 / 12))

data_hogar <- data_hogar %>%
  mutate(
    mensual_e_r1_4 = case_when(
      e_r1_4 %in% valores_invalidos ~ NA_real_,
      e_r1_3 == 1 ~ e_r1_4 * 4,        # Semanal
      e_r1_3 == 2 ~ e_r1_4 * 2,        # Quincenal
      e_r1_3 == 3 ~ e_r1_4,            # Mensual
      e_r1_3 == 4 ~ e_r1_4 / 2,        # Bimestral
      e_r1_3 == 5 ~ e_r1_4 / 3,        # Trimestral
      e_r1_3 == 6 ~ e_r1_4 / 6,        # Semestral
      e_r1_3 == 7 ~ e_r1_4 / 12,       # Anual
      TRUE ~ NA_real_
    )
  )

variables_gastos <- c("e_a1", "e_a2", "e_a3", "e_a4","e_b1", "e_b2", "e_b3", 
                      "e_t1", "e_t2", "e_t4", "e_v2", "e_v3","e_v12", 
                      "e_v14", "e_v15", "e_v16", "e_v17", "e_v18", 
                      "e_e1", "e_s3",
                      "e_o1", "e_o2", "e_o3", 
                      "e_ah1","e_d1",
                      "mensual_e_s1","mensual_e_s2","mensual_e_o4","mensual_e_o5",
                      "mensual_e_r1_4")

data_hogar <- data_hogar %>%
  mutate(across(all_of(variables_gastos), ~ as.numeric(.))) %>%
  mutate(across(all_of(variables_gastos), ~ ifelse(. %in% valores_invalidos, NA, .)))

data_hogar <- data_hogar %>%
  rowwise() %>%
  mutate(gasto_total = sum(c_across(all_of(variables_gastos)), na.rm = TRUE)) %>%
  ungroup()

data_hogar <- data_hogar %>%
  rowwise() %>%
  mutate(ingreso_total = sum(ingreso_ind4, ingreso_dep3, na.rm = TRUE)) %>%
  ungroup()

data_hogar <- data_hogar %>%
  mutate(
    balance_hogar = ingreso_total - gasto_total
  )

data_hogar <- data_hogar %>%
  mutate(
    porcentaje_gasto_sobre_ingreso = if_else(
      ingreso_total > 0,
      gasto_total / ingreso_total * 100,
      NA_real_
    )
  )

data_hogar <- data_hogar %>%
  mutate(
    exceso_porcentaje = if_else(
      ingreso_total > 0,
      (gasto_total - ingreso_total) / ingreso_total * 100,
      NA_real_))

data_hogar <- data_hogar %>%
  mutate(
    clasificacion_exceso = case_when(
      is.na(exceso_porcentaje) ~ "No disponible",
      exceso_porcentaje <= 0 ~ "Sin exceso",
      exceso_porcentaje <= 20 ~ "Leve (0–20%)",
      exceso_porcentaje <= 100 ~ "Moderado (20–100%)",
      exceso_porcentaje <= 500 ~ "Grave (100–500%)",
      exceso_porcentaje > 500 ~ "Extremo (>500%)"))

#table(data_hogar$clasificacion_exceso)

# Revisar la ubicacion de las encuestas según su zona censal

# 1. Dividir la variable ubicación para obtener la latitud y longitud de cada punto

data_hogar <- data_hogar %>%
  separate(ubicacion, into = c("lat", "lon", "alt", "otra"), sep = " ") %>%
  mutate(across(c(lat, lon), as.numeric))

# 2. Transformar la base data_hogar en formato sf y unirla con la base de sectores

data_hogar <- st_as_sf(data_hogar, coords = c("lon", "lat"), crs = 4326)  # WGS84
data_hogar <- st_transform(data_hogar, st_crs(shp_sectores))

# 3. Calcular el índice del polígono más cercano
idx_sector_cercano <- st_nearest_feature(data_hogar, shp_sectores)

# 4. Agregar columnas del sector más cercano
data_hogar <- bind_cols(
  data_hogar,
  shp_sectores[idx_sector_cercano, ] %>% st_drop_geometry()
)

# 4. Calcular distancia mínima en metros
data_hogar$dist_min_m <- as.numeric(st_distance(data_hogar, shp_sectores[idx_sector_cercano, ], by_element = TRUE))

# 5. Visualizar en el mapa las ubicaciones
#tmap_mode("view")
#tm_shape(shp_sectores) +
#  tm_polygons() +
#  tm_shape(data_hogar) +
#  tm_dots(col = "blue", size = 0.05)

#summary(data_hogar$dist_min_m)
#quantile(data_hogar$dist_min_m, probs = seq(0, 1, 0.1), na.rm = TRUE)

# 6. Clasificar la distancia en categorias
data_hogar <- data_hogar %>%
  mutate(distancia_categoria = case_when(
    dist_min_m <= 10 ~ "0–10 m",
    dist_min_m <= 50 ~ "11–50 m",
    dist_min_m <= 150 ~ "51–150 m",
    dist_min_m <= 300 ~ "151–300 m",
    dist_min_m <= 1000 ~ "301 m – 1 km",
    dist_min_m <= 5000 ~ "1–5 km",
    dist_min_m > 5000 ~ "> 5 km"
  ))

# Revisar puntos fuera de la zona
#puntos_fuera <- data_hogar %>% 
#  filter(is.na(sec_anm))  
#summary(puntos_fuera$dist_min_m)
#quantile(puntos_fuera$dist_min_m, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Unir la base data_hogar con data

# 1. Convertir data_hogar a data.frame
data_hogar <- as.data.frame(data_hogar)

# 2. Identificar las variables nuevas que tiene data_hogar y que no están en data
nuevas_vars <- setdiff(names(data_hogar), names(data))

# 3. Agregar instanceID para poder hacer el join
nuevas_vars <- c("instanceID", nuevas_vars)

# 4. Hacer el join solo con esas variables
data <- left_join(data, select(data_hogar, all_of(nuevas_vars)), by = "instanceID")

# Encuestas invalidas

data <- data %>%
  mutate(ale_invalida = if_else(
    clasificacion_exceso %in% c("Grave (100–500%)", "Extremo (>500%)", "No disponible"),1, 0
  ))

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
