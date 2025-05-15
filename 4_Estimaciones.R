# 1. Instalar librerias --------------------------------------------------------

#install.packages("pacman")
library(pacman)

p_load(dplyr, gt, googledrive, gtsummary, googlesheets4, ggplot2,httr, haven, 
       jsonlite, kableExtra, labelled, lubridate, purrr, skimr, stringr, 
       tidyverse, tidyr, tidygeocoder, readxl, writexl)

# 2. Importar datos ------------------------------------------------------------

load(paste0('2_Auditadas/contribucion_fiscal_audit_dashboard.RData'))
load(paste0('2_Auditadas/contribucion_fiscal_audit_', Sys.Date(), '.RData'))


#
seccion_c <- c("c1", "c2", "c3", "c4", "c5", "c6","c7","c9")

# Crear tabla descriptiva
caracterizacion <- data_labels %>%
  select(all_of(seccion_c)) %>%
  tbl_summary(
    by = NULL, # Sin desagregación
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    missing = "ifany", # Muestra valores perdidos si existen
    digits = all_continuous() ~ 1
  ) %>%
  modify_caption("**Tabla de Estadística Descriptiva**")

caracterizacion
## 47% hombres y 53% mujeres
## edad 36 años
## 92% venezolanos
## Estudios superiores 

## revisar familias mixtas y solo venezolanas

table(data_labels$c3)

# nacionalidad y jefatura de hogar


# Filtrar solo las columnas que corresponden a c9
cols_c8 <- grep("^c8_", names(data_labels), value = TRUE)

# Crear la tabla de frecuencias
situacion_migratoria <- data_labels %>%
  select(all_of(cols_c8)) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Opción", values_to = "Frecuencia") %>%
  arrange(desc(Frecuencia))

## Pasaporte venezolano vigente, 
## Cédula de identidad ecuatoriana para extranjeros vigente,
## Visa VIRTE (Vigente)
## (47)  14no cuenta con ninguno

 
seccion_d <- c("d1", "d2", "d3", "d4", "d5", "d6")

# Crear tabla descriptiva
situacion_laboral <- data_labels %>%
  select(all_of(seccion_d)) %>%
  tbl_summary(
    by = NULL, # Sin desagregación
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    missing = "ifany", # Muestra valores perdidos si existen
    digits = all_continuous() ~ 1
  ) %>%
  modify_caption("**Tabla de Estadística Descriptiva**")

situacion_laboral

seccion_ind <- c("ind2", "ind3", "ind6")

situacion_laboral_ind <- data_labels %>%
  select(all_of(seccion_ind)) %>%
  tbl_summary(
    by = NULL, # Sin desagregación
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    missing = "ifany", # Muestra valores perdidos si existen
    digits = all_continuous() ~ 1
  ) %>%
  modify_caption("**Tabla de Estadística Descriptiva**")

situacion_laboral_ind

seccion_dep <- c("dep1","dep2","dep4","dep5")

situacion_laboral_dep <- data_labels %>%
  select(all_of(seccion_dep)) %>%
  tbl_summary(
    by = NULL, # Sin desagregación
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    missing = "ifany", # Muestra valores perdidos si existen
    digits = all_continuous() ~ 1
  ) %>%
  modify_caption("**Tabla de Estadística Descriptiva**")

situacion_laboral_dep

## Ingresos

valores_invalidos <- c(999, 9999, 99999, 999999, 888, 8888, 88888, 888888)

data <- data %>%
  filter(ale_invalida == 0 & distancia_categoria !="> 5 km")

data %>%
  filter(d1 == 1, !dep3 %in% valores_invalidos, !is.na(dep3)) %>%
  summarise(
    Min = min(dep3, na.rm = TRUE),
    Q1 = quantile(dep3, 0.25, na.rm = TRUE),
    Mediana = median(dep3, na.rm = TRUE),
    Media = mean(dep3, na.rm = TRUE),
    Q3 = quantile(dep3, 0.75, na.rm = TRUE),
    Max = max(dep3, na.rm = TRUE),
    Desviación_std = sd(dep3, na.rm = TRUE)
  )

data %>%
  filter(d1 == 1, !ind4 %in% valores_invalidos, !is.na(ind4)) %>%
  summarise(
    Min = min(ind4, na.rm = TRUE),
    Q1 = quantile(ind4, 0.25, na.rm = TRUE),
    Mediana = median(ind4, na.rm = TRUE),
    Media = mean(ind4, na.rm = TRUE),
    Q3 = quantile(ind4, 0.75, na.rm = TRUE),
    Max = max(ind4, na.rm = TRUE),
    Desviación_std = sd(ind4, na.rm = TRUE)
  )

##
data %>%
  filter(d1 == 1, !ind5 %in% valores_invalidos, !is.na(ind5)) %>%
  summarise(
    Min = min(ind5, na.rm = TRUE),
    Q1 = quantile(ind5, 0.25, na.rm = TRUE),
    Mediana = median(ind5, na.rm = TRUE),
    Media = mean(ind5, na.rm = TRUE),
    Q3 = quantile(ind5, 0.75, na.rm = TRUE),
    Max = max(ind5, na.rm = TRUE),
    Desviación_std = sd(ind5, na.rm = TRUE)
  )

data <- data %>%
  mutate(
    dep3 = ifelse(dep3 %in% valores_invalidos, NA_real_, dep3),
    ind4 = ifelse(ind4 %in% valores_invalidos, NA_real_, ind4),
    ind5 = ifelse(ind5 %in% valores_invalidos, NA_real_, ind5)
      ) %>%
  mutate(
    ingreso_anual_dep = case_when(
      d2 %in% c(1, 2, 3, 4) ~ replace_na(dep3, 0) * 12,
      TRUE ~ 0
    )) %>%
  mutate(
    ingreso_anual_ind = case_when(
      d2 %in% c(5, 6, 7) ~ replace_na(ind4, 0) * 12,
      TRUE ~ 0
    )) %>%
  mutate(
    gasto_anual_ind = case_when(
      d2 %in% c(5, 6, 7) ~ replace_na(ind5, 0) * 12,
      TRUE ~ 0
    ))

## Estadística descriptivas ingreso anual individual

data %>%
  filter(d1 == 1, !ingreso_anual_ind %in% valores_invalidos, !is.na(ingreso_anual_ind)) %>%
    summarise(
    Min = min(ingreso_anual_ind, na.rm = TRUE),
    Q1 = quantile(ingreso_anual_ind, 0.25, na.rm = TRUE),
    Mediana = median(ingreso_anual_ind, na.rm = TRUE),
    Media = mean(ingreso_anual_ind, na.rm = TRUE),
    Q3 = quantile(ingreso_anual_ind, 0.75, na.rm = TRUE),
    Max = max(ingreso_anual_ind, na.rm = TRUE),
    Desviación_std = sd(ingreso_anual_ind, na.rm = TRUE)
  )

## independientes
## ingresos - gastos del negocio
### Calculo IR dependientes
# Calcular aportes al IESS (9.45% del ingreso bruto)

data <- data %>%
  mutate(
    iess_mensual = if_else(d6 %in% c(6, 8), 0, dep3 * 0.0945),
    iess_anual = iess_mensual * 12,
    base_imponible_dep = ingreso_anual_dep - iess_anual)
    
data <- data %>%
  mutate(
    base_imponible_ind = ingreso_anual_ind - gasto_anual_ind)

# Función para calcular el impuesto a la renta según la tabla de Ecuador
calcular_impuesto_renta <- function(base_imponible) {
  tramos <- data.frame(
    fraccion_basica = c(0, 11902, 15159, 19682, 26031, 34255, 45407, 60450, 80605, 107199),
    exceso_hasta = c(11902, 15159, 19682, 26031, 34255, 45407, 60450, 80605, 107199, Inf),
    impuesto_fraccion_basica = c(0, 0, 163, 615, 1377, 2611, 4841, 8602, 14648, 23956),
    porcentaje_excedente = c(0.00, 0.05, 0.10, 0.12, 0.15, 0.20, 0.25, 0.30, 0.35, 0.37)
  )
  
sapply(base_imponible, function(x) {
    tramo <- tramos[x >= tramos$fraccion_basica & x <= tramos$exceso_hasta, ]
    if (nrow(tramo) == 0 || is.na(x)) return(NA_real_)
    tramo$impuesto_fraccion_basica + (x - tramo$fraccion_basica) * tramo$porcentaje_excedente
  })}

# Aplicar la función de cálculo de IR

data <- data %>%
  mutate(impuesto_renta_dep = calcular_impuesto_renta(base_imponible_dep))

data <- data %>%
  mutate(impuesto_renta_ind = calcular_impuesto_renta(base_imponible_ind))

summary(data$impuesto_renta_dep)
summary(data$impuesto_renta_ind)

## GASTOS
# anualizar gastos
# supuesto: lineal

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
