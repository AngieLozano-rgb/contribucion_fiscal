# 1. Instalar librerias --------------------------------------------------------

#install.packages("pacman")
library(pacman)

p_load(dplyr, gt, googledrive, gtsummary, googlesheets4, ggplot2,httr, haven, 
       jsonlite, kableExtra, labelled, lubridate, purrr, skimr, stringr, 
       tidyverse, tidyr, tidygeocoder, readxl, writexl)

# 2. Importar datos ------------------------------------------------------------

load(paste0('2_Auditadas/contribucion_fiscal_audit_dashboard.RData'))
load(paste0('2_Auditadas/contribucion_fiscal_audit_', Sys.Date(), '.RData'))

data_hogar_labels <- contribucion_fiscal_audit_dashboard %>%
  group_by(instanceID) %>%
  slice(1) %>%
  ungroup()

data_hogar_labels <- data_hogar_labels %>%
  mutate(fecha = as.Date(end))

# Base con ale_invalida == 0
data_hogar_labels_val <- data_hogar_labels %>%
  filter(ale_invalida == 0 & distancia_categoria !="> 5 km")

# Base con ale_invalida == 1
data_hogar_labels_inv <- data_hogar_labels %>%
  filter(ale_invalida == 1 | distancia_categoria == "> 5 km")

# corregir
#a0 == "Santa Rosa" if "Machala" id_encuestador == Katherine Oropesa
# id_encuestador == Encuestador Guaranda
# Yonattan Mejías Naranjal
# SAN MIGUEL DE IBARRA if Yonattan Mejías

#conteo_df <- data_hogar_labels_val %>%
#  count(fecha,nom_par,Nombre.del.mapa,id_encuestador, condicion, clasificacion_exceso, dist_min_m,ingreso_total, gasto_total)

conteo_df <- data_hogar_labels_val %>%
  count(nom_par, sec_anm)

#conteo_df_inv <- data_hogar_labels_inv %>%
#  count(fecha,nom_par,Nombre.del.mapa,id_encuestador, condicion, clasificacion_exceso, dist_min_m, ingreso_total, gasto_total)

conteo_df_inv <- data_hogar_labels_inv %>%
  count(nom_par, condicion, Nombre.del.mapa)

#write_xlsx(conteo_df,"conteo_df_val.xlsx")
#write_xlsx(conteo_df_inv,"conteo_df_inv.xlsx")
