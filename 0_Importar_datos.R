#install.packages("pacman")
library(pacman)

p_load(dplyr, gt, googledrive, gtsummary, googlesheets4, ggplot2,httr, haven, 
       jsonlite, kableExtra, labelled, lubridate, purrr, skimr, stringr, 
       tidyverse, tidyr, tidygeocoder, readxl, writexl)

# Reemplaza con tus credenciales
usuario <- "alozano02"
contrasena <- "KBaplo2023"

# Paso 1: define la URL
url_api <- "https://kobo.humanitarianresponse.info/api/v2/assets"

# Paso 2: realiza la llamada con autenticación
respuesta <- GET(url_api, authenticate(usuario, contrasena))

# Paso 3: convierte a JSON
assets <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))

# Paso 4: selecciona columnas que te interesan
assets_df <- assets$results %>% select(uid, name)

# Paso 5: Seleccionar el formulario y obtener el url
uid_formulario <- "apTvGCaDwt2EKosqmDhXFG"
url_data <- paste0("https://kobo.humanitarianresponse.info/api/v2/assets/", uid_formulario, "/data/")
data <- GET(url_data, authenticate(usuario, contrasena))

# Paso 6: Verifica si la solicitud fue exitosa
if (status_code(data) == 200) {
  # Extraer el contenido de la respuesta en formato texto (JSON)
  json_data <- content(data, as = "text", encoding = "UTF-8")
  # Convertir el contenido JSON a un data.frame
  data <- fromJSON(json_data, flatten = TRUE)$results
  # Ver el data.frame con los resultados
} else {
  stop("No se pudo obtener los datos. Código de error: ", status_code(data_response))
}

# Paso 7: Expandir para que quede en una sola base de datos

  ## Unnest sección H
  df_seccionH <- data %>%
  select(`_uuid`,seccionH) %>%
  unnest(seccionH)

  ## Cambiar nombres de las variables en las secciones
  colnames(df_seccionH) <- gsub("^[^/]+/", "", colnames(df_seccionH))  # Elimina todo hasta el primer /
  colnames(df_seccionH) <- gsub("^[^/]+/", "", colnames(df_seccionH))  # Elimina todo hasta el primer /
  colnames(df_seccionH) <- gsub("^[^/]+/", "", colnames(df_seccionH))  # Elimina todo hasta el primer /

  ## Unir la base con la sección H
  data <- data%>%left_join(df_seccionH)%>%
    select(-seccionH)

  ## Cambiar nombres de las variables
  colnames(data) <- gsub("^[^/]+/", "", colnames(data))  # Elimina todo hasta el primer /
  colnames(data) <- gsub("^[^/]+/", "", colnames(data))  # Elimina todo hasta el primer /

data <- data %>%
    filter(id_encuestador != "E999")

# Paso 8: Guardar en múltiples formatos ---------------------------------------------
  ruta <- paste0("1_Crudas/contribucion_fiscal_cruda_", Sys.Date())
  
  ## RData
  save(data, file = paste0(ruta, ".RData"))
  
  ## Excel
  write_xlsx(data, path = paste0(ruta, ".xlsx"), col_names = TRUE)
  
  ## CSV
  write.csv(data, file = paste0(ruta, ".csv"), row.names = FALSE)
  
# Limpieza del entorno ----------------------------------------------------------
rm(list = ls())
