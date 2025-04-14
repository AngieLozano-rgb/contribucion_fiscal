# Library -----------------------------------------------------------------

#install.packages("pacman")
library(pacman)
p_load(tidyverse, httr,jsonlite, googledrive, writexl, haven, stringr, labelled, 
       lubridate, gtsummary,googlesheets4)

# Google Drive ------------------------------------------------------------
#drive_find(n_max = 5)

# 1. Crear caché

#options(gargle_oauth_cache = "secrets")
#gargle::gargle_oauth_cache()
#drive_auth()
#list.files("secrets/")

#drive_auth()

# 2. Autenticar con caché
options(
  gargle_oauth_cache = "secrets",
  gargle_oauth_email = TRUE
)

id_folder_crudas <- "1G4M5xf4mMWedThrWGQVluAV_WkUbU5SR"
id_folder_auditoria <- "1DESmm8DWtlUm1ZTEHlE7Yi3IlY57RotU"

folder_output_drive_crudas <- drive_get(as_id(id_folder_crudas))
folder_output_drive_auditoria <- drive_get(as_id(id_folder_auditoria))

# Upload Crudas to GoogleDrive --------------------------------------------
# Upload RData
drive_upload(
  paste0('1_Crudas/contribucion_fiscal_cruda_', Sys.Date(), '.RData'),
  path = as_id(folder_output_drive_crudas)
)

# Upload xlsx
drive_upload(
  paste0('1_Crudas/contribucion_fiscal_cruda_', Sys.Date(), '.xlsx'),
  path = as_id(folder_output_drive_crudas)
)

# Upload csv
drive_upload(
  paste0('1_Crudas/contribucion_fiscal_cruda_', Sys.Date(), '.csv'),
  path = as_id(folder_output_drive_crudas)
)

# Type Google Sheet
drive_upload(
  paste0('1_Crudas/contribucion_fiscal_cruda_', Sys.Date(), '.csv'),
  name = paste0('contribucion_fiscal_cruda_', Sys.Date()),
  type = 'spreadsheet',
  path = as_id(folder_output_drive_crudas)
)

# Upload Auditoría to GoogleDrive -----------------------------------------

# Upload RData
drive_upload(
  paste0('2_Auditadas/contribucion_fiscal_audit_', Sys.Date(), '.RData'),
  path = as_id(folder_output_drive_auditoria)
)

# Upload xlsx
drive_upload(
  paste0('2_Auditadas/contribucion_fiscal_audit_', Sys.Date(), '.xlsx'),
  path = as_id(folder_output_drive_auditoria)
)

# Upload csv
drive_upload(
  paste0('2_Auditadas/contribucion_fiscal_audit_', Sys.Date(), '.csv'),
  path = as_id(folder_output_drive_auditoria)
)

# Type Google Sheet
drive_upload(
  paste0('2_Auditadas/contribucion_fiscal_audit_', Sys.Date(), '.csv'),
  name = paste0('contribucion_fiscal_audit_', Sys.Date()),
  type = 'spreadsheet',
  path = as_id(folder_output_drive_auditoria)
)

# Dashboard ---------------------------------------------------------------

# Autenticación
gs4_auth(email = 'alozano@equilibriumbdc.com', cache = 'secrets')

# ID del documento de Google Sheets
id_dashboard <- "1AUFxK7Xj7GrYHjA7SgcGfrunvp13CTx3Kwv8V5GOWKk"

contribucion_fiscal_audit_dashboard <- data

contribucion_fiscal_audit_dashboard <- contribucion_fiscal_audit_dashboard %>%
  select(-c(`_attachments`, `_geolocation`, `_tags`, `_notes`))

# Escribir datos en una hoja específica
sheet_write(
  data = contribucion_fiscal_audit_dashboard,
  ss = as_sheets_id(id_dashboard),
  sheet = 'Hoja 1'
)

rm(list = ls())