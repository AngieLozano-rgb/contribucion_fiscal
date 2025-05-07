library(pacman)

p_load(readxl, sf, ggplot2, tmap, dplyr,leaflet, purrr, haven,tidyr, writexl,openxlsx)

# Cargar bases
#shp_zonas <- st_read("G:/Unidades compartidas/EQUILIBRIUM   Intranet 🌍📂/🥁 PROYECTOS/🏆 Proyectos S. Desarrollo/118. Impacto fiscal Regional/02 - Implementación/Contribución Ecuador/1. Implementación/Muestreo/sectores_anonimizados.gpkg")
contribucion_fiscal_coordenadas <- read_excel("Otros/contribucion_fiscal_coordenadas.xlsx")
shp_zonas <- st_read("Otros/sectores_anonimizados.gpkg")

# Definir sectores seleccionados
sectores_seleccionados <- c(
  "091250005005", "091250004008", "091250002011", "091250005010", "091150007004",
  "091150002005", "240352010009", "240352010007", "090150291005", "090150431004",
  "090150492008", "090150374008", "090150451002", "090150284008", "090150247002",
  "090150426001", "090150329011", "090150449008", "130850902002", "130850009010",
  "130850902004", "130850034004", "130850009002", "130850902005", "130850022006",
  "130850010007", "130950918004", "130950916006", "130950918003", "130950914005",
  "130950932002", "120750003010", "120750005002", "120750007005", "170550001008",
  "170550004007", "170550002006", "100150019008", "100150029002", "100150029001",
  "100150023007", "100150026005", "170157004001", "170157008003", "170157006002",
  "170157004003", "170157005006", "170150148010", "170150149001", "170150134004",
  "170150154011", "170150141005", "170150069008", "170150277009", "170150156001",
  "170150254001", "170150154013", "170150136005", "170150048005", "170150174006",
  "170150132009", "050250007003", "050250001007", "050250004004", "020150004004",
  "020150004005", "020150007006", "020150009003", "020150007009", "070750012007",
  "070750013010", "070750011007", "070750012008", "070750019005", "071250011006",
  "071250009002", "071250008001", "071250008006", "010150066012", "010150033004",
  "010150051007", "010150071004", "010150033005", "070150054003", "070150001008",
  "070150030011", "070150036009", "180150015009", "180150050005", "180150042006",
  "180150014001", "100250003003", "100250003005", "100250004003", "170184008005",
  "170184008004", "170184006008", "170184006009", "170184009009", "050550006004",
  "050550001006", "050550001005", "050550001004", "070750018004", "070750014005",
  "070750011003", "070750015007")

# Filtrar por los sectores seleccionados

shp_zonas <- shp_zonas %>% 
  filter(sec_anm %in% sectores_seleccionados)

# Unir por sec_anm
shp_unido <- shp_zonas %>%
  left_join(contribucion_fiscal_coordenadas, by = "sec_anm")

# Visualizar el mapa
#tmap_mode("view")  # modo interactivo con zoom
#tm_shape(shp_unido) +
#  tm_polygons(fill = "sec_anm", col = "black", fill_alpha = 0.5, 
#              palette = "Set3", n = 91)

# Seleccionar variables para mantener
shp_unido <- shp_unido %>%
  select(`COD Parroquia`, nom_par, sec_anm, `Nombre del mapa`, Latitud, Longitud, `HOGARES X SEC_ANM`, Muestra, `Código encuestador`, Encuestador, geom)

# Verifica que sea objeto sf
if (!inherits(shp_unido, "sf")) {
  shp_unido <- st_as_sf(shp_unido)
}

# Transformar a latitud/longitud (EPSG:4326)
shp_unido <- st_transform(shp_unido, crs = 4326)

# Extraer latitud y longitud desde la geometría
coords <- st_coordinates(st_centroid(shp_unido))
shp_unido$Longitud <- coords[,1]
shp_unido$Latitud <- coords[,2]

# Agregar coordenadas SOLO donde Latitud o Longitud están en NA
shp_unido <- shp_unido %>%
  mutate(
    Latitud = ifelse(is.na(Latitud), coords[, "Y"], Latitud),
    Longitud = ifelse(is.na(Longitud), coords[, "X"], Longitud)
  )

# Guardar el archivo formato gpkg
st_write(shp_unido, "Otros/contribucion_fiscal_sectores_v2.gpkg")  # GeoPackage

# Guardar en Excel
# Quitar la variable geometría
#shp_unido_sin_geom <- shp_unido %>% st_drop_geometry()
#write.xlsx(shp_unido_sin_geom, "Otros/contribucion_fiscal_coordenadas_v3.xlsx")

# Exportar como .kml
# Datos en WGS84
#shp_unido_wgs84 <- st_transform(shp_unido, crs = 4326)
#st_write(shp_unido_wgs84, "Otros/contribucion_fiscal_sectores_v3.kml", driver = "KML")

# Limpieza del entorno 
rm(list = ls())


## Reemplazo Huaquillas

#-3.4838841129705305, -80.2183013323778
#-3.479699470125414, -80.22958291910193
#-3.477984392487183, -80.23493819505605
#-3.4816977478022513, -80.2186358

# 1. Crear el data.frame con coordenadas
coords <- data.frame(
  lon = c(-80.2183013, -80.2295829, -80.2349382, -80.2186358),
  lat = c(-3.4838841, -3.4796995, -3.4779844, -3.4816977)
)

# 2. Convertir a sf con CRS WGS84 (EPSG:4326)
puntos <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

# 3. Asegurarse que ambos objetos estén en el mismo CRS
puntos <- st_transform(puntos, st_crs(shp_zonas))

# 4. Unir a los polígonos que los contienen
puntos_con_zona <- st_join(puntos, shp_zonas, join = st_within)

# 5. Ver resultado
print(puntos_con_zona)
