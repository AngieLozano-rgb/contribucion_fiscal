
valores_invalidos <- c(999, 9999, 99999, 999999)

data_hogar <- data %>%
  group_by(instanceID) %>%
  mutate(
    # Ingreso ind4: si todos son inválidos, poner 0; si hay válidos, sumarlos
    ingreso_ind4 = if (all(ind4 %in% valores_invalidos | is.na(ind4))) {
      0
    } else {
      sum(coalesce(if_else(ind4 %in% valores_invalidos, NA_real_, ind4), 0), na.rm = TRUE)
    },
    
    # Ingreso dep3: misma lógica
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

# 11 casos no validos (7 extremos y 8 sin datos)
# 5 graves
# 33 moderados

table(data_hogar$clasificacion_exceso)
summary(data_hogar$exceso_porcentaje)
quantile(data_hogar$exceso_porcentaje, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Total de encuestas por encuestador
total_por_encuestador <- data_hogar %>%
  group_by(id_encuestador) %>%
  summarise(total_encuestas = n(), .groups = "drop")

# Total en categorías específicas por encuestador
tabla_exceso_encuestador <- data_hogar %>%
  filter(clasificacion_exceso %in% c("Grave (100–500%)", "Extremo (>500%)", "No disponible")) %>%
  group_by(id_encuestador, clasificacion_exceso) %>%
  summarise(total = n(), .groups = "drop")

# Unir ambas tablas y calcular el porcentaje
tabla_exceso_encuestador <- tabla_exceso_encuestador %>%
  left_join(total_por_encuestador, by = "id_encuestador") %>%
  mutate(porcentaje = round((total / total_encuestas) * 100, 1))


fechas_exceso <- data_hogar %>%
  filter(clasificacion_exceso %in% c("Grave (100–500%)", "Extremo (>500%)", "No disponible")) %>%
  select(instanceID, id_encuestador, end, clasificacion_exceso) %>%
  arrange(id_encuestador, end)

data_hogar <- data_hogar %>%
  rowwise() %>%
  mutate(across(
    all_of(variables_gastos),
    ~ (. / gasto_total) * 100,
    .names = "pct_{.col}"
  )) %>%
  ungroup()

categoria_gastos <- data_hogar %>%
  summarise(across(starts_with("pct_"), ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "categoria",
               values_to = "porcentaje_promedio") %>%
  arrange(desc(porcentaje_promedio))


ggplot(data_hogar, aes(x = tamano_hogar)) +
  geom_point(aes(y = ingreso_total, color = "Ingreso")) +
  geom_point(aes(y = gasto_total, color = "Gasto")) +
  labs(
    title = "Ingreso vs Gasto mensual según tamaño del hogar",
    x = "Tamaño del hogar",
    y = "Monto mensual",
    color = ""
  ) +
  theme_minimal()


# Calcular suma por hogar solo con las variables definidas
data_hogar <- data_hogar %>%
  mutate(across(all_of(variablesgastos), ~ ifelse(. %in% valores_invalidos, NA, .))) %>%
  group_by(instanceID) %>%
  summarise(
    total_gastos_hogar = sum(
      rowSums(across(all_of(variablesgastos)), na.rm = TRUE),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

criterios_inclusion <- piloto %>%
  select(all_of(seccion_a), tiempo_residencia) %>%
  tbl_summary(
    by = NULL, # Sin desagregación
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    missing = "ifany", # Muestra valores perdidos si existen
    digits = all_continuous() ~ 1
  ) %>%
  modify_caption("**Tabla de Estadística Descriptiva**")

# E. Ingresos del hogar

# Ingreso independiente mensual

data %>%
  filter(!ind4 %in% c(9999, 99999)) %>%  
  summarise(
    Min = min(ind4, na.rm = TRUE),
    Q1 = quantile(ind4, 0.25, na.rm = TRUE),
    Mediana = median(ind4, na.rm = TRUE),
    Media = mean(ind4, na.rm = TRUE),
    Q3 = quantile(ind4, 0.75, na.rm = TRUE),
    Max = max(ind4, na.rm = TRUE),
    Desviación_std = sd(ind4, na.rm = TRUE)
  )

# Ingreso dependiente mensual
data %>%
  filter(!dep3 %in% c(9999)) %>%  
  summarise(
    Min = min(dep3, na.rm = TRUE),
    Q1 = quantile(dep3, 0.25, na.rm = TRUE),
    Mediana = median(dep3, na.rm = TRUE),
    Media = mean(dep3, na.rm = TRUE),
    Q3 = quantile(dep3, 0.75, na.rm = TRUE),
    Max = max(dep3, na.rm = TRUE),
    Desviación_std = sd(dep3, na.rm = TRUE)
  )

ggplot(data %>% filter(!ind4 %in% c(9999, 99999)), aes(y = ind4)) +
  geom_boxplot() +
  labs(title = "Distribución del Ingreso Trabajador Independiente", y = "Ingreso (USD)") +
  theme_minimal()

ggplot(data %>% filter(!dep3 %in% c(9999)), aes(y = ind4)) +
  geom_boxplot() +
  labs(title = "Distribución del Ingreso Trabajador Dependiente", y = "Ingreso (USD)") +
  theme_minimal()

#
data <- data %>%
  mutate(
    ind4 = as.numeric(as.character(ind4)),
    dep3 = as.numeric(as.character(dep3)) )

data_hogar <- data %>%
  group_by(instanceID) %>%
  summarise(
    ingreso_ind4 = sum(coalesce(ind4, 0)),
    ingreso_dep3 = sum(coalesce(dep3, 0))
  )

cols_a <- c("e_a1", "e_a2", "e_a3", "e_a4")
cols_b <- c("e_b1", "e_b2", "e_b3")
cols_t <- c("e_t1", "e_t2", "e_t4")
cols_v <- c("e_v3", "e_v14", "e_v15", "e_v16", "e_v17", "e_v18")
cols_otros <- c("e_s3", "e_e1", "e_o1", "e_o2", "e_o3")

data_hogar <- data_hogar %>%
  mutate(across(
    all_of(c(cols_a, cols_b, cols_t, cols_v, cols_otros)),
    ~ as.numeric(as.character(.))
  ))

data_hogar <- data_hogar %>%
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

table(data_hogar$total_alimentacion)
table(data_hogar$total_bebidas)
table(data_hogar$total_transporte)
table(data_hogar$total_vivienda)
table(data_hogar$total_gastos_mensuales)

variables_gasto <- c( "e_a1", "e_a2", "e_a3", "e_a4","e_b1", "e_b2", "e_b3", 
                          "e_t1", "e_t2", "e_t4", "e_v2", "e_v3","e_v12", 
                          "e_v14", "e_v15", "e_v16", "e_v17", "e_v18", 
                          "e_s1", "e_s2","e_s3","e_e1", 
                          "e_o1", "e_o2", "e_o3", "e_o4", "e_o5", 
                          "e_ah1","e_d1", "e_r1_4", "e_reg1")

stats_gasto <- data %>%
  summarise(across(all_of(variables_gasto), 
                   list(Min = ~ min(if_else(. %in% c(9999), NA_real_, .), na.rm = TRUE),
                        Q1 = ~ quantile(if_else(. %in% c(9999), NA_real_, .), 0.25, na.rm = TRUE),
                        Mediana = ~ median(if_else(. %in% c(9999), NA_real_, .), na.rm = TRUE),
                        Media = ~ mean(if_else(. %in% c(9999), NA_real_, .), na.rm = TRUE),
                        Q3 = ~ quantile(if_else(. %in% c(9999), NA_real_, .), 0.75, na.rm = TRUE),
                        Max = ~ max(if_else(. %in% c(9999), NA_real_, .), na.rm = TRUE),
                        SD = ~ sd(if_else(. %in% c(9999), NA_real_, .), na.rm = TRUE)), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", ".value"), names_sep = "_") %>%
  arrange(Variable)

 stats_gasto %>%
  gt() %>%
  tab_header(title = "Resumen Estadístico de Gastos del Hogar")

# Contar cuántas veces aparece 999 en cada variable
conteo_9999 <- data %>%
  summarise(across(everything(), ~ sum(. == 9999, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "conteo") %>%
  arrange(desc(conteo))  # Ordenar de mayor a menor

conteo_outliers <- data_hogar %>%
  summarise(across(starts_with("ale_outlier_"), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "conteo") %>%
  arrange(desc(conteo))

conteo_999 %>%
  gt() %>%
  tab_header(title = "Resumen Estadístico PREFIEREN NO RESPONDER")
