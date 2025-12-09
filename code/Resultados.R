###############################################################################
# PROYECTO: Impacto de la adopción de Rappi en los precios de vivienda en Bogotá
# AUTOR: David Florez y Daniel Hernandez
# DESCRIPCIÓN:
#   Este script implementa la estrategia empírica y genera los principales 
#   resultados del estudio, incluyendo:
#       - Estadísticas descriptivas y tablas comparativas
#       - Visualizaciones espaciales por año de tratamiento
#       - Estimaciones Difference-in-Differences con múltiples periodos
#       - Cálculo del efecto promedio del tratamiento (~3%)
#       - Figuras y tablas finales para el documento
# FECHA: 2025
###############################################################################


#Fijamos el directorio de trabajo
setwd("C:/Users/braya/OneDrive - Universidad de los Andes/Escritorio/U/8vo semestre/Economía Urbana/Trabajo final")


panel <- readRDS("data/work/panel.rds")
head(panel)


### Grafico de la evolucion del precio de las casas por año
panel_cs <- panel %>%
  mutate(
    group_type = case_when(
      treated == 1 ~ "Treated",
      neigh == 1   ~ "Neighbor",
      TRUE         ~ "Control"
    )
  ) %>% 
  mutate(log_price =log (price))



df_trends <- panel_cs %>%
  group_by(year, group_type) %>%
  summarise(
    mean_price = mean(log_price, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)

ggplot(df_trends, aes(x = year, y = mean_price, linetype = group_type)) +
  
  geom_line(size = 1) +
  
  # Línea vertical en el primer año de tratamiento (tú eliges)
  geom_vline(
    xintercept = 2015,
    linetype   = "dashed",
    size       = 0.8,
    color      = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(2013, 2015, 2017, 2019, 2021, 2023)
  ) +
  # --- Eje Y en increments de 0.2 ---
  scale_y_continuous(
    breaks = seq(
      from = 0,
      to   = max(df_trends$mean_price, na.rm = TRUE) + 0.1,
      by   = 0.2
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Control"  = "solid",
      "Neighbor" = "dashed",
      "Treated"  = "dotted"
    )
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    legend.title = element_blank(),
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  
  labs(
    x = "Year",
    y = "Log (Precio promedio)",
    title = ""
  )



###Graficas de los efectos segun el tipo de grilla

library(fixest)

model_cells <- feols(
  log(price) ~ treated + neigh | grid_id + year,
  data = panel,
  cluster = "grid_id"
)

summary(model_cells)

library(broom)
library(dplyr)

coef_df <- tidy(model_cells) %>%
  filter(term %in% c("treated", "neigh")) %>%
  mutate(
    label = case_when(
      term == "treated" ~ "Celda tratada",
      term == "neigh"   ~ "Celda vecina"
    ),
    ci_low  = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )


library(ggplot2)
ggplot(coef_df, aes(x = estimate, y = label)) +
  
  # Línea vertical en 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  
  # Intervalos de confianza
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                 height = 0.15, size = 0.9, color = "black") +
  
  # Puntos
  geom_point(size = 3, color = "black") +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(face = "bold", size = 16)
  ) +
  
  labs(
    x = "Efecto sobre log(price)",
    title = ""
  )








require(pacman)
#install.packages("devtools")
#devtools::install_github("bcallaway11/did")

p_load(did, dplyr, fixest, ggplot2)


panel_cs <- panel %>%
  mutate(
    log_price = log(price),
    G = if_else(is.na(treated_year), 0L, treated_year)
  )

panel_cs <- panel_cs %>%
  mutate(
    year = as.integer(as.character(year)),  # aseguramos que sea numérico
    G    = ifelse(G == 0, NA_integer_, G),  # never treated = NA
    log_price = log(price)
  )

att_relative <- att_gt(
  yname = "log_price",
  tname = "year",
  idname = "OBJECTID",
  gname = "G",
  control_group = "notyettreated",
  bstrap = TRUE,
  data = panel_cs,
  xformla = ~ habitaciones + banos + area_m2 + estrato_promedio +
    dist_colegio + dist_hospital + dist_parque 
)


summary(att_relative)

ggdid(att_relative)

es <- aggte(att_relative, type = "dynamic")
summary(es)

ggdid(es)

df_es <- data.frame(
  rel_time = es$egt,
  att = es$att.egt,
  se = es$se.egt
) %>%
  mutate(
    ci_low = att - 1.96*se,
    ci_high = att + 1.96*se
  )

df_es <- df_es %>% 
  mutate(
    time_label = case_when(
      rel_time == -4 ~ "4 años antes",
      rel_time == -2 ~ "2 años antes",
      rel_time == 0  ~ "Año de adopción",
      rel_time == 2  ~ "2 años después",
      rel_time >= 4  ~ "4 o más años después"
    ),
    # Orden correcto en el eje X
    time_label = factor(
      time_label,
      levels = c(
        "4 años antes",
        "2 años antes",
        "Año de adopción",
        "2 años después",
        "4 o más años después"
      )
    )
  )



# Gráfico estilo paper con Times New Roman
ggplot(df_es, aes(x = time_label, y = att)) +
  
  # Línea horizontal en 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.4) +
  
  # Intervalos de confianza
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.12, size = 0.9) +
  
  # Puntos
  geom_point(size = 3) +
  
  # Tema estilo artículo
  theme_bw(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),  
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x  = element_text(angle = 35, hjust = 1),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    plot.title   = element_text(face = "bold", family = "Times New Roman")
  ) +
  
  labs(
    x = "",
    y = "Log(precio)",
    title = ""
  )


group_effects <- aggte(att_relative, type = "group")
summary(group_effects)

############################################
###TABLA DE RESULTADOS DE LAS REGRESIONES
############################################

library(did)
library(fixest)
library(dplyr)
library(modelsummary)

# ============================================================
# 1. TWFE SIN CONTROLES
# ============================================================
twfe_1 <- feols(
  log(price) ~ treated + i(year) | OBJECTID,
  cluster = "OBJECTID",
  data = panel_cs
)
summary(twfe_1)


# ============================================================
# 2. TWFE CON CONTROLES
# ============================================================
twfe_2 <- feols(
  log(price) ~ treated + i(year)+ habitaciones + banos + area_m2 + estrato_promedio +
    dist_colegio + dist_hospital + dist_parque + dist_transmilenio
  | OBJECTID,
  cluster = "OBJECTID",
  data = panel_cs
)
summary(twfe_2)


# ============================================================
# 3. CALLAWAY–SANT’ANNA SIN CONTROLES
# ============================================================
cs_noctrl <- att_gt(
  yname = "log_price",
  tname = "year",
  idname = "OBJECTID",
  gname = "G",
  control_group = "notyettreated",
  bstrap = TRUE,
  data = panel_cs,
  xformla = ~ 1
)

es <- aggte(cs_noctrl, type = "simple")
summary(es)

# ============================================================
# 4. CALLAWAY–SANT’ANNA CON CONTROLES
# ============================================================
cs_ctrl <- att_gt(
  yname = "log_price",
  tname = "year",
  idname = "OBJECTID",
  gname = "G",
  control_group = "notyettreated",
  data = panel_cs,
  bstrap = TRUE,
  xformla = ~ habitaciones + banos + area_m2 + estrato_promedio +
    dist_colegio + dist_hospital + dist_parque + dist_transmilenio
)

es <- aggte(cs_ctrl, type = "simple")
summary(es)

