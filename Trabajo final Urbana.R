###############################################################################
# PROYECTO: Impacto de la adopción de Rappi en los precios de vivienda en Bogotá
# AUTOR: David Florez y Daniel Hernandez
# DESCRIPCIÓN:
#   Este script realiza la carga, depuración y estructuración de los datos 
#   utilizados en el estudio. Incluye:
#       - Limpieza de bases de vivienda y restaurantes
#       - Construcción de la grilla espacial regular
#       - Clasificación de celdas (Treated, Neighbor, Control)
#       - Cálculo de distancias y variables geográficas
#       - Preparación final del panel para el análisis econométrico
# FECHA: 2025
###############################################################################


#Fijamos el directorio de trabajo
setwd("C:/Users/braya/OneDrive - Universidad de los Andes/Escritorio/U/8vo semestre/Economía Urbana/Trabajo final")

rm(list = ls())

library(sf)
library(dplyr)
library(osmdata)
library(purrr)
library(units)
library(ggplot2)
library(fixest)
library(stringr)
library(mapview)
library(tidyr)


set.seed(123)

###########################################################
##### Limpieza de los datos ##################################
###########################################################

casas <- st_read("data/construccion_1/construccion_1_piso_corregidas.shp")
sum(!st_is_valid(casas))

 #Ccnvertimos los poligonos a centroides 
casas_pts <- st_centroid(casas)
#mapview(casas_pts)


sector <- st_read("data/SECTOR/SECTOR.shp")

manzanas_estrato <- st_read("data/manzanas/ManzanaEstratificacion.shp")

manzanas_densidad <- readRDS("data/manzanas_con_densidad_oficial.rds")

manzanas <- manzanas_estrato %>%
  left_join(
    manzanas_densidad %>%
      mutate(
        # eliminar el 3° dígito desde el final (ese cero sobrante)
        MANCODIGO = paste0(
          substr(MANCODIGO, 1, nchar(MANCODIGO) - 3),
          substr(MANCODIGO, nchar(MANCODIGO) - 1, nchar(MANCODIGO))
        )
      ) %>%
      st_drop_geometry() %>%  
      group_by(MANCODIGO) %>%              # colapsar por ID
      summarise(DensPob = mean(DensPob, na.rm = TRUE)) %>%  
      rename(CODIGO_MAN = MANCODIGO),   # renombrar ID
    by = "CODIGO_MAN"
  )


#Calcular el estrato promedio del sector catastral según el estrato de las manzans

# Alinear CRS -------------------------------
# Deben estar en el MISMO sistema de coordenadas
sector  <- st_transform(sector,  st_crs(manzanas))

# Spatial Join: asignar el barrio a cada manzana --------------------
# Usamos st_within: cada manzana debe estar dentro de un sector
manzana_sector <- st_join(
  manzanas,
  sector %>% select(SCACODIGO),
  join = st_within,
  left = TRUE
)

# Calcular estrato y densidad promedio por barrio ----------------------
estrato_por_barrio <- manzana_sector %>%
  st_drop_geometry() %>% 
  group_by(SCACODIGO) %>% 
  summarise(
    estrato_promedio = mean(ESTRATO, na.rm = TRUE),
    DensPob = mean(DensPob, na.rm = TRUE)
  )

# Unir el resultado al shapefile de sectores ------------------------
sector_final <- sector %>%
  left_join(estrato_por_barrio, by = "SCACODIGO")


#-----------------------------------------------#
# 0. LEER SECTORES Y DEFINIR BOUNDING BOX
#-----------------------------------------------#

# Polígono aproximado de Bogotá (WGS84)
coords_lonlat <- matrix(c(
  -74.225, 4.837,
  -73.987, 4.837, 
  -73.987, 4.469, 
  -74.225, 4.469,  
  -74.225, 4.837),
  ncol = 2, byrow = TRUE)

bogota_poly <- st_sfc(st_polygon(list(coords_lonlat)), crs = 4326)
bogota_bbox <- st_bbox(bogota_poly)

#-----------------------------------------------#
# 1. DESCARGAR RESTAURANTES DESDE OSM
#-----------------------------------------------#

osm_raw <- opq(bbox = bogota_bbox) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()

# Combinar puntos y polígonos
rest_all <- bind_rows(
  # convertir polígonos a puntos (centroides)
  osm_raw$osm_polygons %>%
    mutate(geometry = st_centroid(geometry)) %>%
    select(osm_id, name, geometry),
  
  # puntos tal como vienen
  osm_raw$osm_points %>%
    select(osm_id, name, geometry)
) %>%
  filter(!is.na(name))

mapview(rest_all)

#-----------------------------------------------#
# 2. TRANSFORMAR CRS A METROS (EPSG:3116)
#-----------------------------------------------#

sector_final <- st_transform(sector_final, 3116)
rest_all      <- st_transform(rest_all, 3116)

#-----------------------------------------------#
# 3. ASIGNAR RESTAURANTES A CADA BARRIO (SPATIAL JOIN)
#-----------------------------------------------#

rest_barrio <- st_join(
  rest_all,
  sector_final %>% select(SCACODIGO),
  join = st_within
)

#-----------------------------------------------#
# 4. CONTAR RESTAURANTES POR BARRIO
#-----------------------------------------------#

rest_count <- rest_barrio %>%
  st_drop_geometry() %>%
  group_by(SCACODIGO) %>%
  summarise(n_restaurantes = n())

#-----------------------------------------------#
# 5. CALCULAR ÁREA DE CADA BARRIO Y DENSIDAD
#-----------------------------------------------#
sector_final <- sector_final %>%
  left_join(rest_count, by = "SCACODIGO")

sector_final <- sector_final %>%
  mutate(
    n_restaurantes = replace_na(n_restaurantes, 0),
    
    # restaurantes por cada 1000 habitantes
    rest_por_1000hab = ifelse(
      DensPob > 0,
      (n_restaurantes / DensPob) * 1000,
      NA ) )
  
    
    

##Cortamos esto para quitar las zonas rurales

localidades_urb <- st_read("data/Localidad_urbana/LOCA_URB.shp")

# Ambos al mismo CRS
sector_final <- st_transform(sector_final, 3116)
localidades_urb <- st_transform(localidades_urb, 3116)

# Join: asigna a cada sector la localidad donde cae
sectores_urbanos <- st_filter(
  sector_final,
  localidades_urb,
  .predicate = st_intersects
)
mapview(sectores_urbanos)



set.seed(123)

# 1. Índice base y tratamiento "simulado" bruto
sectores_urbanos <- sectores_urbanos %>%
  mutate(
    base_index = 0.6 * scale(estrato_promedio) +
      0.4 * scale(rest_por_1000hab) +
      rnorm(n(), 0, 0.2)
  )



# 2. Normalizar base_index a [0,1] (cuidando los NA)
library(scales)
sectores_urbanos <- sectores_urbanos %>%
  mutate(
    prob_treat = rescale(base_index, to = c(0, 1), na.rm = TRUE)
  )


# 3. Umbral (percentil 40, puedes cambiarlo)
umbral_2015 <- quantile(sectores_urbanos$prob_treat, 0.97, na.rm = TRUE)
umbral_2017 <- quantile(sectores_urbanos$prob_treat, 0.9, na.rm = TRUE)
umbral_2019 <- quantile(sectores_urbanos$prob_treat, 0.85, na.rm = TRUE)
umbral_2021 <- quantile(sectores_urbanos$prob_treat, 0.7, na.rm = TRUE)


# 4. Tratamiento final (0/1) a partir de la probabilidad
sectores_urbanos <- sectores_urbanos %>%
  mutate(
    treated_2015 = as.numeric(!is.na(prob_treat) & prob_treat > umbral_2015), 
    treated_2017 = as.numeric(!is.na(prob_treat) & prob_treat > umbral_2017),
    treated_2019 = as.numeric(!is.na(prob_treat) & prob_treat > umbral_2019),
    treated_2021 = as.numeric(!is.na(prob_treat) & prob_treat > umbral_2021)
  )


mapview(
  sectores_urbanos,
  zcol = "treated_2015",
  col.regions = c("pink", "red"),
  legend = TRUE,
  layer.name = "Tratado"
)







#####Aqui hay que graficar los mapas
library(sf)
library(ggplot2)
library(patchwork)

# Paleta como la del ejemplo
paleta <- c("0" = "#c6dbef", "1" = "#2171b5")

# -------------------------
# Función para crear cada mapa
# -------------------------
plot_treated <- function(data, year, label_letter) {
  
  varname <- paste0("treated_", year)
  
  ggplot(data) +
    geom_sf(aes(fill = factor(.data[[varname]])),
            color = "grey20",      # bordes visibles
            size = 0.05) +           # borde muy delgado
    scale_fill_manual(
      values = paleta,
      name = "Rappi",
      labels = c("0" = "No", "1" = "Sí")
    ) +
    labs(title = paste0("(", label_letter, ")  ", year)) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5) #
    )
}

# -------------------------
# Crear cada mapa
# -------------------------
map_2015 <- plot_treated(sectores_urbanos, 2015, "A")
map_2015 
map_2017 <- plot_treated(sectores_urbanos, 2017, "B")
map_2017
map_2019 <- plot_treated(sectores_urbanos, 2019, "C")
map_2019
map_2021 <- plot_treated(sectores_urbanos, 2021, "D")
map_2021







library(sf)
library(ggplot2)
library(dplyr)

# ============================================================
# Preprocesamiento: convertir 0 a NA si quieres excluirlos visualmente
# ============================================================

sectores_plot <- sectores_urbanos %>%
  mutate(
    estrato_plot = ifelse(estrato_promedio == 0, NA, estrato_promedio),
    densidad_clean = ifelse(rest_por_1000hab == 0, NA, rest_por_1000hab), 
    densidad_log = log(densidad_clean)  # log natural
  )


# ============================================================
# 1. MAPA — Estrato promedio (azul monocromático elegante)
# ============================================================

map_estrato <- ggplot(sectores_plot) +
  geom_sf(aes(fill = estrato_plot),
          color = "grey30", size = 0.05) +
  scale_fill_gradient(
    low = "#FFFFD4",     # azul muy claro
    high = "#CC4C02",    # azul profundo elegante
    na.value = "grey90",
    name = "Estrato\npromedio"
  ) +
  theme_void() +
  labs(title = "") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

map_estrato

ggsave("results/mapa_estrato.pdf",
       map_estrato,
       width = 4, height = 6, dpi = 300)




# ============================================================
# 2. MAPA DE DENSIDAD DE RESTAURANTES POR KM2
# ============================================================
map_densidad_rest <- ggplot(sectores_plot) +
  geom_sf(aes(fill = densidad_log),
          color = "grey30", size = 0.05) +
  scale_fill_gradient(
    low = "#CCFFCE",     
    high = "#075D24",    # dorado elegante
    na.value = "grey85",
    name = "Restaurantes\npor 1000 hab"
  ) +
  theme_void() +
  labs(title = "") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

map_densidad_rest

ggsave("results/mapa_densidad_rest.pdf",
       map_densidad_rest,
       width = 4, height = 6, dpi = 300)



#####################################################
###### RESTAURANTES TRATADOS Y NO TRATADOS ##########
#####################################################

rest_with_sector <- st_join(rest_all, sectores_urbanos, join = st_within)

set.seed(123)

rest_treated <- rest_with_sector %>%
  group_by(SCACODIGO) %>%   # agrupamos por sector para que la aleatoriedad sea dentro de sector
  mutate(
    
    # 2015 --------------------------------------------------
    rest_treated_2015 = case_when(
      treated_2015 == 1 ~ rbinom(n(), 1, 0.02),   # solo si el sector fue tratado en 2015
      TRUE ~ 0
    ),
    
    # 2017 --------------------------------------------------
    rest_treated_2017 = case_when(
      treated_2017 == 1 ~ rbinom(n(), 1, 0.02),   # solo si el sector fue tratado en 2017
      TRUE ~ 0
    ),
    
    # 2019 --------------------------------------------------
    rest_treated_2019 = case_when(
      treated_2019 == 1 ~ rbinom(n(), 1, 0.02),
      TRUE ~ 0
    ),
    
    # 2021 --------------------------------------------------
    rest_treated_2021 = case_when(
      treated_2021 == 1 ~ rbinom(n(), 1, 0.02),
      TRUE ~ 0
    )
  ) %>%
  ungroup()

#Mapa de sectores tratados y no tratados
mapview(
  rest_treated,
  zcol = "rest_treated_2015",
  col.regions = c("blue", "purple"),
  layer.name = "Restaurantes 2015"
) +
mapview(
  sectores_urbanos,
  zcol = "treated_2015",
  col.regions = c("pink", "red"),
  alpha.regions = 0.3,
  layer.name = "Sectores 2015"
) 


#Mapa de restaurantes tratados por año 

rest_treated_year <- rest_treated %>%
  mutate(
    treat_year = case_when(
      rest_treated_2015 == 1 ~ "2015",
      rest_treated_2017 == 1 ~ "2017",
      rest_treated_2019 == 1 ~ "2019",
      rest_treated_2021 == 1 ~ "2021",
      TRUE ~ NA_character_
    )
  )
rest_treated_only <- rest_treated_year %>%
  filter(!is.na(treat_year))


mapview(
  rest_treated_only,
  zcol = "treat_year",
  layer.name = "Restaurantes tratados"
) + mapview(
  sectores_urbanos,
  alpha.regions = 0.3,
  layer.name = "Sectores 2015"
) 



ggplot() +
  # capa 1: sectores urbanos (polígonos)
  geom_sf(data = sectores_urbanos,
          fill = "white", 
          color = "grey60",
          alpha = 0.3) +
  
  # capa 2: restaurantes tratados (puntos)
  geom_sf(data = rest_treated_only,
          aes(color = treat_year),
          size = 1,
          alpha = 0.9) +
  
  scale_color_brewer(palette = "Set1", name = "Año de tratamiento") +
  theme_void() +
  theme(
    text = element_text(family = "Times New Roman"),  
    legend.position = c(0.35, 0.80),      # más arriba y a la derecha
    legend.justification = c(1, 1),
    legend.title = element_text(size = 8),
    legend.text  = element_text(size = 7),
    legend.key.size = unit(0.25, "cm")  # iconos más pequeños
  )


ggsave("results/rest_treated_by_year.png",
       width = 4, height = 6, dpi = 300)



#-------------------------------------------------------#
# 1. CREAR GRILLA 500M + ID
#-------------------------------------------------------#

grid_500m <- st_make_grid(
  sectores_urbanos,
  cellsize = 500,     # 500 metros
  square = TRUE
) %>%
  st_sf() %>%
  st_intersection(sectores_urbanos %>% summarise()) %>%
  mutate(grid_id = row_number())

#-------------------------------------------------------#
# 2. UNIR RESTAURANTES A GRILLA
#-------------------------------------------------------#

rest_with_grid <- st_join(
  rest_treated,
  grid_500m,
  join = st_within,
  left = FALSE
)

#-------------------------------------------------------#
# 3. CALCULAR VARIABLES grid_treated_XXXX POR AÑO
#-------------------------------------------------------#

grid_treated <- rest_with_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    grid_treated_2015 = as.integer(any(rest_treated_2015 == 1)),
    grid_treated_2017 = as.integer(any(rest_treated_2017 == 1)),
    grid_treated_2019 = as.integer(any(rest_treated_2019 == 1)),
    grid_treated_2021 = as.integer(any(rest_treated_2021 == 1))
  )

# Unir estas variables a la grilla
grid_final <- grid_500m %>%
  left_join(grid_treated, by = "grid_id") %>%
  mutate(
    grid_treated_2015 = replace_na(grid_treated_2015, 0),
    grid_treated_2017 = replace_na(grid_treated_2017, 0),
    grid_treated_2019 = replace_na(grid_treated_2019, 0),
    grid_treated_2021 = replace_na(grid_treated_2021, 0)
  )

#-------------------------------------------------------#
# 4. MATRIZ DE VECINOS — Debe ser sobre grid_final
#-------------------------------------------------------#

neighbors_matrix <- st_touches(grid_final, sparse = FALSE)

#-------------------------------------------------------#
# 5. FUNCIÓN PARA CALCULAR NIVELES
#-------------------------------------------------------#

get_levels <- function(treated_vector, neighbors_matrix) {
  
  treated_cells <- which(treated_vector == 1)
  
  if (length(treated_cells) == 0) {
    return(rep("Control", length(treated_vector)))
  }
  
  neighbors_1 <- apply(neighbors_matrix[, treated_cells, drop = FALSE], 1, any)
  
  case_when(
    treated_vector == 1 ~ "Treated",
    neighbors_1 & treated_vector == 0 ~ "Neighbor",
    TRUE ~ "Control"
  )
}

#-------------------------------------------------------#
# 6. CREAR NIVELES PARA TODOS LOS AÑOS
#-------------------------------------------------------#

grid_final <- grid_final %>%
  mutate(
    level_2015 = get_levels(grid_treated_2015, neighbors_matrix),
    level_2017 = get_levels(grid_treated_2017, neighbors_matrix),
    level_2019 = get_levels(grid_treated_2019, neighbors_matrix),
    level_2021 = get_levels(grid_treated_2021, neighbors_matrix)
  )



# Filtrar SOLO restaurantes tratados 2015 (la forma correcta en mapview)
rest_2019 <- rest_treated %>%
  filter(rest_treated_2019 == 1)

# Asegurar factor para colores consistentes
grid_2019 <- grid_final %>%
  mutate(level_2019 = factor(level_2019,
                             levels = c("Treated", "Neighbor", "Control")))

# Mapview final
mapview(grid_2019,
        zcol = "level_2019",
        alpha.regions = 0.5,
        layer.name = "Grillas 2019") +
  
  mapview(rest_2019,
          col.regions = "red",
          cex = 2,
          layer.name = "Restaurantes tratados 2019")



###########################################################
###### AGREGAR INFORMACIÓN DE AMENIDADES A LAS CASAS ######
###########################################################

require("pacman")

p_load(
  tidyverse, modeldata, stargazer, broom, fixest, dplyr, extrafont, psych,
  skimr, modelsummary, kableExtra, rio, osmdata, sf, tmaptools, spatialreg,
  spdep, splm, spgwr, sp, raster, gstat, tmap, lmtest, sandwich, units,
  purrr, nngeo, patchwork, tmap, effects, RANN
)

bbox_bogota <- st_bbox(st_transform(casas_pts, 4326))

# Parques
parks_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()
parks_points <- parks_osm$osm_points

# Colegios
schools_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf()
schools_points <- schools_osm$osm_points

# Hospitales
hospitals_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()
hospitals_points <- hospitals_osm$osm_points

# Estaciones TransMilenio
transmilenio_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf()
transmilenio_points <- transmilenio_osm$osm_points


#Transformamos todo a ESPG:3116
casas_proj <- st_transform(casas_pts, 3116)
parks_proj <- st_transform(parks_points, 3116)
schools_proj <- st_transform(schools_points, 3116)
hospitals_proj <- st_transform(hospitals_points, 3116)
transmilenio_proj <- st_transform(transmilenio_points, 3116)

#Extraer coordenadas númerocas para usar nn2 

casas_mat <- st_coordinates(casas_proj)

parks_mat <- st_coordinates(parks_proj)
schools_mat <- st_coordinates(schools_proj)
hospitals_mat <- st_coordinates(hospitals_proj)
transmi_mat <- st_coordinates(transmilenio_proj)


##Calcular las distancias mínimas

dist_parques <- nn2(parks_mat, casas_mat, k = 1)$nn.dists[,1] / 1000
casas_proj$dist_parque <- dist_parques

dist_colegios <- nn2(schools_mat, casas_mat, k = 1)$nn.dists[,1] / 1000
casas_proj$dist_colegio <- dist_colegios

dist_hospital <- nn2(hospitals_mat, casas_mat, k = 1)$nn.dists[,1] / 1000
casas_proj$dist_hospital <- dist_hospital

dist_transmi <- nn2(transmi_mat, casas_mat, k = 1)$nn.dists[,1] / 1000
casas_proj$dist_transmilenio <- dist_transmi


summary(casas_proj$dist_parque)
summary(casas_proj$dist_colegio)
summary(casas_proj$dist_hospital)
summary(casas_proj$dist_transmilenio)
summary(casas_proj$dist_cbd)


###Ahora simulamos algunas varibles 

set.seed(123)


#Metros cuadrados
casas_proj$area_m2 <- rlnorm(
  n = nrow(casas_proj),
  meanlog = log(60),   # centro en 60 m2
  sdlog = 0.5          # dispersión realista
)

#Número de habitaciones
casas_proj$habitaciones <- pmax(
  1,
  round(casas_proj$area_m2 / 25 + rnorm(nrow(casas_proj), 0, 0.8))
)

#Número de baños
casas_proj$banos <- pmax(
  1,
  pmin(
    round(casas_proj$habitaciones * runif(nrow(casas_proj), 0.4, 0.7)),
    4
  )
)


##############################################################
####Ahora asignamos a cada casa la información de la grilla
###############################################################

library(dplyr)

casas_with_grid <- st_join(
  casas_proj,
  dplyr::select(grid_final, grid_id, level_2015, level_2017, level_2019, level_2021),
  join = st_intersects,
  left = TRUE
)


set.seed(123)  # para reproducibilidad

casas_with_grid <- casas_with_grid %>%
  group_by(grid_id) %>%
  sample_frac(size = 0.5) %>%   # toma el 50% de las casas de cada grid_id
  ungroup()

casas_with_grid <- casas_with_grid %>%
  mutate(
    across(starts_with("level_"), ~factor(.x, levels = c("Treated", "Neighbor", "Control")))
  )


#Agregamos información de los estratos y el sector


# Asegurar que ambos tengan el mismo CRS
casas_with_grid <- st_transform(casas_with_grid, st_crs(sectores_urbanos))


library(dplyr)
library(tidyr)

tabla_casas_categorias <- casas_with_grid %>%
  st_drop_geometry() %>% 
  dplyr::select(level_2015, level_2017, level_2019, level_2021) %>%
  pivot_longer(
    cols = everything(),
    names_to = "year",
    values_to = "category"
  ) %>%
  mutate(
    year = gsub("level_", "", year)  # limpiar nombre del año
  ) %>%
  group_by(year, category) %>%
  summarise(n_casas = n(), .groups = "drop") %>%
  arrange(year, category)

tabla_casas_categorias




# Spatial join: asigna a cada casa el sector en el que cae
casas_final <- casas_with_grid %>%
  st_join(sectores_urbanos %>% 
            dplyr:: select(SCACODIGO, estrato_promedio),
          left = TRUE)


casas_final <- casas_final %>%
  pivot_longer(
    cols = starts_with("level_"),
    names_to = "year_str",
    values_to = "group"
  ) %>%
  mutate(
    year = as.numeric(str_remove(year_str, "level_"))
  ) 


# ============================================================
#  Crear dummies de tratado, vecino, control
# ============================================================

casas_final <- casas_final %>%
  mutate(
    treated = if_else(group == "Treated", 1, 0),
    neigh   = if_else(group == "Neighbor", 1, 0),
    control = if_else(group == "Control", 1, 0)
  )

library(dplyr)
library(tidyr)

# ============================================================
# 0. Años que queremos en el panel
# ============================================================
years_full <- c(2013, 2015, 2017, 2019, 2021, 2023)

# ============================================================
# 1. Expandir el panel a todos los años SIN copiar estados de tratamiento
# ============================================================

# 1A. Crear 2013 basado en 2015 pero REINICIANDO tratamiento
duplicados_2013 <- casas_final %>%
  filter(year == 2015) %>%
  mutate(
    year     = 2013,
    treated  = 0,
    neigh    = 0,
    control  = 1,
    treated_year = NA_integer_
  )

# 1B. Crear 2023 basado en 2021 PERO SIN PROPAGAR TRATAMIENTO HACIA ATRÁS
duplicados_2023 <- casas_final %>%
  filter(year == 2021) %>%
  mutate(
    year = 2023
    # aquí SÍ se mantienen treated/neigh/control,
    # porque 2023 es posterior
  )

casas_final <- casas_final %>%
  bind_rows(duplicados_2013, duplicados_2023) %>%
  arrange(OBJECTID, year)

# ============================================================
# 2. RECALCULAR TRATAMIENTO CORRECTAMENTE
# ============================================================

dt <- as.data.table(casas_final)
setorder(dt, OBJECTID, year)

# 2A. Calcular treated_year correctamente
dt[, treated_year := {
  idx <- which(treated == 1)
  if (length(idx) == 0) NA_integer_ else year[idx[1]]
}, by = OBJECTID]

# 2B. Reconstruir treated solo hacia adelante, nunca hacia atrás
dt[, treated := ifelse(!is.na(treated_year) & year >= treated_year, 1, 0)]

# 2C. Recalcular neigh y control correctamente
dt[treated == 1, `:=`(neigh = 0, control = 0)]
dt[treated == 0 & neigh == 1, control := 0]
dt[treated == 0 & neigh == 0, control := 1]

casas_final <- as_tibble(dt)

saveRDS(casas_final, "results/casas_final.rds")

##################################################################
############# Simulamos el precio de las casas. 
##################################################################

casas_final <- readRDS("results/casas_final.rds")

library(dplyr)
library(tidyr)
library(purrr)

# ============================================================
# 0. PARÁMETROS GENERALES
# ============================================================

set.seed(123)

years <- c(2011, 2013, 2015, 2017, 2019, 2021, 2023)

beta_treated  <- 0.03   # 3% yearly premium causal
beta_neighbor <- 0.005  # efecto pequeño no significativo
beta_control  <- 0

trend <- 0.02            # tendencia general del mercado (2% anual)

# Covariables internas de vivienda
beta_area        <- 0.0008     # cada m2 aumenta ~0.08%
beta_habit       <- 0.04       # cada habitación +4%
beta_banos       <- 0.06       # cada baño +6%

# Covariables urbanas previas
beta_strata      <- 0.10
beta_dist_parque  <- -0.015
beta_dist_colegio <- -0.010
beta_dist_hosp   <- -0.008
beta_dist_transmi   <- -0.01


# ============================================================
# 1. BASE INICIAL: CASAS, SECTOR, ESTRATO, AMENIDADES
# ============================================================

base0 <- casas_final %>%
  dplyr::select(
    OBJECTID,
    grid_id,
    SCACODIGO,
    estrato_promedio,
    area_m2,
    habitaciones,
    banos,
    dist_parque,
    dist_colegio,
    dist_hospital,
    dist_transmilenio,
    treated,
    neigh, 
    year,
    treated_year
  )


# ============================================================
# 3. EFECTOS FIJOS POR SECTOR
# ============================================================

casas_nogeo <- casas_final %>% st_drop_geometry()

sector_fe <- casas_nogeo %>%
  distinct(SCACODIGO) %>%
  mutate(fe_sector = rnorm(n(), mean = 0, sd = 0.15))

casas_final <- casas_nogeo %>%
  left_join(sector_fe, by = "SCACODIGO")


# ============================================================
# 4. MODELO DE PRECIO LATENTE (LOG-LINEAL)
# ============================================================

casas_final <- casas_final %>%
  mutate(
    
    # Tendencia temporal
    trend_term = (year - min(year)) * trend,
    
    # Efectos tratamiento
    treat_effect = treated * beta_treated,
    neigh_effect = neigh * beta_neighbor,
    
    # Covariables internas
    cov_int =
      beta_area  * area_m2 +
      beta_habit * habitaciones +
      beta_banos * banos,
    
    # Covariables externas
    cov_ext =
      beta_strata * estrato_promedio +
      beta_dist_parque  * log1p(dist_parque) +
      beta_dist_colegio * log1p(dist_colegio) +
      beta_dist_transmi   * log1p(dist_transmilenio)+
      beta_dist_hosp   * log1p(dist_hospital),
    
    # Precio base inicial en 2011
    base_price = 100 + 20 * estrato_promedio + 2 * area_m2 +
      20 * habitaciones + 10 * banos +
      rnorm(n(), 0, 8),
    
    # Predictor lineal log
    mu = log(base_price) +
      trend_term +
      treat_effect +
      neigh_effect +
      cov_int +
      cov_ext +
      fe_sector   
  )

# ============================================================
# 5. ERROR HETEROCEDÁSTICO REALISTA
# ============================================================

casas_final <- casas_final %>%
  mutate(
    sigma = 0.10 + 0.03 * estrato_promedio,  # varianza ↑ con estrato
    eps = rnorm(n(), 0, sigma),
    
    price = exp(mu + eps)  # log-normal
  )

# ============================================================
# 6. RESULTADO FINAL
# ============================================================

panel <- casas_final %>%
  dplyr::select(
    OBJECTID,grid_id, year, price,
    treated, neigh, control, treated_year, 
    estrato_promedio,
    area_m2, habitaciones, banos,
    SCACODIGO, dist_transmilenio,
    dist_parque, dist_colegio, dist_hospital
  )

head(panel)

saveRDS(panel, "data/work/panel.rds")


vars <- c("price", "area_m2", "habitaciones", "banos", 
          "estrato_promedio", "dist_parque", "dist_colegio", "dist_hospital", "dist_transmilenio")



library(dplyr)
library(tidyr)
library(purrr)

# crear etiqueta de grupo
panel <- panel %>%
  mutate(group = case_when(
    treated == 1 ~ "Treated",
    neigh   == 1 ~ "Neighbor",
    control == 1 ~ "Control",
    TRUE         ~ "Other"
  ))
# === 3. Crear dataset para All ===
df_all <- casas_final %>%
  mutate(group = "All")

# === 4. Unir todo en un solo dataset ===
df_stats <- bind_rows(casas_final, df_all)

# === 5. Calcular medias, SD y N ===
stats <- df_stats %>%
  dplyr::select(all_of(vars), group) %>%
  pivot_longer(cols = all_of(vars), names_to = "variable") %>%
  group_by(variable, group) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    n    = sum(!is.na(value)),
    .groups = "drop"
  )

# === 6. Construir tabla en dos filas por variable ===
stats_2rows <- stats %>%
  mutate(
    mean_fmt = sprintf("%.3f", mean),
    sd_fmt   = sprintf("(%.3f)", sd)
  ) %>%
  dplyr::select(variable, group, mean_fmt, sd_fmt) %>%
  pivot_longer(cols = c(mean_fmt, sd_fmt), names_to = "stat", values_to = "value") %>%
  mutate(
    variable_row = if_else(stat == "mean_fmt",
                           variable,
                           paste0("   (", variable, ")"))
  ) %>%
  dplyr::select(variable_row, group, value) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  arrange(variable_row)

stats_2rows


# ============================================
# N observaciones por categoría
# ============================================

N_stats <- casas_final %>%
  mutate(group = case_when(
    treated == 1 ~ "Treated",
    neigh   == 1 ~ "Neighbor",
    control == 1 ~ "Control",
    TRUE         ~ NA_character_
  )) %>%
  summarise(
    All      = n(),
    Treated  = sum(group == "Treated", na.rm = TRUE),
    Neighbor = sum(group == "Neighbor", na.rm = TRUE),
    Control  = sum(group == "Control", na.rm = TRUE)
  )











