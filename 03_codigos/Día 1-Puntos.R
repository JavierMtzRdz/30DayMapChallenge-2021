##################################################################
##        Proyecto: 30DayMapChallenge 2021 :Día 1-Points        ##
##################################################################
##
## Descripción:    Este script genera un mapa de puntos a partir de 
##                 la altitud
##
## Autor:          Javier Mtz.  
##
## Fecha creac.:   2021-11-01
##
## Email:          javier.mtz.rd@gmail.com
##
## ---------------------------
## Notas:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales, mexicoR,
               sf, sp, rgdal, elevatr, ggrepel)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Cargar mapa de México ----
estados <- st_read("01_datos_brutos/shp estatal/ent_simp.shp")


# Generar coordenadas ----
polig <- st_coordinates(estados) %>%
  data.frame() %>%
  transmute(x_lon = X,
            y_lat = Y)

max_lon <- max(polig$x_lon)
min_lon <- min(polig$x_lon)

max_lat <- max(polig$y_lat)
min_lat <- min(polig$y_lat)

dist_point <- (max_lon-min_lon)/250

seq_lon <- seq(min_lon, max_lon, dist_point)

seq_lat <- seq(min_lat, max_lat, dist_point)

# Crear matriz de puntos ----
points <- tibble()

for (i in seq_lat) {
  
  points <- points %>% 
    bind_rows(tibble(lon = seq_lon,
                     lat = i))
}

# Crear data frame con elebaciones ----

pacman::p_load(googleway)

api_key <- "api key"

bd_elevation <- tibble()

for (i in 1:362) {
  
  if(i == 1) {ini <- i} else {ini <- ((i-1)*100)+1}
  
  fin <- i*100
  
  elevation <- google_elevation(df_locations = points[ini:fin,], key = api_key)
  
  
  bd_elevation <- bd_elevation %>% 
    bind_rows(elevation$results)
  
}
bd_elevation$lat <- bd_elevation$location$lat

bd_elevation$lon <- bd_elevation$location$lng

bd_elevation <- bd_elevation %>% 
  select(-location)

points_sf <- st_as_sf(bd_elevation,
                      coords = c("lon", "lat"),
                      crs = 4326, agr = "constant")

# Guardar shapefile ----

st_write(points_sf, "04_datos_generados/points_sf/points_sf.shp")

# Generar data frame de montañas más altas ----

principales_montanas <- tibble(nombre = c("Pico de Orizaba",
                                          "Popocatépetl",
                                          "Iztaccíhuatl"),
                               lat = c(19.0305, 19.0225, 19.1802),
                               lon = c(-97.2698, -98.6278, -98.6415),
                               elevation = c(5610, 5500, 5220))

## Generar mapa ----

points_sf %>% 
  filter(elevation >= 0) %>% 
  ggplot() +
  geom_sf(mapping = aes(size = elevation,
                        alpha = elevation),
          color = "grey10") +
  geom_sf(data = estados,
          fill = NA,
          size = 0.1,
          colour = "grey40") +
  geom_point(principales_montanas,
             mapping = aes(x = lon,
                           y = lat),
             color = "red",
             size = 0.25) +
  geom_text_repel(principales_montanas,
             mapping = aes(x = lon,
                           y = lat,
                           label = paste0(nombre, "\n(", 
                                          comma(elevation), " m)")),
             color = "black", 
             size = 3) +
  theme_void() +
  theme(legend.position = c(0.15, 0.15),
        panel.background = element_rect(fill = "grey98",
                                        color = "transparent")) +
  scale_size(name = 'Elevación (mts)',
             labels = comma,
             range = c(0,0.5)) +
    scale_alpha_continuous(name = 'Elevación (mts)',
                           labels = comma)
  
  
  ggsave(paste0("02_graficas/Mapas entidad/",
                ifelse(length(enti_nom) > 1,
                       paste(enti_nom, collapse = "_"),
                       enti_nom),
                ".png"),
         bg = "transparent",
         width = 200,                  # Ancho de la gráfica
         height =130,
         units = "mm")


