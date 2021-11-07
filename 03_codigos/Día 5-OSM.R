##################################################################
##      Proyecto: 30DayMapChallenge 2021: Día 5-OpenStreetMap    ##
##################################################################
##
## Descripción:    Mapa de la Ciudad de México con OpenStretMap
##                 
## Descripción:    https://gist.github.com/leeolney3/95cc3418cc719f1e89911d921b98873d
##
## Autor:          Javier Mtz.  
##
## Fecha creac.:   2021-11-05
##
## Email:          javier.mtz.rd@gmail.com
##
## ---------------------------
## Notas:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales, mexicoR,
               sf, rgdal, osmdata)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Funciones relevantes -----
c_pol_crs <- function(.x, crs = 4326) {
  # st_crs(.x$osm_polygons) <- st_crs(crs)
  
  for (i in names(.x[map_lgl(.x, function(x){(any(class(x) %in% "sf"))})])) {
    
    st_crs(.x[[i]]) <- st_crs(crs)
    
  }
  
  return(.x)
}

# Cargar datos de OpenStreetMap ----
park <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% 
  c_pol_crs()

water <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("water",
                                             "glacier",
                                             "bay",
                                             "strait",
                                             "cape",
                                             "spring",
                                             "hot_spring")) %>%
  osmdata_sf() %>% 
  c_pol_crs()

trees <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("tree",
                                             "tree_row",
                                             "heath",
                                             "scrub",
                                             "grassland",
                                             "fell")) %>%
  osmdata_sf() %>% 
  c_pol_crs()

reserve <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = "nature_reserve") %>%
  osmdata_sf() %>% 
  c_pol_crs()

# streets data
big_streets <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf() %>% 
  c_pol_crs()

med_streets <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf() %>% 
  c_pol_crs()


small_streets <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf() %>% 
  c_pol_crs()

# river data
river <- getbb("Ciudad de México") %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf() %>% 
  c_pol_crs()

# Generar un área general ----
area <- big_streets$osm_lines %>% 
  st_coordinates() %>%
    data.frame() %>%
    transmute(x_lon = X,
              y_lat = Y)

# Generar mapa ----
ggplot() +
  geom_sf(data = reserve$osm_polygons,
          mapping = aes(color = "Reservas", 
                        fill = "Reservas"),
          alpha = .8,
          inherit.aes = FALSE) +
  geom_sf(data = trees$osm_polygons,
          mapping = aes(color = "Áreas verdes",
                        fill = "Áreas verdes"),
          alpha = .8,
          inherit.aes = FALSE) +
  geom_sf(data = park$osm_polygons,
          mapping = aes(color = "Parques",
                        fill = "Parques"),
          alpha = .8,
          inherit.aes = FALSE) +
          # color = "#c1d033", fill="#c1d033") +
  geom_sf(data = water$osm_lines,
          mapping = aes(color = "Cuerpos de agua",
                        fill = "Cuerpos de agua"),
          inherit.aes = FALSE,
          # color = "steelblue",
          size = .8,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          mapping = aes(color = "Ríos",
                        fill = "Ríos"),
          inherit.aes = FALSE,
          # color = "steelblue",
          size = .8,
          alpha = .6) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          alpha = .4) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  ylim(c(min(area$y_lat) + 0.2,
         max(area$y_lat) - 0.05)) +
  xlim(c(min(area$x_lon) + 0.1,
         max(area$x_lon) - 0.05)) +
  theme_void(base_family = "Lato") +
  theme(text = element_text(size = 30),
        plot.title = element_text(size = 40,face = "bold", hjust = .5),
        plot.caption = element_text(size = 26, hjust = .5),
        legend.position = "top",
        plot.margin = margin(t = .5, r = 0, b = .5, l = 0, unit = "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.subtitle = element_markdown(size = 8, hjust = .5, 
                                         lineheight = 1.25, 
                                         margin = margin(2, 0, 5, 0))) +
  labs(title = "Ciudad de México", 
       caption = "Datos: OpenStreetMap | @javiermtzrd") +
  scale_fill_manual(
    values = c("Áreas verdes" = "#c1d033",
               "Parques" = "#99d033",
               "Reservas" = "#49991a",
               "Cuerpos de agua" = "#1a3891",
               "Ríos" = "steelblue"),
    breaks = c("Áreas verdes",
               "Parques",
               "Reservas",
               "Cuerpos de agua",
               "Ríos"),
    name = "",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"), 
      keywidth = unit(5, units = "mm"),
      title.position = 'left',
      title.hjust = 0,
      label.hjust = 0,
      nrow = 1,
      byrow = F,
      reverse = F,
      label.position = "right"
    )
  ) +
  scale_colour_manual(
    values = c("Áreas verdes" = "#c1d033",
               "Parques" = "#99d033",
               "Reservas" = "#49991a",
               "Cuerpos de agua" = "#1a3891",
               "Ríos" = "steelblue"),
    breaks = c("Áreas verdes",
               "Parques",
               "Reservas",
               "Cuerpos de agua",
               "Ríos"),
    name = "",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"), 
      keywidth = unit(5, units = "mm"),
      title.position = 'left',
      title.hjust = 0,
      label.hjust = 0,
      nrow = 1,
      byrow = F,
      reverse = F,
      label.position = "right"
    )
  )

ggsave(paste0("02_graficas/",
              "30DMC-Día-5-OSM",
              ".png"),
       bg = "transparent",
       width = 180,                  # Ancho de la gráfica
       height = 170,
       units = "mm")
