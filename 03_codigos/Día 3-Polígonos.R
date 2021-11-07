##################################################################
##       Proyecto: 30DayMapChallenge 2021: Día 3-Polígonos      ##
##################################################################
##
## Descripción:    Este script con las curvas de nivel y ríos de
##                 México
##
## Autor:          Javier Mtz.  
##
## Fecha creac.:   2021-11-03
##
## Email:          javier.mtz.rd@gmail.com
##
## ---------------------------
## Notas:          
## ---------------------------

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales, mexicoR,
               sf, rgdal, ggrepel, naniar, hablar, lwgeom)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Cargar datos ----
## Cargar distritos electorales ----
distritos <- st_read("01_datos_brutos/electoral_2020/distritos_simp.shp") %>%
  clean_names() %>%
  st_transform("+init=epsg:4326") %>% 
  sf::st_make_valid()

## Generar capa con shapefile de la entidad ----
entidad <- distritos %>% 
  count(entidad)

## Cargar datos electorales por distrito ----
res_comp_dist_2021 <- read_excel("04_datos_generados/res_comp_dist_2021.xlsx")

# Cargar datos de localidades amanzanadas ----

localidades <- st_read("01_datos_brutos/loc 2020/00l.shp") %>% 
  clean_names() %>% 
  st_transform("+init=epsg:4326") %>% 
  sf::st_make_valid()

sf::sf_use_s2(FALSE)

localidades$geometry <- localidades$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

# Crusar localidades urbanos con distritos ----

distritos_loc <- st_join(localidades, distritos) %>% 
  count(entidad, distrito_f)

# Datos geográficos con computos 2021 ----
## Datos con mapa dacimétrico ----
distritos_21_urb <- distritos_loc %>%
  full_join(res_comp_dist_2021 %>%
              filter(ganador == "Ganador"),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito"))

## Datos con mapa cloroplectico ----
distritos_21 <- distritos %>%
  full_join(res_comp_dist_2021 %>%
              filter(ganador == "Ganador"),
            by = c("entidad" = "id_estado",
                   "distrito_f" = "id_distrito"))


# Generar mapa nacional dacimétrico -----
distritos_21 %>% 
  ggplot() +
  geom_sf(data = distritos_21_urb,
          mapping = aes(fill = participación),
          size = 0.01,
          color = "grey55") +
  geom_sf(aes(fill = participación),
          size = 0.2,
          alpha = 0.1,
          color = "grey35") +
  geom_sf(data = entidad,
          fill = "transparent",
          size = 0.3,
          color = "grey15") +
  theme_void() +
  theme(legend.position = c(0.85, 0.75),
        panel.background = element_rect(fill = "grey97",
                                        color = "transparent")) +
  scale_fill_gradient(low = "grey75",
                      high = '#0e9a9d',
                      na.value = "white",
                      name = 'Participación \nelectoral 2021',
                      labels = percent_format(accuracy = 1L),
                      # trans = "log",
                      guide = guide_colourbar(direction = "horizontal",
                                              barheight = unit(3, 
                                                               units = "mm"),
                                              barwidth = unit(40, 
                                                              units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              label.position = "bottom",
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))


# Generar mapa dacimétrico del centro de México ----
distritos_21 %>% 
  ggplot() +
  geom_sf(data = distritos_21_urb,
          mapping = aes(fill = participación),
          size = 0.01,
          color = "grey55") +
  geom_sf(aes(fill = participación),
          size = 0.2,
          alpha = 0.1,
          color = "grey35") +
  geom_sf(data = entidad,
          fill = "transparent",
          size = 0.3,
          color = "grey15") +
  ylim(c(15.8, 20)) +
  xlim(c(-100.8, -94.2)) +
  theme_void() +
  theme(legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "grey97",
                                        color = "transparent")) +
  scale_fill_gradient(low = "grey75",
                      high = '#0e9a9d',
                      na.value = "white",
                      name = 'Participación \nelectoral 2021',
                      labels = percent_format(accuracy = 1L),
                      # trans = "log",
                      guide = guide_colourbar(direction = "horizontal",
                                              barheight = unit(3, 
                                                               units = "mm"),
                                              barwidth = unit(40, 
                                                              units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              label.position = "bottom",
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))

ggsave(paste0("02_graficas/",
              "30DMC-Día-3-Polígono-Dasimétrico-CentroMx",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 110,
       units = "mm")

# Generar mapa cloropléctico del centro de México ----
distritos_21 %>% 
  ggplot() +
  geom_sf(aes(fill = participación),
          size = 0.2,
          color = "grey35") +
  geom_sf(data = entidad,
          fill = "transparent",
          size = 0.3,
          color = "grey15") +
  ylim(c(15.8, 20)) +
  xlim(c(-100.8, -94.2)) +
  theme_void() +
  theme(legend.position = c(0.85, 0.8),
        panel.background = element_rect(fill = "grey97",
                                        color = "transparent")) +
  scale_fill_gradient(low = "grey75",
                      high = '#0e9a9d',
                      na.value = "white",
                      name = 'Participación \nelectoral 2021',
                      labels = percent_format(accuracy = 1L),
                      # trans = "log",
                      guide = guide_colourbar(direction = "horizontal",
                                              barheight = unit(3, 
                                                               units = "mm"),
                                              barwidth = unit(40, 
                                                              units = "mm"),
                                              draw.ulim = F,
                                              title.position = 'top',
                                              label.position = "bottom",
                                              title.hjust = 0.5,
                                              label.hjust = 0.5))

ggsave(paste0("02_graficas/",
              "30DMC-Día-3-Polígono-Cloropléctico-CentroMx",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 110,
       units = "mm")
