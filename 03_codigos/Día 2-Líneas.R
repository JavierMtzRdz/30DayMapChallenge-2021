##################################################################
##        Proyecto: 30DayMapChallenge 2021: Día 2-Líneas        ##
##################################################################
##
## Descripción:    Este script con las curvas de nivel y ríos de
##                 México
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
               sf, rgdal, ggrepel)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Mapa nacional ----
## Cargar datos ----
estados <- st_read("01_datos_brutos/shp estatal/ent_simp.shp")

curvas <- st_read("01_datos_brutos/cni250kgw/cni250kgw.shp") %>%
  clean_names()

principales_montanas <- tibble(nombre = c("Pico de Orizaba",
                                          "Popocatépetl",
                                          "Iztaccíhuatl"),
                               lat = c(19.0305, 19.0225, 19.1802),
                               lon = c(-97.2698, -98.6278, -98.6415),
                               elevation = c(5610, 5500, 5220))

rios <- st_read("01_datos_brutos/hidro4mgw/hidro4mgw.shp") %>%
  clean_names()

## Generar mapa nacional ----

ggplot() +
  geom_sf(data = estados,
          fill = "grey93",
          size = 0.15) +
  geom_sf(data = curvas,
          mapping = aes(color = contour),
          size = 0.08) +
  geom_sf(data = rios,
          color = '#0d47a1',
          alpha = 0.8,
          size = 0.15) +
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
                  min.segment.length = 0,
                  segment.curvature = -0.2,
                  arrow = arrow(length = unit(0.015, "npc"),
                                # type = "closed",
                                ends = "last"),
                  force = 70,
                  size = 3,
                  segment.size = 0.4,
                  segment.color = "grey25",
                  bg.color = "grey90",
                  bg.r = 0.1,
                  fontface = "bold") +
  theme_void() +
  theme(legend.position = c(0.8, 0.75),
        panel.background = element_rect(fill = "grey97",
                                        color = "transparent")) +
  scale_color_gradient(low= "grey65",
                       high = '#931323',
                       na.value = "white",
                       name ='Altitud (mts)',
                       labels = comma,
                       guide = guide_colourbar(direction = "horizontal",
                                               barheight = unit(3, 
                                                                units = "mm"),
                                               barwidth = unit(50, 
                                                               units = "mm"),
                                               draw.ulim = F,
                                               title.position = 'top',
                                               label.position = "bottom",
                                               title.hjust = 0.5,
                                               label.hjust = 0.5))

ggsave(paste0("02_graficas/",
              "30DMC-Día-2-Líneas",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 110,
       units = "mm")


# Mapa de Oaxaca ----
## Cargar datos ----
map_oaxaca_cont <- st_read("01_datos_brutos/shp estatal/ent_simp.shp") %>% 
  filter(nom_ent == "Oaxaca") 

# polig <- st_coordinates(map_oaxaca_cont) %>%
#   data.frame() %>%
#   transmute(x_lon = X,
#             y_lat = Y)

no_oax <- st_read("01_datos_brutos/shp estatal/ent_simp.shp") %>% 
  filter(nom_ent != "Oaxaca") 

list_curvas <- st_intersects(curvas,
                             map_oaxaca_cont,
                             sparse = FALSE)

curvas_oax <- curvas[list_curvas[,1],]

list_rios <- st_intersects(rios,
                             map_oaxaca_cont,
                             sparse = FALSE)

rios_oax <- rios[list_rios[,1],]


## Generar data frame de montañas más altas ----

principales_montanas_oax <- tibble(nombre = c("Cerro Nube (Quie Yelaag)",
                                          "Quiexobee"),
                               lat = c(16.2115, 16.2667),
                               lon = c(-96.1967, -96.2833),
                               elevation = c(3720, 3640))

polig <- tibble(x_lon = c(-96.3967, -96.4833,
                          -95.9967, -96.0833),
                y_lat = c(16.4115, 16.4667,
                          16.0115, 16.0667))

## Generar mapa de Oaxaca ----

ggplot() +
  geom_sf(data = map_oaxaca_cont,
          fill = "grey93",
          size = 0.20) +
  geom_sf(data = curvas_oax,
          mapping = aes(color = contour),
          size = 0.10) +
  geom_sf(data = rios_oax,
          color = '#0d47a1',
          alpha = 0.8,
          size = 0.30) +
  geom_sf(data = no_oax,
          fill = "grey97",
          size = 0.20) +
  ylim(c(min(polig$y_lat), 
         max(polig$y_lat))) +
  xlim(c(min(polig$x_lon), 
         max(polig$x_lon))) +
  # coord_sf(ylim = c(min(polig$y_lat), 
  #                   max(polig$y_lat)),
  #          xlim = c(min(polig$x_lon), 
  #                   max(polig$x_lon))) +
  geom_point(principales_montanas_oax,
             mapping = aes(x = lon,
                           y = lat),
             color = "red",
             size = 0.25) +
  geom_text_repel(principales_montanas_oax,
                  mapping = aes(x = lon,
                                y = lat,
                                label = paste0(nombre, "\n(",
                                               comma(elevation), " m)")),
                  color = "black",
                  min.segment.length = 0,
                  segment.curvature = -0.2,
                  arrow = arrow(length = unit(0.015, "npc"),
                                # type = "closed",
                                ends = "last"),
                  force = 40,
                  size = 3,
                  segment.size = 0.4,
                  segment.color = "grey25",
                  bg.color = "grey90",
                  bg.r = 0.1,
                  fontface = "bold") +
  theme_void() +
  theme(legend.position = c(0.85, 0.75),
        panel.background = element_rect(fill = "grey97",
                                        color = "transparent")) +
  scale_color_gradient(low= "grey65",
                       high = '#931323',
                       na.value = "white",
                       name='Altitud (mts)',
                       labels = comma,
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
              "30DMC-Día-2-Líneas-Oaxaca",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 110,
       units = "mm")


# Mapa de Oaxaca con acercamiento a montañas----
# Generar límites -----
dist <- 0.5

polig_zoom <- tibble(x_lon = c(-96.2833 - dist - 0.3,
                               -96.1967 + dist + 0.3),
                     y_lat = c(+16.2667 + dist,
                               +16.2115 - dist))

## Generar mapa de Oaxaca ----

ggplot() +
  geom_sf(data = map_oaxaca_cont,
          fill = "grey93",
          size = 0.20) +
  geom_sf(data = curvas_oax,
          mapping = aes(color = contour,
                        alpha = contour),
          size = 0.20) +
  geom_sf(data = rios_oax,
          color = '#0d47a1',
          alpha = 0.8,
          size = 0.30) +
  geom_sf(data = no_oax,
          fill = "grey97",
          size = 0.20) +
  ylim(c(min(polig_zoom$y_lat), 
         max(polig_zoom$y_lat))) +
  xlim(c(min(polig_zoom$x_lon), 
         max(polig_zoom$x_lon))) +
  # coord_sf(ylim = c(min(polig_zoom$y_lat), 
  #                   max(polig_zoom$y_lat)),
  #          xlim = c(min(polig_zoom$x_lon), 
  #                   max(polig_zoom$x_lon))) +
  geom_point(principales_montanas_oax,
             mapping = aes(x = lon,
                           y = lat),
             color = "red",
             size = 0.25) +
  geom_text_repel(principales_montanas_oax,
                  mapping = aes(x = lon,
                                y = lat,
                                label = paste0(nombre, "\n(",
                                               comma(elevation), " m)")),
                  color = "black",
                  min.segment.length = 0,
                  segment.curvature = -0.2,
                  arrow = arrow(length = unit(0.015, "npc"),
                                # type = "closed",
                                ends = "last"),
                  force = 40,
                  size = 3,
                  segment.size = 0.4,
                  segment.color = "grey25",
                  bg.color = "grey90",
                  bg.r = 0.1,
                  fontface = "bold") +
  theme_void() +
  theme(legend.position = c(0.85, 0.75),
        panel.background = element_rect(fill = "grey97",
                                        color = "transparent")) +
  scale_color_gradient(low= "grey65",
                       high = '#931323',
                       na.value = "white",
                       name='Altitud (mts)',
                       labels = comma,
                       trans = "log",
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
              "30DMC-Día-2-Líneas-Oaxaca-zoom",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 110,
       units = "mm")
  
  