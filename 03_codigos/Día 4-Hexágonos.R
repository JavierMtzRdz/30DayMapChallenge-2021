##################################################################
##       Proyecto: 30DayMapChallenge 2021: Día 4-Hexágonos      ##
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
               sf, rgdal, gganimate)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

# Cargar datos ----
## Cargar mapa municipal ----
municipios <- st_read("01_datos_brutos/muni_2018gw/muni_2018gw.shp") %>% 
  clean_names() %>% 
  st_transform(4326) 

## Cargar datos de saquías ----
sequias <- read_excel("01_datos_brutos/MunicipiosSequia.xlsx") %>% 
  clean_names() %>% 
  select(cve_concatenada,
         starts_with("x")) %>% 
  pivot_longer(-cve_concatenada,
               values_to = "sequia",
               names_to = "fecha") %>% 
  mutate(fecha = str_remove(fecha, "x"),
         fecha = as.Date(as.numeric(fecha), origin = '1899-12-30'),
         year = year(fecha),
         dia = substr(fecha, 9, 10)) %>% 
  filter(year == 2021,
         dia > 20) %>% 
  mutate(sequia = ifelse(is.na(sequia),
                         "Sin sequía",
                         sequia),
         sequia = factor(sequia,
                         levels = rev(c("Sin sequía",
                                    "D0",
                                    "D1",
                                    "D2",
                                    "D3",
                                    "D4"))))

## Generar hexagonos ----
hex <- st_make_grid(municipios, square = F, n = c(120, 120)) %>% 
  st_as_sf() %>% 
  mutate(num = row_number())

hex_points <- hex %>% 
  st_centroid()

map_points_mun <- st_join(hex_points, municipios) %>% 
  filter(!is.na(cvegeo)) %>% 
  st_set_geometry(NULL) %>% 
  select(num, cvegeo)

map_hex <- hex %>% 
  right_join(map_points_mun, 
             by = "num")

write_sf(map_hex, "04_datos_generados/mexico hex/map_hex.shp")

map_hex_edo <- map_hex %>% 
  mutate(cve_edo = substr(cvegeo, 1,2)) %>% 
  count(cve_edo)


# Generar mapa ----  
map_hex %>% 
  left_join(sequias %>% 
              filter(fecha == last(fecha)), 
            by = c("cvegeo" = "cve_concatenada")) %>% 
  ggplot() +
  geom_sf(aes(fill = sequia),
          size = 0.1,
          color = "grey35") +
  geom_sf(data = map_hex_edo,
          fill = "transparent",
          size = 0.3,
          color = "grey15") +
  labs(
    title = "Sequía en México",
    subtitle = paste0("Información al ",
                      format(as.Date("2021-10-15"), "%d de %B de %Y")),
    caption = "Elaboración con datos de CONAGUA, Monitor de Sequía de México. | @javiermtzrd"
  ) +
  coord_sf() +
  # theme(legend.position = c(0.85, 0.8),
  #       panel.background = element_rect(fill = "grey97",
  #                                       color = "transparent")) +
  scale_fill_manual(
    values = rcartocolor::carto_pal(6, "RedOr"),
    breaks = c("Sin sequía",
               "D0",
               "D1",
               "D2",
               "D3",
               "D4"),
    labels = c("Sin sequía" = "Sin sequía",
               "D0" = "Anormalmente seco",
               "D1" = "Sequía moderada",
               "D2" = "Sequía severa",
               "D3" = "Sequía extrema",
               "D4" = "Sequía excepcional"),
    name = "Intensidad de la sequía",
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"), 
      keywidth = unit(5, units = "mm"),
      title.position = 'top',
      title.hjust = 0,
      label.hjust = 0,
      ncol = 1,
      byrow = F,
      reverse = F,
      label.position = "right"
    )
  ) +
  theme_void() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.2, 0.23),
    plot.title = element_text(hjust = 0.9, 
                              size = 18, 
                              margin = margin(b = -40),
                              face = "bold", 
                              color = "grey20"),
    plot.subtitle = element_text(hjust = 0.9,
                                 size = 14,
                                 vjust = -15,
                                 # margin = margin(b = -100),
                                 color = "gray50"),
    plot.caption =  element_text(color = "gray50",
                                 size = 10, 
                                 hjust = 0,
                                 margin = margin(t = -10)),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank()
    )

ggsave(paste0("02_graficas/",
              "30DMC-Día-4-Hexágonos-Sequía-Mx",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 127,
       units = "mm")

