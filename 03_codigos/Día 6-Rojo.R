##################################################################
##          Proyecto: 30DayMapChallenge 2021: Día 6-Rojo        ##
##################################################################
##
## Descripción:    Mapa de la Ciudad de México con OpenStretMap
##                 
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
               sf, rgdal, geojsonsf, gganimate)
## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)


# Cargar datos ----
## Datos de conflicto ----
load("01_datos_brutos/ucdp-ged-poly-v-1-1-shape/GEDEvent_v21_1.RData") 

GEDEvent_v21_1 <- GEDEvent_v21_1 %>% 
  filter(region == "Americas",
         country == "Mexico",
         latitude < 38) %>% 
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant", remove = F) %>% 
  mutate(longitude = ifelse(longitude == 19.25585,
                            -102.592150,
                            longitude))

## Cargar datos geográficos ----
municipal <- st_read("01_datos_brutos/00_muns/00mun.shp") %>% 
  clean_names() %>%
  st_transform("+init=epsg:4326") %>% 
  sf::st_make_valid()


estado <- municipal %>% 
  count(cve_ent)

## Mapa de latam ----
map_latam <- geojson_sf("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json") %>% 
  filter(name %in% c("United States of America",
                     "Guatemala",
                     "Belize",
                     "Costa Rica",
                     "Honduras",
                     "El Salvador",
                     "Nicaragua")) %>% 
  # filter(name %in% c("Anguilla", "Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", "Belize", "Bermuda", "Bolivia", "Brazil", "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guadeloupe", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique", "Mexico", "Montserrat", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands", "Uruguay", "Venezuela", "Virgin Islands")) %>% 
  st_transform(4326) 

# Generar mapa ----
ggplot() +
  geom_sf(data = estado,
          # mapping = aes(),
          size = 0.1,
          fill = "#afa9a6") +
  geom_sf(data = map_latam,
          # mapping = aes(),
          size = 0.5,
          fill = "#918d8a") +
  stat_density_2d(data = GEDEvent_v21_1,
                  mapping = aes(x = longitude,
                                y = latitude,
                                fill = ..level..),
                  # adjust = c(0.95, 0.95),
                  alpha = 0.65,
                  size = 0.05, 
                  colour = "grey20",
                  geom = "polygon") +
  geom_point(data = GEDEvent_v21_1,
             mapping = aes(x = longitude,
                           y = latitude,
                           size = best),
             colour = "black",
             fill = "#ab0305", 
             shape = 21,
             stroke = 0.1,
             alpha = 0.75) +
  # geom_density_2d_filled(data = GEDEvent_v21_1,
  #                        mapping = aes(x = longitude,
  #                                      y = latitude),
  #            alpha = 0.5) +
  scale_fill_gradient2(low = "#aa9d94",
                        mid = "#882721",
                        high = "#40010b",
                       midpoint = 0.023,
                        guide = "none") +
  scale_size(name = 'Fatalidades (est.)',
             labels = comma,
             range = c(1,3)) +
  labs(
    title = "**Violencia organizada en México, 1989-2020**<br><span style='color:#4d4d4d;font-size:13.5pt;'>Eventos individuales de <span style='color:#ab0305;'>violencia organizada</span></span>",
    caption = "Datos: Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP<br>Georeferenced Event Dataset”, Journal of Peace Research, vol.50, no.4, 523-532<br>Elaboración: **@javiermtzrd**"
  ) +
  theme_void() +
  theme(
    text = element_text(color = "white",
                        family = "Lato"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.2, 0.25),
    plot.title = element_markdown(hjust = 0.95, 
                              size = 18, 
                              margin = margin(b = -40,
                                              t = 20),
                              face = "bold", 
                              color = "grey20"),
    plot.subtitle = element_markdown(hjust = 0.95,
                                     size = 14,
                                     color = "gray20"),
    plot.caption =  element_markdown(color = "gray60",
                                 size = 10, 
                                 hjust = 0,
                                 margin = margin(t = -30,
                                                 l = 5)),
    # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "black", color = NA), 
    panel.background = element_rect(fill = "black", color = NA), 
    legend.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_blank()
  ) +
  ylim(c(min(GEDEvent_v21_1$latitude)  - 2,
         max(GEDEvent_v21_1$latitude)) + 2) +
  xlim(c(min(GEDEvent_v21_1$longitude) - 2,
         max(GEDEvent_v21_1$longitude) + 2)) 

ggsave(paste0("02_graficas/",
              "30DMC-Día-6-Rojo",
              ".png"),
       bg = "transparent",
       width = 200,                  # Ancho de la gráfica
       height = 140,
       units = "mm")

