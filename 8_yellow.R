# #30DayMapChallenge
# Día 8: amarillo
# Rutas del desierto de Atacama, Chile
# Fuente datos: Geodatos abiertos INE
# https://geoine-ine-chile.opendata.arcgis.com/datasets/1fe5a48791c0486f94c310693944b7f5_2
# https://phys.org/news/2015-05-driest-earth-hosts-life.html
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(tidyverse)
library(ggmap)
library(ggforce)

# Cargar y procesar datos -------------------------------------------------

antofagasta <- read_sf(
    "https://opendata.arcgis.com/datasets/1fe5a48791c0486f94c310693944b7f5_2.geojson"
  )

me <- read_sf("data/comunas_chile.geojson") %>% filter(Comuna == "María Elena") %>% st_transform(crs = 4326)

me_point <- me %>% st_centroid() %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])


# Descargar mapa base -----------------------------------------------------

bb <- st_bbox(antofagasta) + c(-0.5, 0,+1, 0)
names(bb) <- c("left", "bottom", "right", "top")
basemap <- get_stamenmap(
    bb,
    zoom = 8,
    maptype = "toner-lite",
    force = T,
    color = "color"
  )


# Visualización -----------------------------------------------------------

# * Descripción para la etiqueta ------------------------------------------
me_point$lab <- "En el sur de la comuna de María Elena existe un sitio en donde la humedad relativa (HR) atmosférica media es de 17,3%
y la humedad relativa del suelo a un metro de profundidad es de 14%. Este valor coincide con las mediciones de HR más bajas tomadas por el Laboratorio Científico de Marte en el cráter Gale en Marte."


p <- ggmap(basemap, darken = c(0.5, "lightyellow")) +
  geom_sf(
    data = antofagasta,
    inherit.aes = FALSE,
    size = 0.1,
    colour = "gold4"
  ) +
  geom_sf(
    data = me,
    inherit.aes = FALSE,
    fill = "darkgoldenrod",
    alpha = 0.3,
    colour = NA
  ) +
  geom_mark_circle(
    data = me_point,
    aes(
      x = lon,
      y = lat,
      description = lab,
      label = Comuna
    ),
    inherit.aes = FALSE,
    expand = unit(25, "mm"),
    label.buffer = unit(40, "mm"),
    label.fontsize = 8,
    label.colour = "gold4",
    con.colour = "grey60",
    colour = "grey60",
    
  ) +
  labs(
    title = toupper("Rutas del desierto de Atacama"),
    subtitle = toupper("Región de Antofagasta, Chile"),
    caption = "@sporella"
  ) +
  theme_void() +
  theme(
    text = element_text(
      colour = "lightgoldenrod4",
      size = 10
    )
  )


ggsave(
  filename = "plots/8_desierto.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "lightyellow1"
  
)
