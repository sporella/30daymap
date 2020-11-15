# #30DayMapChallenge
# Día 15: Conexiones
# Pasajeros Transportados en Avión desde Chile
# Fuente datos: http://www.jac.gob.cl/estadisticas/estadisticas-historicas/
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

tahiti <- data.frame(name = "Tahiti", 
                     st_sfc(st_point(c(-149.45970, -17.65449))))

world <- ne_countries(scale = "medium", returnclass = "sf")

world_cent <- world %>%
  st_centroid(of_largest_polygon = T) %>%
  select(name) %>%
  bind_rows(tahiti) %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2])

pasajeros <- read_csv("data/trafico_aereo_desdechile2019.csv") %>%
  left_join(world_cent) %>%
  mutate(inicio_lon = world_cent$lon[world_cent$name == "Chile"],
         inicio_lat = world_cent$lat[world_cent$name == "Chile"]) %>%
  st_as_sf(crs = 4326)

theme_world <- function(back_colour = "#420b41") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    panel.grid = element_line(
      colour = "grey70",
      linetype = "dotted",
      size = 0.1
    ),
    panel.ontop = T,
    legend.position = "none",
    text = element_text(size = 5, colour = "khaki"),
    strip.background = element_rect(fill = NA),
    plot.caption.position = "plot",
    axis.ticks = element_line(colour = "grey60"),
    axis.text = element_text(colour = "grey60"),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 3.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
}

p <- ggplot() +
  geom_sf(
    data = world,
    fill = "turquoise4",
    colour = "dodgerblue3",
    alpha = 0.3,
    size = 0.1
  ) +
  geom_curve(
    data = pasajeros,
    aes(
      x = inicio_lon,
      y = inicio_lat,
      xend = lon,
      yend = lat,
      size = TOTAL
    ),
    curvature = 0.4,
    colour = "khaki",
    arrow = arrow(length = unit(0.7, "mm"), type = "closed")
  ) +
  scale_size(range = c(0.01, 0.4)) +
  labs(
    x = "",
    y = "",
    title = toupper("Transporte de pasajeros en avión desde Chile\nAÑO 2019"),
    caption = "@sporella",
    tag = "Datos: Junta de Aeronáutica Civil de Chile"
  )


p <- p + theme_world("#0b3d42")

ggsave(
  filename = "plots/15_vuelos.png",
  plot = p,
  device = "png",
  height = 3,
  width = 5,
  bg = "#0b3d42",
  dpi = 300
)
