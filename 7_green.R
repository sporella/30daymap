# #30DayMapChallenge
# Día 7: verde
# Áreas Verdes, Santiago de Chile
# Fuente datos: IDE Observatorio de Ciudades UC
# https://ideocuc-ocuc.hub.arcgis.com/datasets/db6ff8c6f75245e390e9a11962ba239f_0
# Autora: Stephanie Orellana (@sporella)


library(sf)
library(tidyverse)


verdes <- read_sf("https://opendata.arcgis.com/datasets/1ae5e0b957124176844f48b935fa8d2f_0.geojson")

cerros <- read_sf("https://opendata.arcgis.com/datasets/1f79763b1f2a42e494cca7362dd22298_0.geojson") %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  st_crop(st_bbox(verdes))

limx <- range(cerros$lon) + c(-0.09, +0.01)
limy <- range(cerros$lat)+ c(-0.01, +0.01)

p <- ggplot() +
  geom_sf(data = verdes,
          fill = "#3DB483",
          colour = "transparent") +
  geom_point(
    data = cerros,
    aes(x = lon, y = lat, shape = "\u1404"),
    size = 3,
    colour = "#69073d",
    alpha = 0.6
  ) +
  scale_shape_manual("", labels = toupper("Cerro Isla"), values = "\u1404") +
  labs(
    title = toupper("Áreas Verdes"),
    subtitle = toupper("Santiago de Chile"),
    caption = "@sporella"
  ) +
  theme_void() +
  theme(
    plot.margin = unit(c(5, 5, 5, 5), "mm"),
    panel.spacing = unit(0, "mm"),
    legend.position = "bottom",
    legend.justification = "left",
    plot.caption.position = "panel",
    plot.title.position = "panel",
    text = element_text(size = 8, colour = "grey60"),
    plot.title = element_text(hjust = 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10)
  ) +
  coord_sf(xlim = limx, ylim = limy)


ggsave(
  "plots/7_areasverdes.png",
  plot = p,
  device = "png",
  height = 5,
  width = 4,
  bg = "#f6fcf5"
)

