# #30DayMapChallenge
# Día 16: Isla
# Rapa Nui
# Fuente datos: https://wikitravel.org/upload/shared//thumb/2/2c/Easter_Island_map.png/600px-Easter_Island_map.png
# Autora: Stephanie Orellana (@sporella)


library(sf)
library(ggplot2)
library(ggmap)
library(ggimage)
library(ggrepel)
library(cowplot)
library(magick)
library(extrafont)

loadfonts()

isla <- read_sf("data/rapanui.geojson")

moais <- read_sf("data/ahu_moais.geojson")


# Obtener mapa base -------------------------------------------------------
bb <- st_bbox(isla) + c(-0.001, -0.03, +0.001, +0.03)
names(bb) <- c("left", "bottom", "right", "top")
basemap <-
  get_stamenmap(
    bb,
    zoom = 13,
    maptype = "watercolor",
    force = T,
    color = "color"
  )

img <- "img/moai.png"

# Visualización -----------------------------------------------------------
p <- ggmap(basemap, darken = c(0.6, "lavender")) +
  geom_image(
    data = moais,
    aes(image = img, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = F,
    size = 0.04
  ) +
  geom_text_repel(
    data = moais,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    seed = 300,
    nudge_x = 0.001,
    segment.size = 0.3,
    size = 2,
    inherit.aes = F,
    colour = "brown",
    family = "Tempus Sans ITC",
    fontface = "bold",
  ) +
  labs(title = "RAPA NUI", caption = "@sporella") +
  theme_nothing() +
  theme(
    plot.title = element_text(
      family = "Tempus Sans ITC",
      size = 30,
      margin = margin(-40, 0, 0, 0),
      hjust = 0.5,
      vjust = -3,
      colour = "brown"
    ),
    plot.margin = margin(0, 0, 0, 0),
    plot.caption = element_text(
      size = 5,
      margin = margin(-10, 0, 0, 0),
      hjust = 0.99,
      colour = "brown"
    )
  ) +
  coord_sf()

norte <- image_colorize(image_read("img/norte.png"),
                        opacity = 90,
                        color = "lightcyan2")
p2 <- ggdraw() +
  draw_plot(p) +
  draw_image(norte,
             x = 0.2,
             y = -0.3,
             scale = .3)

ggsave(
  filename = "plots/16_rapanui.png",
  plot = p2,
  width = 6,
  height = 6,
  bg = "lavender"
)
