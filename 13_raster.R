# #30DayMapChallenge
# Día 13: raster
# Humedad de suelo. Octubre 2020.
# Fuente datos: https://developers.google.com/earth-engine/datasets/catalog/NASA_USDA_HSL_SMAP_soil_moisture#bands
# Autora: Stephanie Orellana (@sporella)

library(stars)
library(ggplot2)
library(rcartocolor)

sm <- read_stars("data/soil_moisture.tif")


p <- ggplot() +
  geom_stars(data = sm) +
  scale_fill_gradientn(colours = carto_pal(n = 7, "Teal"), na.value = "transparent") +
  labs(
    title = "HUMEDAD SUPERFICIAL DE SUELO ",
    subtitle = "PROMEDIO OCTUBRE 2020",
    caption = "@sporella",
    tag = "NASA-USDA SMAP Global Soil Moisture"
  ) +
  theme(
    legend.justification = "bottom",
    legend.position = "left",
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10,-80, 20, 30),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 5),
    plot.caption = element_text(size = 6),
    text = element_text(colour = "grey60")
  ) +
  guides(
    fill = guide_colourbar(
      "Humedad de Suelo [mm]",
      title.position = "top",
      direction = "horizontal",
      barheight = unit(3, "mm"),
      barwidth = unit(30, "mm"),
      title.theme = element_text(size = 8, colour = "grey60"),
      label.theme = element_text(
        size = 6,
        face = "bold",
        colour = "grey60"
      )
    )
  ) + coord_sf()

ggsave(
  "plots/13_humedadsuelo.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "grey10"
)
