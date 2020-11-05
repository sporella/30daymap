# #30DayMapChallenge
# Día 5: azul
# Fracción de nubosidad
# Fuente datos: https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MODAL2_M_CLD_FR&year=2020
# Autora: Stephanie Orellana (@sporella)

library(stars)
library(ggplot2)

clouds <- read_stars("data/MODAL2_M_CLD_FR_2020-10-01_rgb_3600x1800.FLOAT.TIFF",
                     NA_value = 99999)


p <- ggplot() +
  geom_stars(data = clouds) +
  scale_fill_gradient2(
    low = "navy",
    high = "white",
    mid = "steelblue1",
    midpoint = 0.55,
    breaks = seq(0, 1, 0.2),
    na.value = "transparent"
  ) +
  labs(
    fill = "",
    tag = "FRACCIÓN DE NUBOSIDAD\nOCTUBRE 2020",
    caption = "@sporella",
    y = "",
    x = ""
  ) +
  theme(
    plot.margin = unit(c(0.1, 0, 0.6, 0), "mm"),
    legend.position = "none",
    plot.caption = element_text(size = 3, vjust = 1),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 4),
    plot.caption.position = "plot",
    panel.background = element_rect(fill = NA),
    axis.text = element_blank(),
    text = element_text(face = "bold", size = 4)
  ) +
  coord_sf(crs = 4326)

ggsave(
  "plots/5_nubes.png",
  plot = p,
  device = "png",
  height = 3,
  width = 6,
  bg = "white"
)
