# #30DayMapChallenge
# Día 27: datos pequeños o grandes
# Equipamiento Cultural
# Datos: https://ideocuc-ocuc.hub.arcgis.com/datasets/d8c16ab7eee04de892de5696d40c94f7_0
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(ggplot2)
library(ggmap)

cultura <- read_sf("https://opendata.arcgis.com/datasets/d8c16ab7eee04de892de5696d40c94f7_0.geojson")

# Obtener mapa base -------------------------------------------------------
bb <- st_bbox(cultura) + c(-0.08, -0.02, +0.08, +0.02)
names(bb) <- c("left", "bottom", "right", "top")
basemap <- get_map(location = bb, zoom = 12, maptype = "roadmap", force = T)

colores <- c("#c1242e",
             "#ffb144",
             "#956400",
             "#007212",
             "#e4b4ff",
             "#4a0440",
             "#e958ba")

# Tema --------------------------------------------------------------------

theme_travel <- function(back_colour = "#420b41") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.title = element_blank(),
    text = element_text(size = 8, colour = "cadetblue"),
    legend.text = element_text(size = 6),
    plot.title.position = "plot",
    plot.title =  element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 7, vjust = 2, hjust = 0.5),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 6, vjust = 2),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = "transparent"),
    legend.key = element_blank(),
    legend.key.height = unit(2, "mm")
  )
}


# Visualización -----------------------------------------------------------

p <- ggmap(basemap, darken = c(0.4, "#c8fadb")) +
  geom_sf(
    data = cultura,
    aes(colour = tipo),
    inherit.aes = F,
    size = 2.5,
    alpha = 0.6
  ) +
  scale_colour_manual(values = colores) +
  labs(
    title = "EQUIPAMIENTO CULTURAL",
    subtitle = "SANTIAGO, CHILE",
    caption = "@sporella",
    colour = ""
  ) +
  guides(colour = guide_legend(nrow = 2, )) +
  theme_travel(back_colour = "#c8fadb")

ggsave("plots/27_cultura.png", p, width = 6, height = 6, bg = "#c8fadb")

