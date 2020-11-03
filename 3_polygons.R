# #30DayMapChallenge
# Día 3: polígonos
# Carta de Inundación por Tsunami
# Datos: http://www.geoportal.cl/geoportal/catalog/search/resource/resumen.page?uuid=%7B5FA8762F-DFAB-4CC7-9155-CC2012D7BF8A%7D
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(tidyverse)
library(ggmap)


# Cargar y procesar datos -------------------------------------------------
tsu <- read_sf("data/tsunami_pichidangui.geojson") %>% 
  mutate(lab = str_replace(Name, "Profundidad de la inundación: ", ""))

mar <- read_sf("data/mar.geojson")


# Obtener mapa base -------------------------------------------------------
bb <- st_bbox(tsu)
names(bb) <- c("left", "bottom", "right", "top")
basemap <- get_stamenmap(bb, zoom = 14, maptype = "toner-lite", force = T, color = "color")


# Visualización -----------------------------------------------------------
p <- ggmap(basemap, darken = c(0.2, "palegoldenrod"))+
  geom_sf(data = mar, inherit.aes = FALSE, fill = "lightcyan")+
  geom_sf(data = tsu, aes(fill = lab), inherit.aes = FALSE, alpha = 0.6, size = 0.1) +
  scale_fill_viridis_d(option = "D", direction = -1)+
  labs(
    fill = toupper("Profundidad de la Inundación"),
    x = "",
    y = "",
    title = toupper("Carta de Inundación por Tsunami"),
    subtitle = toupper("Pichidangui, Chile"),
    caption = "@sporella"
  ) +
  theme(
    legend.justification = "bottom",
    plot.caption.position = "plot",
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(size = 0.3),
    panel.ontop = TRUE,
    axis.text = element_text(size = 6),
    text = element_text(face = "bold")
  ) +
  guides(
    fill = guide_legend(
      label.position = "bottom",
      title.position = "left",
      title.theme = element_text(angle = 90, size = 7, face="bold"),
      keywidth = unit(2, "mm"),
      keyheight = unit(3, "mm"),
      override.aes = list(size = 1)
    )
  )

ggsave("plots/3_tsunami.png", plot = p, device = "png", width = 6, height = 6)
