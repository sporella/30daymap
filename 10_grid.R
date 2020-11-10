# #30DayMapChallenge
# Día 10: grilla
# Paradas Transantiago
# Fuente datos: IDE Observatorio de Ciudades UC
# https://ideocuc-ocuc.hub.arcgis.com/datasets/2fc8b3d204254ff181cc3f64dc407d65_0?geometry=-71.734%2C-33.694%2C-69.648%2C-33.293
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(stars)
library(ggplot2)
library(rcartocolor)

comunas <- st_read("data/comunas_metropolitana.geojson") %>%
  st_transform(32719)

points <- st_read(
  "https://opendata.arcgis.com/datasets/2fc8b3d204254ff181cc3f64dc407d65_0.geojson"
) %>% st_transform(32719)

points$count <- 1
template <- st_as_stars(st_bbox(points),
              dx = 1000,
              dy = 1000,
              values = -1)
s <- st_rasterize(
    points[, "count"],
    template = template,
    options = c("MERGE_ALG=ADD", "ALL_TOUCHED=TRUE")
  )

s[s == -1] <- NA
grid <- st_as_sf(s)

limx <- st_bbox(points)[c(1, 3)]
limy <- st_bbox(points)[c(2, 4)]

p <- ggplot() +
  geom_sf(
    data = grid,
    aes(fill = count),
    size = 0.3,
    alpha = 0.7,
    colour = "grey50"
  ) +
  geom_sf(
    data = comunas,
    fill = "transparent",
    size = 0.4,
    linetype = "dotted",
    colour = "cyan4"
  ) +
  scale_fill_gradientn(colors = carto_pal(n = 7, name = "Purp"),
                       na.value = NA) +
  labs(fill = expression(Paradas ~ por ~ km ^ {2}),
    title = toupper("Paradas de transporte público"),
    subtitle = toupper("Santiago de Chile"),
    caption = "@sporella"
  ) +
  coord_sf(xlim = limx, ylim = limy) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    plot.margin = unit(c(5, 0, 5, 0), "mm"),
    panel.spacing = unit(0, "mm"),
    panel.background = element_rect(fill = "#fbf5fc", colour = "transparent"),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    text = element_text(size = 8, colour = "grey60"),
    plot.title = element_text(hjust = 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10,-10,-10,-10)
  ) +
  guides(
    fill = guide_colourbar(
      title.position = "top",
      title.theme = element_text(size = 6, colour = "grey60"),
      barheight = unit(2, "mm"),
      barwidth = unit(20, "mm")
    )
  )

ggsave(
  "plots/10_paradas.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "#fbf5fc"
)
