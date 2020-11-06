# #30DayMapChallenge
# Día 6: rojo
# Cantidad de Bovinos en Regiones de Los Ríos y Los Lagos, Chile
# Fuente datos: https://ide.minagri.gob.cl/geoweb/2019/11/20/agricultura-y-ganaderia/
# Autora: Stephanie Orellana (@sporella)

library(tidyverse)
library(sf)
library(cowplot)
library(magick)
library(rcartocolor)

bov <- read_sf("data/cc_bovinos_cut.geojson")

regiones <- read_sf("data/regiones_bov.geojson")

# Hacer grilla de círculos ------------------------------------------------
## Codigo original en: https://rpubs.com/dieghernan/beautifulmaps_I

initial <- bov
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

grid <- st_make_grid(target, 5000,
                     crs = st_crs(initial),
                     what = "centers")

grid <- st_sf(index = 1:length(lengths(grid)), grid)
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- st_buffer(cent_merge, 5000 / 2)
circ_geom <- aggregate(
  grid_new,
  by = list(grid_new$index_target),
  FUN = min,
  do_union = FALSE
)

circ <- left_join(circ_geom %>% select(index_target),
                  st_drop_geometry(initial)) %>%
  select(-index_target)


# Visualización -----------------------------------------------------------


p <- ggplot() +
  geom_sf(
    data = circ,
    aes(fill = tot_bovino),
    colour = "grey90",
    size = 0.2
  ) +
  geom_sf(
    data = regiones,
    fill = "transparent",
    colour = "firebrick",
    size = 0.1
  ) +
  scale_fill_gradientn(
    colours = c("white", carto_pal(n = 7, name = "BurgYl")),
    breaks = seq(0, 50000, 5000),
    guide = "legend"
  ) +
  labs(
    tag = toupper("Bovinos en Regiones de Los Ríos y Los Lagos, Chile"),
    fill = "Número de bovinos",
    caption = "@sporella"
  ) +
  theme_minimal() +
  theme(
    legend.justification = "bottom",
    axis.text = element_text(size = 6, colour = "grey60"),
    plot.caption.position = "plot",
    plot.tag.position = "left",
    plot.tag = element_text(
      size = 10,
      angle = 90,
      vjust = 10,
      hjust = 10
    ),
    text = element_text(colour = "grey60")
  ) +
  guides(fill = guide_legend(
    title.position = "left",
    title.theme = element_text(angle = 90, colour = "grey60")
  )) +
  coord_sf(crs = 4326) 


vaca <-
  image_colorize(image_read("img/vaca.png"),
                 opacity = 40,
                 color = "white")
p2 <- ggdraw() +
  draw_image(vaca,  x = 0.3, y = 0.3, scale = .15) +
  draw_plot(p)


ggsave(
  "plots/6_vacas.png",
  plot = p2,
  device = "png",
  height = 6,
  width = 6
)
