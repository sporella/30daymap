# #30DayMapChallenge
# Día 4: hexágonos
# Temperatura superficial enero
# Fuente datos: Procesados y descargados de Google Earth Engine 
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MOD11A1
# Autora: Stephanie Orellana (@sporella)

library(stars)
library(tidyverse)
library(sf)
library(extrafont)
library(rcartocolor)
# font_import()
loadfonts()


# Cargar datos ------------------------------------------------------------
temp <- read_stars("data/ene_val_san_2010_2020.tif") %>% 
  st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  st_transform(crs=32719) %>% 
  rename(enero = 1)

comunas <- read_sf("data/comunas_chile.geojson") %>% 
  st_transform(crs=32719) %>%
  filter(codregion %in% c(4,5,6,13))

# Hacer grilla hexagonal --------------------------------------------------
## Codigo original en: https://rpubs.com/dieghernan/beautifulmaps_I

initial <- temp
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

grid <- st_make_grid(target,
                     5000,
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE
)

grid <- st_sf(index = 1:length(lengths(grid)), grid)
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

hex_geom <-
  aggregate(
    grid_new,
    by = list(grid_new$index_target),
    FUN = min,
    do_union = FALSE
  )

hex_comb <-
  left_join(hex_geom %>% select(index_target), st_drop_geometry(initial)) %>%
  select(-index_target)


# Visualización -----------------------------------------------------------

# * Cortar área de interés ------------------------------------------------
hex_comb_cut <- hex_comb %>% 
  st_filter(comunas) %>% 
  st_transform(crs = 4326) %>% 
  mutate(grados = (enero * 0.02) - 273.15)

# * Límites para zoom -----------------------------------------------------

limx <- st_bbox(hex_comb_cut)[c(1, 3)] #+ c(-10000,+10000)
limy <- st_bbox(hex_comb_cut)[c(2, 4)] #+ c(-10000,+10000)


p <- ggplot()+
  geom_sf(data = hex_comb_cut, aes(fill = grados), colour = "transparent")+
  geom_sf(data = comunas, fill = "transparent", colour = "grey85", size = 0.3)+
  scale_fill_gradientn(colours = carto_pal(n = 7, "Temps"))+
  labs(title = "Temperatura Superficial Mes de Enero", 
       subtitle = "MOD11A1 PROMEDIO 2010-2020\nRegiones Valparaíso y Metropolitana, Chile.", 
       fill = "Temperatura [°C]",
       caption = "@sporella")+
  theme(text = element_text(family = "Arial Narrow", colour = "mediumturquoise"),
        plot.caption.position = "plot", 
        plot.title.position = "plot",
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "grey33", colour = "grey33"),
        axis.text = element_text(colour = "mediumturquoise"),
        axis.ticks = element_line(colour = "mediumturquoise"),
        panel.grid = element_line(colour = "mediumturquoise", linetype = "dotted"),
        legend.background = element_rect(fill = "grey33"),
        legend.key = element_rect(fill = "grey33"),
        legend.text = element_text(colour = "mediumturquoise"),
        panel.ontop = TRUE)+
  guides(fill = guide_colourbar(
    title.position = "left",
    title.theme = element_text(
      angle = 90,
      family = "Arial Narrow",
      colour = "mediumturquoise",
      hjust = 0.5
    ),
  ))+
  coord_sf(crs = 4326, xlim = limx, ylim = limy)

ggsave(
  "plots/4_temp_ene.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "grey33"
)
