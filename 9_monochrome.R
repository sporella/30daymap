# #30DayMapChallenge
# Día 9: monocromo
# Volcanes Wallmapu
# Fuente datos: https://ide.mma.gob.cl/
# Autora: Stephanie Orellana (@sporella)

library(elevatr)
library(sf)
library(stars)
library(tidyverse)
library(extrafont)

loadfonts()


# Cargar capas ------------------------------------------------------------

volcanes <- st_read("data/volcanes.geojson") %>% 
  filter(str_detect(`Región`, "Arau")) %>% 
  st_transform(crs = 4326)


com <- st_read("data/comunas_chile.geojson") %>% 
  filter(Region == "Región de La Araucanía" | Comuna == "Panguipulli") %>% 
  st_transform(crs = 4326)

regiones <- st_read("http://geonode.meteochile.gob.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Adivision_regional_geo&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")


# Descargar elevación -----------------------------------------------------
bb <- st_bbox(com) + c(-1, 0, +0.1, +1)
bb_p <- st_as_sf(st_as_sfc(bb))
a <- get_elev_raster(bb_p, z = 7, clip = "bbox")
a[a < 0] <- NA
s <- st_as_stars(a)
con <- st_contour(
  s,
  na.rm = T,
  contour_lines = T,
  breaks = seq(0, max(s$layer, na.rm = T), 200)
)

# Visualización -----------------------------------------------------------

p <- ggplot() +
  geom_stars(data = s,
             alpha = 0.75,
             show.legend = F) +
  geom_sf(data = regiones,
          fill = "transparent",
          colour = "#6E355B") +
  geom_sf(
    data = con,
    colour = alpha("#E4AACF", 0.4),
    size = 0.3,
    show.legend = F
  ) +
  geom_sf(
    data = volcanes,
    colour = "#61234D",
    shape = "\u17D9",
    size = 10,
    alpha = 0.9,
    stroke = 1
  ) +
  geom_sf_text(
    data = volcanes,
    aes(label = Sistema_vo),
    nudge_y = -0.06,
    nudge_x = -0.13,
    size = 2.5,
    colour = "#61234D",
    fontface = "bold",
    family = "Rockwell"
  ) +
  scale_fill_gradient(low = "#38142D",
                      high = "#F1D7E8",
                      na.value = "#EBD5E4") +
  labs(title = "Volcanes Activos",
       subtitle = "Región de La Araucanía, Wallmapu, Chile",
       caption = "@sporella") +
  coord_sf(
    crs = 4326,
    xlim = c(-73.8,-70.8),
    ylim = c(-39.8,-37.6)
  ) +
  theme(
    panel.grid = element_line(
      colour = "#61234D",
      size = 0.1,
      linetype = "dashed"
    ),
    panel.ontop = T,
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "#EBD5E4", colour = "transparent"),
    text = element_text(family = "Rockwell", colour = "#61234D"),
    axis.text = element_text(family = "Algerian", colour = "#61234D"),
    axis.title = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

ggsave(
  "plots/9_volcanes.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "#EBD5E4"
)
