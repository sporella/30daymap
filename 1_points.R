# #30DayMapChallenge
# Día 1: points
# Accidentes de tránsito en Santiago de Chile
# Datos: Comisión Nacional de Seguridad de Tránsito y Cedeus
# Autora: Stephanie Orellana (@sporella)


library(ggplot2)
library(sf)

accidentes <-
  read_sf(
    "https://opendata.arcgis.com/datasets/eb4a3814cf6e478e988b7dae38c770de_0.geojson"
  )

# Esta capa es muy pesada, demorará en cargar y el gráfico también demora en generarse.
red_vial <-
  read_sf(
    "http://datos.cedeus.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Asantiago_teczno_2014feb_line_lu_type_contains_road_proj_utm19s&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature"
  ) %>%
  st_crop(st_bbox(accidentes))


p <- ggplot() +
  geom_sf(data = red_vial, color = alpha("mediumturquoise", 0.2)) +
  geom_sf(data = accidentes,
          aes(colour = Accidentes, size = Accidentes),
          shape = 20) +
  scale_size_continuous(range = c(0.1, 6), limits = c(5, 60)) +
  scale_colour_viridis_c(option = "C", limits = c(5, 60)) +
  labs(title = "Puntos Críticos de Accidentes de Tránsito\nSantiago de Chile", 
       subtitle = "Año 2019",
       caption = "@sporella") +
  guides(
    colour = guide_legend(title.position = "left",),
    size = guide_legend(title.position = "left")
  ) +
  theme(plot.caption.position = "plot",
    panel.background = element_rect(fill = "grey33"),
    plot.background = element_rect(fill = "grey33", colour = "grey33"),
    text = element_text(colour = "goldenrod2"),
    axis.text = element_text(colour = "goldenrod2"),
    axis.ticks = element_line(colour = "goldenrod2"),
    panel.grid = element_line(colour = "goldenrod2", linetype = "dotted"),
    legend.background = element_rect(fill = "grey33"),
    legend.key = element_rect(fill = "grey33"),
    legend.text = element_text(colour = "goldenrod2"),
    legend.title = element_text(
      colour = "goldenrod2",
      angle = 90,
      hjust = 0.5
    )
  )

ggsave(
  filename = "plots/1_accidentes.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "grey33"
  
)
