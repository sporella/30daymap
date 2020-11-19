# #30DayMapChallenge
# Día 18: Uso Suelo
# Uso Suelo Santiago Centro
# Fuente datos: IDE Observatorio de Ciudades UC
# https://ideocuc-ocuc.hub.arcgis.com/datasets/08dea558cea94b64bee9074ed0fbfd4f_0
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(tidyverse)
library(ggmap)


# Datos -------------------------------------------------------------------

prc <-
  st_read(
    "https://opendata.arcgis.com/datasets/08dea558cea94b64bee9074ed0fbfd4f_0.geojson"
  ) %>%
  mutate(
    lab = case_when(
      uso_suelo == "AREA verde publica" ~ "Área verde pública",
      uso_suelo == "Uso AREA verde" ~ "Uso área verde",
      uso_suelo == "Club hipico" ~ "Club hípico",
      uso_suelo == "Club hipico" ~ "Club hípico",
      uso_suelo == " " ~ " Sin Información",
      TRUE ~ uso_suelo
    )
  )

# Obtener mapa base -------------------------------------------------------
bb <- st_bbox(prc) + c(-0.001,-0.001,+0.001,+0.001)
names(bb) <- c("left", "bottom", "right", "top")
basemap <-
  get_stamenmap(bb,
                zoom = 14,
                maptype = "toner-lite",
                force = T)



# Visualización -----------------------------------------------------------

colores <- c("#00b3d7",
             "#759b44",
             "#6778d0",
             "#9750a1",
             "#c1a03a",
             "#b75b37",
             "#4dc38e")

p1 <- ggmap(basemap, darken = c(0.5, "grey80")) +
  geom_sf(
    data = prc,
    aes(fill = lab),
    inherit.aes = F,
    alpha = 0.6,
    size = 0.3,
    colour = "grey33"
  ) +
  scale_fill_manual(values = colores) +
  labs(
    x = "",
    y = "",
    fill = "USO DE SUELO",
    title = "PLAN REGULADOR COMUNAL\nSANTIAGO CENTRO",
    caption = "@sporella"
  ) +
  theme(
    text = element_text(size = 8, colour = "grey50"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 6),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    plot.background = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1.1, size = 6)
  ) +
  guides(
    fill = guide_legend(
      label.position = "bottom",
      title.position = "top",
      title.theme = element_text(size = 5, colour = "grey50"),
      nrow = 1,
      keyheight = unit(2, "mm")
    )
  ) +
  coord_sf()


ggsave("plots/18_prcsantiago.png",
       p1,
       width = 6,
       height = 6,
       bg = "grey95")
