# #30DayMapChallenge
# Día 23: Límites
# Reclamaciones territoriales en la antártica
# Datos: https://geodata.lib.berkeley.edu/catalog/stanford-jj231ks3840
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

ant <- read_sf("data/antarctic_claims.json")

ant_cent <- ant %>%
  st_cast("POLYGON") %>%
  bind_cols(as.data.frame(do.call("rbind", lapply(st_geometry(.), st_bbox)))) %>%
  mutate(
    lat = ifelse(name == "Peter I Island", ymax - 3, ymax - 8),
    lon = case_when(
      name == "Queen Maud Land" ~ xmax - 5,
      name == "Peter I Island" ~ xmax - 3,
      TRUE ~ xmin + 3
    )
  ) %>%
  arrange(xmin) %>%
  mutate(
    angle = c(88 , 0,-5,-15,-20,-40,-72, 45, 45,-45,-52,-80),
    lab = ifelse(sovereignt == "New Zealand" & angle == -80, "", sovereignt),
    lab = c(
      "Nueva Zelanda",
      "Noruega",
      "Chile",
      "Reino Unido",
      "Argentina",
      "Brasil (no oficial)",
      "Alemania (histórico)",
      "Noruega",
      "Australia",
      "Francia",
      "Australia",
      ""
    )
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


world <- ne_countries(scale = "medium", returnclass = "sf", continent = c("Antarctica"))


# Visualización -----------------------------------------------------------

colores <- c("palevioletred",
    "dodgerblue4",
    "springgreen4",
    "gold",
    "plum3",
    "wheat4",
    "deeppink3",
    "lightseagreen",
    "mediumpurple"
  )


theme_myvoid <- function(back_colour = "#420b41") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(size = 8, colour = "grey99"),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 10,
      hjust = 0.5,
      vjust = -3
    ),
    plot.caption.position = "plot",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
}

p <- ggplot() +
  geom_sf(data = world,
          colour = "transparent",
          fill = "grey90") +
  geom_sf(
    data = ant,
    aes(colour = sovereignt),
    size = 0.7,
    fill = "transparent",
    linetype = "dotdash",
    show.legend = F
  ) +
  geom_text(
    data = ant_cent,
    aes(
      group = sovereignt,
      label = lab,
      colour = sovereignt,
      geometry = geometry,
      angle = angle
    ),
    size = 2.4,
    fontface = "bold",
    stat = "sf_coordinates",
    show.legend = F
  ) +
  scale_fill_manual(values = colores) +
  scale_colour_manual(values = colores) +
  labs(title = toupper("Reclamaciones territoriales en la Antártica"),
       caption = "@sporella") +
  coord_sf(crs = "+proj=laea +lat_0=-90 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") +
  theme_myvoid(back_colour = "paleturquoise3")

ggsave("plots/23_antartica.png",
       p,
       width = 6,
       height = 6,
       bg = "paleturquoise3")
