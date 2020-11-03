# #30DayMapChallenge
# Día 2: líneas
# Origen y destino de viajes en Santiago
# Datos: http://www.dtpm.gob.cl/index.php/documentos/matrices-de-viaje
# Autora: Stephanie Orellana (@sporella)

library(tidyverse)
library(sf)


# Cargar y procesar datos -------------------------------------------------

viajes <-
  read_csv2(
    "https://raw.githubusercontent.com/sporella/nightingale/master/data/matriz_viajes.csv"
  ) %>%
  filter(MediaHora == 25200) %>%
  select(1, 2, 4) %>%
  mutate_if(is.character, str_to_title)


comunas <-
  read_sf("data/comunas_metropolitana.geojson") %>%
  filter(Region == "Región Metropolitana de Santiago") %>%
  mutate(Comuna = iconv(Comuna, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  select(Comuna, Provincia)

centroides <- comunas %>%
  st_centroid() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2],
    Comuna = iconv(Comuna, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  )


# Join --------------------------------------------------------------------

viajes_sp <- viajes %>%
  left_join(centroides, by = c("ComunaSubida" = "Comuna")) %>%
  left_join(
    centroides,
    by = c("ComunaBajada" = "Comuna"),
    suffix = c("_Subida", "_Bajada")
  ) %>%
  slice(1:300) %>%
  mutate(
    lon_Bajada = ifelse(ComunaSubida == ComunaBajada, lon_Bajada + 10, lon_Bajada),
    lat_Bajada = ifelse(ComunaSubida == ComunaBajada, lat_Bajada + 10, lat_Bajada)
  )


# Filtrar solo comunas presentes ------------------------------------------
cc <- c(unique(viajes_sp$ComunaSubida), unique(viajes_sp$ComunaBajada))

centroides2 <- centroides %>% filter(Comuna %in% cc)


# Límites para zoom -------------------------------------------------------

limx <- st_bbox(centroides2)[c(1, 3)] + c(-10000,+10000)
limy <- st_bbox(centroides2)[c(2, 4)] + c(-10000,+10000)


# Visualización -----------------------------------------------------------

p <- ggplot() +
  geom_curve(
    data = viajes_sp,
    aes(
      x = lon_Subida,
      y = lat_Subida,
      xend = lon_Bajada,
      yend = lat_Bajada,
      colour = ComunaSubida,
      size = ViajeLaboralPromedio
    ),
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_sf(data = comunas,
          fill = "transparent",
          colour = "grey80") +
  geom_sf_text(data = centroides,
               aes(label = Comuna),
               colour = "grey90",
               size = 2) +
  scale_size(range = c(0.01, 2.5)) +
  labs(
    x = "",
    y = "",
    title = "Origen y Destino de Viajes en Transporte Público 7:00 AM",
    subtitle = "Santiago de Chile",
    caption = "@sporella"
  ) +
  coord_sf(xlim = limx, ylim = limy) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey33", colour = "grey33"),
    panel.background = element_rect(fill = "grey33"),
    panel.grid = element_line(
      colour = "mediumturquoise",
      linetype = "dashed",
      size = 0.3
    ),
    axis.text = element_text(colour = "mediumturquoise"),
    axis.ticks = element_line(colour = "mediumturquoise"),
    text = element_text(colour = "plum")
  )

ggsave(
  "plots/2_viajes7am.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "grey33"
)
