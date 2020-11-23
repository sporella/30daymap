# #30DayMapChallenge
# Día 22: Movimiento
# Origen y destino de viajes en Santiago 7 AM
# Datos: http://www.dtpm.gob.cl/index.php/documentos/matrices-de-viaje
# Autora: Stephanie Orellana (@sporella)

library(tidyverse)
library(sf)
library(gganimate)


# Cargar y procesar datos -------------------------------------------------

viajes <-
  read_csv2(
    "https://raw.githubusercontent.com/sporella/nightingale/master/data/matriz_viajes.csv"
  ) %>%
  select(1:4) %>%
  mutate_if(is.character, str_to_title) %>% 
  mutate(hora = as.numeric(MediaHora)) %>% 
  select(-3) %>% 
  filter(hora == 25200)


comunas <-
  read_sf("data/comunas_metropolitana.geojson") %>%
  filter(Region == "Región Metropolitana de Santiago") %>%
  mutate(Comuna = iconv(Comuna, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  select(Comuna)

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
  mutate(
    lon_Bajada = ifelse(ComunaSubida == ComunaBajada, lon_Bajada + 1000, lon_Bajada),
    lat_Bajada = ifelse(ComunaSubida == ComunaBajada, lat_Bajada + 1000, lat_Bajada)
  ) %>% 
  pivot_longer(-c(1:5, 8)) %>% 
  mutate(
    time = ifelse(str_detect(name, "_Subida"), 60, 60*29),
    id = paste0(ComunaSubida, "_", ComunaBajada, "_", hora),
    name = str_replace(name, "_Subida|_Bajada", "")) %>% 
  pivot_wider(id_cols = c(id, time, hora, 1:5)) %>% 
  mutate(horatime = hora+time)



# Filtrar solo comunas presentes ------------------------------------------
cc <- c(unique(viajes_sp$ComunaSubida), unique(viajes_sp$ComunaBajada))

centroides2 <- centroides %>% filter(Comuna %in% cc)

# Límites para zoom -------------------------------------------------------

limx <- st_bbox(centroides2)[c(1, 3)] + c(-5000,+5000)
limy <- st_bbox(centroides2)[c(2, 4)] + c(-5000,+5000)


# Visualización -----------------------------------------------------------

# * Tema ------------------------------------------------------------------

theme_travel <- function(back_colour = "#420b41") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(size = 8, colour = "grey60"),
    plot.title.position = "plot",
    plot.title = element_text(colour = "grey70", face = "bold"),
    plot.caption.position = "plot",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
}


colores <- c("#88e875",
             "#c86ee6",
             "#3eb64a",
             "#e660b8",
             "#71b634",
             "#8480ef",
             "#cde663",
             "#4691eb",
             "#dac53d",
             "#be7ecf",
             "#92ac35",
             "#ec5481",
             "#5ee7a5",
             "#ed634d",
             "#4fd0da",
             "#e2792c",
             "#59b9e4",
             "#dd9e37",
             "#5ea0da",
             "#b09c35",
             "#9999dc",
             "#73aa57",
             "#e1a1df",
             "#55ae70",
             "#dd80a8",
             "#72e7ce",
             "#dd7d78",
             "#47a88e",
             "#be8147",
             "#b8e59a",
             "#e8a978",
             "#96a863",
             "#e3d687",
             "#a39754")

names(colores) <- unique(viajes_sp$ComunaBajada)


p <- ggplot() +
  geom_sf(
    data = comunas,
    fill = "transparent",
    colour = "gold4",
    size = 0.2
  ) +
  geom_point(
    data = viajes_sp,
    aes(
      x = lon,
      y = lat,
      colour = ComunaBajada,
      group = id,
      size = ViajeLaboralPromedio
    ),
    show.legend = F
  ) +
  geom_sf_text(
    data = centroides2,
    aes(label = Comuna,
        colour = Comuna),
    nudge_y = -2000,
    size = 3,
    show.legend = F
  ) +
  scale_color_manual(values = colores) +
  scale_size(range = c(0.1, 10)) +
  labs(title = "VIAJES EN TRANSPORTE PÚBLICO\n7:00 AM\nSANTIAGO DE CHILE", caption = "@sporella") +
  theme_travel(back_colour = "grey10") +
  coord_sf(xlim = limx, ylim = limy) +
  transition_time(horatime, range = c(25200, 27000)) +
  shadow_wake(wake_length = 0.1, alpha = 0.1)


anim_save(
  filename = "plots/22_viajes7am.gif",
  nframes = 100,
  fps = 10,
  animation = p,
  bg = "grey10",
  width = 6,
  height = 6,
  units = "in",
  res = 120
)
