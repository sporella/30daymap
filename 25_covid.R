# #30DayMapChallenge
# Día 25: COVID
# Casos Activos Región Metropolitana
# Datos: https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto19
# Autora: Stephanie Orellana (@sporella)

library(tidyverse)
library(sf)
library(gganimate)
library(ggforce)
library(rcartocolor)


# Cargar y procesar datos -------------------------------------------------

covid <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna.csv") %>% 
  pivot_longer(
    cols = -c(1:5),
    names_to = "fecha",
    values_to = "casos",
    names_prefix = "x"
  ) %>%
  mutate(
    cod_comuna = as.numeric(`Codigo comuna`),
    fecha = as.Date(fecha, format = "%Y-%m-%d")
  )

comunas <- st_read("data/comunas_metropolitana.geojson")

covid_sp <- comunas %>%
  left_join(covid, by = "cod_comuna") %>%
  mutate(x0 = st_coordinates(st_centroid(.))[, 1],
         y0 = st_coordinates(st_centroid(.))[, 2])


# Tema --------------------------------------------------------------------

theme_myvoid <- function(back_colour = "#420b41", text_colour = "grey50") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    panel.grid = element_blank(),
    text = element_text(size = 10, colour = text_colour),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 14,
      hjust = 0.5,
      vjust = -1,
      face = "bold"
    ),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8),
    plot.tag.position = "top",
    plot.tag = element_text(size = 7, hjust = 0.5),
    plot.subtitle = element_text(
      size = 10,
      hjust = 0.5,
      vjust = -2,
      colour = "indianred1"
    ),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
}


# Visualización -----------------------------------------------------------

p <- ggplot() +
  geom_circle(data = covid_sp,
              aes(
                fill = casos,
                r = casos * 10,
                x0 = x0,
                y0 = y0
              ),
              colour = "grey90") +
  geom_sf(data = comunas,
          fill = "transparent",
          colour = "#635782") +
  scale_fill_carto_c(palette = "SunsetDark",
                     direction = 1,
                     n.breaks = 6) +
  labs(
    title = toupper("COVID19: Casos Activos Por Comuna"),
    tag = toupper("Región Metropolitana, Chile"),
    subtitle = "{frame_time}",
    caption = "@sporella",
    fill = "Número de casos"
  ) +
  theme_myvoid(back_colour = "#1c0063") +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(5, "cm"),
    barheight = unit(2, "mm")
  )) +
  coord_sf(xlim = range(covid_sp$x0), ylim=range(covid_sp$y0))+
  transition_time(fecha)

anim_save(
  filename = "plots/25_covid.gif",
  nframes = 100,
  fps = 5,
  end_pause = 2,
  animation = p,
  bg = "#1c0063",
  width = 6,
  height = 6,
  units = "in",
  res = 120
)
