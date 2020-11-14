# #30DayMapChallenge
# Día 14: Cambio Climático
# Anomalías año 2019
# Fuente datos: https://www.globalclimatemonitor.org/#
# Autora: Stephanie Orellana (@sporella)

library(stars)
library(tidyverse)
library(sf)
library(rcartocolor)
library(patchwork)


# Cargar y procesar datos -------------------------------------------------

an_temp <- read_csv("data/anomalias_temp_anual_espacial_p.csv") %>% 
  st_as_sf(wkt = "geom", crs = 3857) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs") %>% 
  select(anomalia_temp_anual) %>% 
  st_rasterize() %>% 
  st_as_sf(merge = F) %>% 
  mutate(lab = case_when(anomalia_temp_anual > 5 ~ 5,
                         anomalia_temp_anual < -5 ~ -5,
                         TRUE ~ anomalia_temp_anual),
         tipo = "Anomalía de Temperatura [°C]")

an_pp <- read_csv("data/anomalias_pre_anual_espacial_p.csv") %>% 
  st_as_sf(wkt = "geom", crs = 3857) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs") %>% 
  select(anomalia_pre_anual) %>% 
  st_rasterize() %>% 
  st_as_sf(merge = F) %>% 
  mutate(lab = case_when(anomalia_pre_anual > 400 ~ 400,
                         anomalia_pre_anual < -400 ~ -400,
                         TRUE ~ anomalia_pre_anual))



# Visualización -----------------------------------------------------------

# * Tema ------------------------------------------------------------------


theme_world <- function(x, back_colour = "#420b41") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    panel.grid = element_line(colour = "grey70", linetype = "dotted"),
    panel.ontop = T,
    legend.position = "left",
    text = element_text(size = 8, colour = "grey60"), 
    strip.text = element_text(size = 8, colour = "grey60",face = "bold"),
    strip.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.ticks = element_line(colour = "grey60"),
    axis.text = element_text(colour = "grey60"),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 5)
  )
}


# * Gráficos separados ----------------------------------------------------

pt <- ggplot(an_temp) +
  geom_sf(aes(fill = lab), colour = "transparent") +
  scale_fill_gradient2(
    low = "dodgerblue4",
    mid = "khaki",
    high = "red3",
    midpoint = 0,
    breaks = seq(-5, 5, 1),
    labels = c("\u2264 -5", seq(-4, 4, 1), "\u2265 5")
  ) +
  labs(title = "ANOMALÍAS CLIMÁTICAS\nAÑO 2019")+
  facet_wrap(~"Anomalía de Temperatura Media [°C]")+
  theme_world()+
  guides(fill = guide_colorbar("", barheight = unit(4, "cm"), barwidth = unit(3, "mm")))


pp <- ggplot(an_pp) +
  geom_sf(aes(fill = lab), colour = "transparent") +
  scale_fill_gradient2(
    low = "orange4",
    mid = "khaki",
    high = "turquoise4",
    midpoint = 0,
    breaks = seq(-400, 400, 100),
    labels = c("\u2264 -400", seq(-300, 300, 100), "\u2265 400")
  ) +
  labs(caption = "@sporella", tag = "Datos: Global Climate Monitor")+
  facet_wrap(~"Anomalía de Precipitación [mm]")+
  theme_world()+
  guides(fill = guide_colorbar(
    "",
    barheight = unit(4, "cm"),
    barwidth = unit(3, "mm")
  ))


# * Unir gráficos ---------------------------------------------------------

p <- (pt / pp) &  theme_world()

ggsave(
  filename = "plots/14_anomalias.png",
  plot = p,
  device = "png",
  height = 6,
  width = 6,
  bg = "#420b41"
)

