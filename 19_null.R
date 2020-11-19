# #30DayMapChallenge
# Día 19: Nulo
# Sin Internet
# Fuente datos: https://www.subtel.gob.cl/estudios-y-estadisticas/internet/
# Autora: Stephanie Orellana (@sporella)


library(tidyverse)
library(sf)
library(ggrepel)
library(stringdist)
library(patchwork)
library(extrafont)

loadfonts()


# Cargar y procesar datos -------------------------------------------------

red <- read_csv("data/conexiones_internet_fija.csv", col_names = FALSE)
names(red) <- paste0(red[1, ], "_", red[2, ])
red <- red[-c(1:2), ]

red_longer <- red %>%
  fill(1) %>%
  pivot_longer(
    cols = -c(1, 2),
    names_to = c("annio", "mes"),
    values_to = "numero_conexiones",
    names_pattern = "(.*)_(.*)",
    values_transform = list(numero_conexiones = as.numeric)
  ) %>%
  rename(region = 1, comuna = 2) %>%
  group_by(annio, comuna) %>%
  summarise(total = sum(numero_conexiones, na.rm = T)) %>%
  filter(annio == "2019") %>%
  mutate(null = if_else(total == 0, 0, 1))


# *Unir con geojson de comunas --------------------------------------------
comunas <- read_sf("data/comunas_chile.geojson")
a <- unique(comunas$Comuna)
b <- unique(red_longer$comuna)
dist <- data.frame(Comuna = a, 
                   comuna = b[stringdist::amatch(a, b, maxDist = 5)])
dist$d <- stringdist(dist$Comuna, dist$comuna)

red_longer <- red_longer %>%
  left_join(dist)

red_sp <- comunas %>%
  left_join(red_longer) %>%
  mutate(lat = st_coordinates(st_centroid(.))[, 2],
         cut = as.character(cut_interval(lat, 3, labels = c("a", "b", "c")))) %>%
  filter(!Comuna %in% c("Isla de Pascua", "Juan Fernández"))


# Visualización -----------------------------------------------------------

# * 3 partes --------------------------------------------------------------

g <- purrr::map(c("c", "b", "a"),
                function(x) {
                  ggplot() +
                    geom_sf(
                      data = red_sp %>% filter(cut ==  x),
                      aes(fill = factor(null)),
                      size = 0.3,
                      colour = "grey80"
                    ) +
                    geom_text_repel(
                      data = red_sp %>% filter(null == 0, cut == x),
                      aes(label = Comuna, geometry = geometry),
                      stat = "sf_coordinates",
                      inherit.aes = F,
                      direction = "both",
                      nudge_x = 0.2,
                      size = 1.7,
                      seed = 2020,
                      colour = "grey40",
                      family = "Lucida Console"
                    ) +
                    scale_fill_manual(
                      values = c("gold", "grey97"),
                      breaks = c(0, 1),
                      labels = c(`0` = "Sin", `1` = "Con")
                    ) +
                    labs(fill = "") +
                    coord_sf(label_graticule = "W") +
                    theme_void()
                })


# * unir con patchwork ----------------------------------------------------

t <- "internet_fija = NULL"
sub <- "Comunas en Chile Continental que No Registraron Conexiones de Internet Fija\nAño 2019"

g2 <- wrap_plots(g) + 
  plot_layout(guides = "collect", design = "123") + 
  plot_annotation(title = t,
                  subtitle = sub,
                  caption = "@sporella") &
  theme(
    legend.position = "none",
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(
      size = 15,
      colour = "grey60",
      family = "Impact"
    ),
    plot.subtitle = element_text(
      size = 8,
      colour = "gold3",
      family = "Lucida Console"
    ),
    plot.caption = element_text(
      size = 6,
      colour = "grey60",
      hjust = 0.95,
      family = "Lucida Console"
    ),
    plot.margin = margin(c(0, -5, 0, 5))
  )


ggsave("plots/19_sininternet.png",
       g2,
       width = 6,
       height = 6,
       bg = "grey98")

