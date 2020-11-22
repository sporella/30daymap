# #30DayMapChallenge
# Día 21: Agua
# 20 lagos con mayor superficie en Chile
# Fuente datos: https://dga.mop.gob.cl/estudiospublicaciones/mapoteca/Paginas/default.aspx#cinco
# Autora: Stephanie Orellana (@sporella)


library(sf)
library(tidyverse)
library(patchwork)


# Función para etiquetar coordenadas --------------------------------------

dms_lab <- Vectorize(function(dd){
  d <- as.integer(dd)
  m <- as.integer((dd - d)*60)
  # s <- as.integer((dd - d - m/60)*3600)
  # text <- paste0(abs(d), "°", abs(m), "'", abs(s), "''")
  text <- paste0(abs(d), "°", abs(m), "'")
  text
}) 


# Cargar y procesar datos -------------------------------------------------


l <- read_sf("data/lagos_chile.geojson") %>% 
  slice_max(n = 20, order_by = AREA_KM2) %>% 
  st_transform(crs = 4326) %>% 
  mutate(lat = st_coordinates(st_centroid(.))[, 2],
         lon = st_coordinates(st_centroid(.))[, 1],
         lat_lab = paste0(dms_lab(lat), " S"),
         lon_lab = paste0(dms_lab(lon), " W"))


# Visualización -----------------------------------------------------------


# * Generar dimensiones según más grande ----------------------------------

lmax <- l %>%  slice_max(AREA_KM2) %>% st_bbox()
xdif <- diff(lmax[c(1,3)])+0.1
ydif <- diff(lmax[c(2,4)])+0.01

g <- purrr::map(l$NOMBRE,
                function(x) {
                  d <- filter(l, NOMBRE == x)
                  bb <- d %>% st_bbox()
                  difx <- diff(bb[c(1, 3)])
                  dify <- diff(bb[c(2, 4)])
                  if (difx < xdif) {
                    limx <- bb[c(1, 3)] + c(-(xdif - difx) / 2, +(xdif - difx) / 2)
                    limy <- bb[c(2, 4)] + c(-(ydif - dify) / 2, +(ydif - dify) / 2)
                  } else{
                    limx <- lmax[c(1, 3)]
                    limy <- lmax[c(2, 4)]
                  }
                  s <- ggplot() +
                    geom_sf(data = d,
                            colour = "transparent",
                            fill = "deepskyblue3") +
                    labs(
                      tag = d$NOMBRE,
                      subtitle = paste0(d$lat_lab, " - ", d$lon_lab, "\n", round(d$AREA_KM2), " km\u00B2")
                    ) +
                    coord_sf(xlim = limx, ylim = limy) +
                    theme_void() +
                    theme(
                      plot.tag.position = "top",
                      plot.tag = element_text(
                        size = 6,
                        hjust = 0.5,
                        colour = "dodgerblue3",
                        face = "bold"
                      ),
                      plot.subtitle = element_text(
                        size = 5,
                        hjust = 0.5,
                        colour = "grey30",
                        vjust = 5
                      )
                    )
                })


# * Unir con patchwork ----------------------------------------------------


g2 <- wrap_plots(g, ncol = 4) +
  plot_annotation(
    title = toupper("20 Cuerpos de agua con mayor superficie en Chile\n"),
    caption = "@sporella"
  ) &
  theme(
    plot.background = element_rect(fill = "lightcyan", colour = "lightcyan"),
    plot.title = element_text(
      colour = "lightblue4",
      vjust = -1,
      hjust = 0.5
    ),
    plot.caption.position = "plot",
    plot.caption = element_text(
      colour = "lightblue4",
      hjust = 1.1,
      size = 6
    )
  )

ggsave("plots/21_20lagos.png",
       g2,
       width = 6,
       height = 6,
       bg = "lightcyan")
