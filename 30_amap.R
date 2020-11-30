# #30DayMapChallenge
# Día 30: Un mapa
# Densidad de poblacion sudamerica
# Datos: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11/data-download
# Autora: Stephanie Orellana (@sporella)

library(raster)
library(rayshader)

r <- raster("data/densidad_poblacion_sudamerica_2020_5min.tif")

elmat = raster_to_matrix(r)

elmat %>%
  sphere_shade(texture = create_texture("springgreen", "darkgreen",
                                        "turquoise", "steelblue3", "white")) %>%
  add_shadow(ray_shade(elmat, zscale = 150), 0.5) %>%
  plot_3d(
    elmat,
    zoom = 0.55,
    theta = 0,
    phi = 45,
    fov = 0,
    zscale = 150,
    soliddepth = -3,
    windowsize = c(800, 720)
  )

text <- paste0("DENSIDAD DE POBLACIÓN\nSUDAMÉRICA",
               strrep("\n", 34),
               "@sporella")

render_snapshot(
  filename = "plots/30_poblacionsudamerica.png",
  title_text = text,
  title_size = 16,
  title_color = "grey50",
  clear = T
)