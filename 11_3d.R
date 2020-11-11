# #30DayMapChallenge
# Día 11: 3D
# Parque Nacional Torres del Paine
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(rayshader)
library(elevatr)
library(raster)

cuernos <- st_sfc(st_point(c(-73.0004, -50.9968)), crs = 4326) %>% 
  st_transform(32719) %>% 
  st_buffer(dist = 50000)

a <- get_elev_raster(as_Spatial(cuernos), z = 9, clip = "locations")
r3 <- focal(a, w= matrix(1,3,3), mean)
# mapview::mapview(a)

elmat = raster_to_matrix(r3)

elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat, zscale = 20), color = "cyan3") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  plot_3d(
    elmat,
    zscale = 20,
    fov = 0,
    theta = 45,
    zoom = 0.58,
    phi = 45,
    windowsize = c(800, 800),
    solidcolor = "honeydew4",
    watercolor = "cyan3",
    solidlinecolor = "grey60",
    background = "honeydew",
    soliddepth = -30,
  )
text <- paste0("PARQUE NACIONAL TORRES DEL PAINE\nCHILE",
               strrep("\n", 38),
               "@sporella")

render_snapshot(
  filename = "plots/11_torresdelpaine.png",
  title_text = text,
  title_size = 16,
  title_color = "grey50",
  clear = T
)

