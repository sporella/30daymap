# #30DayMapChallenge
# Día 29: Globo
# GFS 29-11-2020
# Datos: https://www.nco.ncep.noaa.gov/pmb/products/gfs/
# Autora: Stephanie Orellana (@sporella)

library(raster)
library(rnaturalearth)
library(ggplot2)
library(animation)
library(sf)

r <- stack("data/gfs.t00z.pgrb2.1p00.f000")
a <- r[[415]]
extent(a) <- extent(0, 360, -90, 90)
a <- rotate(a)

world <- ne_countries(scale = "medium", returnclass = 'sf')

saveGIF({ lapply(seq(0, 360, length.out = 30) , function(x) {
    b <- projectRaster(a,
        crs = paste0("+proj=laea +lat_0=20 +lon_0=", x),
        res = c(111000, 104000)
      )
    
    tab <- data.frame(rasterToPoints(b))
    
    world_p <- st_transform(world, crs = paste0("+proj=laea +lat_0=20 +lon_0=", x))
    
    p <- ggplot() +
      geom_tile(data = tab, aes(x = x, y = y, fill = gfs.t00z.pgrb2.1p00.415)) +
      geom_sf(
        data = world_p,
        fill = NA,
        colour = "cadetblue",
        size = 0.0001
      ) +
      scale_fill_gradientn(
        colours = rev(rainbow(11, alpha = 0.7)),
        guide = "legend",
        breaks = seq(-40, 40, 10)
      ) +
      labs(caption = "@sporella",
           title = "Global Forecast System\n2020-11-29",
           fill = "Temperature 2 m above ground [°C]") +
      guides(
        fill = guide_legend(
          title.position = "left",
          title.theme = element_text(
            angle = 90,
            size = 8,
            colour = "grey33"
          ),
          keyheight = unit(0.6, "cm"),
          keywidth = unit(0.2, "cm"),
          reverse = T
        )
      ) +
      theme(
        rect = element_blank(),
        text = element_text(colour = "grey33"),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, colour = "grey33"),
        plot.caption.position = "plot",
        legend.justification = "bottom"
      ) +
      coord_sf()
    
    print(p)
  })
}, movie.name = "29_gfs.gif", ani.width = 580, ani.height = 580, ani.res =
  120, interval = 0.5, autobrowse = FALSE)

file.rename("29_gfs.gif", "plots/29_gfs.gif")
