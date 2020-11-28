# #30DayMapChallenge
# Día 28: no geográfico
# Marte
# Datos: https://astrogeology.usgs.gov/search/details/Mars/GlobalSurveyor/MOLA/Mars_MGS_MOLA_DEM_mosaic_global_463m/cub
# Autora: Stephanie Orellana (@sporella)

library(stars)
library(RColorBrewer)
library(showtext)

# Descargar datos ---------------------------------------------------------
# !!! 2GB

download.file("https://planetarymaps.usgs.gov/mosaic/Mars_MGS_MOLA_DEM_mosaic_global_463m.tif", 
              destfile = "./data/Mars_MGS_MOLA_DEM_mosaic_global_463m.tif", mode = "wb")


mars <- read_stars("data/Mars_MGS_MOLA_DEM_mosaic_global_463m.tif")

font_add_google("Exo 2", "Exo_bold", regular.wt = 900)
font_add_google("Exo 2", "Exo", regular.wt = 300)


showtext_auto()

png(filename = "plots/28_marte.png", width = 6, height = 4.3, bg = "lemonchiffon", units = "in", res = 180)
plot(mars, main = "", col = rev(brewer.pal(9, "YlOrBr")), key.pos = 1, key.width = lcm(1.2))
title("MARTE", family = "Exo_bold", line = 2.3, cex.main = 4, col.main = "brown")
mtext("MGS MOLA DEM", family = "Exo", line = 1.5, at = 0.5, cex = 1, col = "grey10")
mtext("@sporella", family = "Exo", line =-17.2, at = 1.04, cex = 0.72, col = "brown")
dev.off()
