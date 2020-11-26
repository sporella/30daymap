# #30DayMapChallenge
# Día 24: Elevación
# PaleoDEM
# Datos: https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/
# Autora: Stephanie Orellana (@sporella)

library(stars)
library(sf)
library(tidyverse)
library(rcartocolor)
library(animation)


# Descargar datos ---------------------------------------------------------

download.file("https://www.earthbyte.org/webdav/ftp/Data_Collections/Scotese_Wright_2018_PaleoDEM/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc.zip", 
              destfile = "./data/paleodem.zip")
untar("./data/paleodem.zip", exdir = "./data/")


l <- list.files("data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/", ".nc$", full.names = T)
desc <- read_csv("data/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/PaleoAtlasTimeIntervalsv22b copy.csv", skip = 1) %>% 
  drop_na()

# Tema --------------------------------------------------------------------

theme_myvoid <- function(back_colour = "#420b41", text_colour = "grey50") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 10, colour = text_colour),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 14,
      hjust = 0.5,
      vjust = -3,
      face = "bold"
    ),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 8),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
}


# Guardar GIF -------------------------------------------------------------
# 20 min aprox.


saveGIF({map(l, function(x){

dem <- read_stars(x) %>% 
  as.data.frame() %>%
  rename(z = 3) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_rasterize() %>% 
  st_as_sf %>% 
  mutate(z = ifelse(z < 0, NA, z))



number <- as.numeric(str_match(x, "Map(\\d+)")[2])
name <- desc$`Stratigraphic Age Description`[desc$`Map ID Number` == number]

s <- dem %>% st_rasterize()

con <- st_contour(s, na.rm = T, contour_lines = T, breaks = seq(0, max(s$z, na.rm=T), length.out=10))

p <- ggplot() +
  geom_sf(data = dem, aes(fill = z), colour = "transparent") +
  geom_sf(
    data = con,
    show.legend = F,
    colour = "grey60",
    fill = "transparent",
    size = 0.01
  ) +
  scale_fill_gradientn(colours = carto_pal(n = 7, name = "BrwnYl"), na.value = "paleturquoise3", breaks = round(seq(0, max(s$z, na.rm = T), length.out=4))) +
  labs(fill = "Elevation [m]", title = name, caption = "@sporella", tag = "Data: paleoDEM")+
  coord_sf(crs = "+proj=moll") +
  theme_myvoid(back_colour = "#fbffe0")+
  guides(
    fill = guide_colorbar(
      title.position = "top",
      label.position = "bottom",
      label.theme = element_text(size = 10, colour = "grey50", hjust = 0.5),
      barheight = unit(2, "mm"),
      barwidth = unit(40, "mm")
    )
  )
print(p)
})},  movie.name = "24_paleodem.gif", ani.width = 700, ani.height = 500, ani.res=90, interval = 0.5, autobrowse = FALSE)

file.rename("24_paleodem.gif", "plots/24_paleodem.gif")

