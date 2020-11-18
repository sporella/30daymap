# #30DayMapChallenge
# Día 17: Histórico
# Sudamérica 1830
# Fuente datos: https://3.bp.blogspot.com/-CewMSzUoZP8/XIo-XvjYFzI/AAAAAAAAUvU/VrKULBMJWUUTSddwV_2WJzDRgwSqNdlTgCLcBGAs/s1600/sudamerica1830.jpg
# Autora: Stephanie Orellana (@sporella)

library(sf)
library(tidyverse)
library(extrafont)
library(ggimage)

loadfonts()

sud <- read_sf("data/sudamerica1830.geojson") %>% 
  mutate(angle = case_when(str_detect(pais, "Guayana") ~ 80,
                           str_detect(pais, "Rio") ~ -30,
                           pais == "Chile" ~ 90,
                           TRUE ~ 0),
         wrap = case_when(str_detect(pais, "Guayana")~ pais,
                          str_detect(pais, "Rio") ~ pais,
                          TRUE ~ str_wrap(pais, width = 4)))

oceanos <- data.frame(
  name = c("Pacífico Sur", "Atlántico Sur"),
  geometry = st_sfc(st_point(c(-90, -30)),
                    st_point(c(-30, -25)),
                    crs = 4326
  )) %>%
  st_as_sf()

colores <- c("#cb9e8e",
             "#6cd75d",
             "#d95271",
             "#c9d945",
             "#d5654a",
             "#75d5ce",
             "#e14330",
             "#548c42",
             "#893539",
             "#b3dba2",
             "#bc5637",
             "#578c83",
             "#d98d36",
             "#31442b",
             "#c1b058",
             "#7f6434")
p <- ggplot() +
  geom_sf(data = sud,
          aes(fill = pais),
          show.legend = F,
          size = 0.4,
          colour = "grey60",
          alpha = 0.6,
          linetype = "dashed"
  ) +
  geom_sf_text(data = sud,
               aes(label = wrap , angle = angle),
               size = 3.5,
               show.legend = F,
               nudge_y = 0.5,
               family = "Vladimir Script",
               colour = "grey30"
  ) +
  geom_sf_text(data = oceanos,
               aes(label = name),
               size = 6,
               show.legend = F,
               nudge_y = 0.5,
               family = "Vladimir Script",
               colour = "grey40",
               angle = 80
  ) +
  scale_fill_manual(values = colores) +
  labs(title = "Sudamérica ~1830", caption = "@sporella") +
  theme(text = element_text(family =  "Vladimir Script", colour = "grey45"),
        plot.title = element_text(size = 30, hjust = 0.5),
        plot.caption= element_text(size = 8),
        axis.title = element_blank(),
        panel.grid = element_line(colour = "grey33", size = 0.4, linetype = "dotted"),
        panel.ontop = T)+
  coord_sf(crs = "+proj=aeqd +lat_0=0 +lon_0=-55 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
           xlim = c(-2371873 - 2000000, 1387906 + 2000000))

img <- "img/pergamino.jpg"

p <- ggbackground(p, img)


ggsave(filename = "plots/17_sudamerica1830.png", plot = p, width = 6, height = 6, bg = "lemonchiffon")
