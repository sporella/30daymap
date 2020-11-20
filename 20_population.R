# #30DayMapChallenge
# Día 20: Población
# Expectativa de vida en 2019
# Fuente datos: https://ourworldindata.org/life-expectancy
# Autora: Stephanie Orellana (@sporella)

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

pop <- read_csv("data/life-expectancy.csv") %>%
  janitor::clean_names() %>%
  drop_na() %>%
  filter(year == 2019)

world <- ne_countries(scale = "medium", returnclass = "sf")

world2 <-
  world %>%  select(iso_a3) %>% left_join(pop, by = c("iso_a3" = "code")) %>%
  mutate(lab = cut_width(
    life_expectancy,
    width = 10,
    closed = "right",
    boundary = 50
  ))


theme_world <- function(back_colour = "#420b41", line_colour = "grey70") {
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(fill = back_colour, colour = back_colour),
    legend.background = element_blank(),
    panel.grid = element_line(
      colour = line_colour,
      linetype = "dotted",
      size = 0.4
    ),
    panel.ontop = T,
    legend.position = "left",
    text = element_text(size = 8, colour = "grey60"),
    strip.text = element_text(
      size = 8,
      colour = "grey60",
      face = "bold"
    ),
    strip.background = element_rect(fill = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.ticks = element_line(colour = line_colour),
    axis.text = element_text(colour = line_colour),
    plot.tag.position = "bottom",
    plot.tag = element_text(size = 5)
  )
}


p <- ggplot() +
  geom_sf(data = world2,
          aes(fill = lab),
          colour = "grey75",
          size = 0.2) +
  scale_fill_manual(
    values = c("#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"),
    guide = "legend",
    na.translate = F
  ) +
  labs(title = "EXPECTATIVA DE VIDA EN 2019", caption = "@sporella") +
  coord_sf(crs = "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs",
           label_graticule = "NSWE") +
  theme_world(back_colour = "#efedff", line_colour = "cadetblue") +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    plot.title = element_text(
      size = 15,
      colour = "grey70",
      face = "bold",
      hjust = 0.5,
      vjust = -2
    )
  ) +
  guides(fill = guide_legend(
    title = NULL,
    keyheight = unit(2, "mm"),
    keywidth = unit(3, "mm"),
    nrow = 1,
    title.position = "top"
  ))

ggsave("plots/20_expectativas.png",
       p,
       width = 6,
       height = 6,
       bg = "#efedff")
