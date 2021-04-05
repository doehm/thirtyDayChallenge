library(ggforce)
library(tidyverse)
library(extrafont)
library(ggfx)
loadfonts(quiet = TRUE)
font_import("C:/Users/Dan/Downloads/Fonts/thewitcher")


igni <- tibble(
  sign = "Igni",
  x = c(0.5, 0, 1, 0.667),
  y = c(sqrt(3)/2, 0, 0, sqrt(3)/3),
  xend = lead(x),
  yend = lead(y)
) %>% 
  filter(!is.na(xend))

axii <- tibble(
  sign = "Axii",
  x = c(1, 0, 0.5, 0.833),
  y = c(sqrt(3)/2, sqrt(3)/2, 0, sqrt(3)/3),
  xend = lead(x),
  yend = lead(y)
) %>% 
  filter(!is.na(xend))

aard <- tibble(
  sign = "Aard",
  x = c(0.333, 0.5, 1, 0, 0.1667, 0.5),
  y = c(sqrt(3)/3, sqrt(3)/2, 0, 0, sqrt(3)/6, sqrt(3)/6),
  xend = lead(x),
  yend = lead(y)
) %>% 
  filter(!is.na(xend))

quen <- tibble(
  sign = "Quen",
  x = c(0.6667, 0.5, 0, 1, 0.833, 0.5),
  y = c(sqrt(3)/6, 0, sqrt(3)/2, sqrt(3)/2, sqrt(3)/3, sqrt(3)/3),
  xend = lead(x),
  yend = lead(y)
) %>% 
  filter(!is.na(xend))

yrden <- tibble(
  sign = "Yrden",
  x = c(0.333, 0, 1, 0, 1, 0.667),
  y = c(sqrt(3)/6, 0, 0, sqrt(3)/2, sqrt(3)/2, sqrt(3)/3),
  xend = lead(x),
  yend = lead(y)
) %>% 
  filter(!is.na(xend))


df <- bind_rows(
  yrden,
  quen,
  igni,
  axii,
  aard
)
df_text <- tibble(
  sign = c("Yrden", "Quen", "Igni", "Axii", "Aard"),
  x = seq(0, 8, 2) + 0.5,
  y = -0.5
)
ls <- 2
lss <- 0.5
pal <- list(
  axii = rgb(33, 233, 25, maxColorValue = 255)
)
ft <- "Sylfaen"
ggplot(df) +
  with_outer_glow(geom_segment(data = yrden, mapping = aes(x = x, xend = xend, y = y, yend = yend), colour = "purple", size = ls), colour = "purple", sigma = 20, expand = 10) +
  with_outer_glow(geom_segment(data = quen, mapping = aes(x = x+2, xend = xend+2, y = y, yend = yend), colour = "yellow", size = ls), colour = "yellow", sigma = 20, expand = 10) +
  with_outer_glow(geom_segment(data = igni, mapping = aes(x = x+4, xend = xend+4, y = y, yend = yend), colour = "red", size = ls), colour = "red", sigma = 20, expand = 10) +
  with_outer_glow(geom_segment(data = axii, mapping = aes(x = x+6, xend = xend+6, y = y, yend = yend), colour = pal$axii, size = ls), colour = pal$axii, sigma = 20, expand = 10) +
  with_outer_glow(geom_segment(data = aard, mapping = aes(x = x+8, xend = xend+8, y = y, yend = yend), colour = "cyan", size = ls), colour = "cyan", sigma = 20, expand = 10) +
  geom_segment(data = yrden, mapping = aes(x = x, xend = xend, y = y, yend = yend), colour = "white", size = lss) +
  geom_segment(data = quen, mapping = aes(x = x+2, xend = xend+2, y = y, yend = yend), colour = "white", size = lss) +
  geom_segment(data = igni, mapping = aes(x = x+4, xend = xend+4, y = y, yend = yend), colour = "white", size = lss) +
  geom_segment(data = axii, mapping = aes(x = x+6, xend = xend+6, y = y, yend = yend), colour = "white", size = lss) +
  geom_segment(data = aard, mapping = aes(x = x+8, xend = xend+8, y = y, yend = yend), colour = "white", size = lss) +
  with_outer_glow(geom_text(aes(x = 0.5, y = -0.5, label = "Yrden"), colour = "purple", size = 18, family = ft), colour = "purple", sigma = 20, expand = 10) +
  with_outer_glow(geom_text(aes(x = 2.5, y = -0.5, label = "Quen"), colour = "yellow", size = 18, family = ft), colour = "yellow", sigma = 20, expand = 10) +
  with_outer_glow(geom_text(aes(x = 4.5, y = -0.5, label = "Igni"), colour = "red", size = 18, family = ft), colour = "red", sigma = 20, expand = 10) +
  with_outer_glow(geom_text(aes(x = 6.5, y = -0.5, label = "Axii"), colour = pal$axii, size = 18, family = ft), colour = pal$axii, sigma = 20, expand = 10) +
  with_outer_glow(geom_text(aes(x = 8.5, y = -0.5, label = "Aard"), colour = "cyan", size = 18, family = ft), colour = "cyan", sigma = 20, expand = 10) +
  # with_outer_glow(geom_text(data = df_text, mapping = aes(x, y, label = sign), colour = c("purple", "yellow", "red", "green", "cyan"), size = 18), sigma = 20, expand = 10) +
  labs(caption = "Graphic: @danoehm")
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(t = 30, b = 30),
    plot.caption = element_text(colour = "grey50")
  ) +
  coord_cartesian(clip = "off") +
  ylim(c(-3, 3)) +
  xlim(c(-1, 10)) +
  ggsave("magical.png", height = 8, width = 12)

