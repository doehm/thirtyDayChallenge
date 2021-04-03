library(tidyverse)
library(survivoR)
library(extrafont)
library(ggimage)
loadfonts(quiet = TRUE)

start <- now()

im_df <- immunity %>% 
  unnest(immunity) %>% 
  filter(!immunity %in% tribe_colours$tribe) %>% 
  arrange(season, order) %>% 
  mutate(
    rank = 1:n(),
    x = (rank - 1) %% 20 + 1,
    y0 = as.numeric(x == 1),
    y = cumsum(y0),
    image = ifelse(immunity == voted_out & !is.na(immunity), "C:/Users/Dan/Pictures/survivor/skull.png", "C:/Users/Dan/Pictures/survivor/flame2.png"),
    xn = 0,
    yn = 0
  )

sb <- str_wrap(
  "Individual immunity has been won 376 times over 40 seasons of Survivor and shown in seqence from the first. 
  It grants you immunity from the vote and free passage to the next challenge... unless you give it away and get voted out.
  This has happened twice. Most famously Erik gave his immunity necklace to Natalie in Survivor: Micronesia and 
  Brandon gave his immunity necklace to Albert in Survivor: South Pacific. They have a special place in history."
  , 140)

bone_head <- im_df %>% 
  filter(str_detect(image, "skull")) %>% 
  mutate(
    xend = x,
    yend = 21
  )

a <- 10
bg <- rgb(230 + a, 190 + a, 142 + a, maxColorValue = 255)
im_df %>% 
  ggplot(aes(x, -y/2, image = image)) +
  # geom_segment(data = bone_head, mapping = aes(x = x, xend = xend, y = -y/2, yend = -yend/2), linetype = 2) +
  # geom_segment(data = bone_head, mapping = aes(x = x, xend = xend+1, y = -yend/2, yend = -yend/2), linetype = 2) +
  # geom_text(data = bone_head, mapping = aes(x = xend+1.5, y = -yend/2, label = immunity), family = "Survivor Font", size = 6, hjust = 0) +
  geom_image() + 
  labs(
    title = "INDIVIDUAL IMMUNITY",
    subtitle = sb,
    caption = "#30dayChartChallenge / Day 2 / Source: {survivoR} / @danoehm"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 20, b = 20, r = 20, l = 20),
    strip.text = element_blank(),
    plot.background = element_rect(fill = bg),
    plot.title = element_text(family = "Survivor Font", size = 32, hjust = 0.5),
    plot.subtitle = element_text(family = "Verdana Pro Cond Light", size = 7, hjust = 0.5),
    plot.caption = element_text(family = "Verdana Pro Cond Light", size = 7, hjust = 0.5)
  ) +
  ggsave("day2.png", height = 7, width = 7)

finish <- now() - start
# 42m