library(tidyverse)
library(survivoR)
library(extrafont)
library(gggibbous)
loadfonts(quiet = TRUE)
font_import()

df <- castaways %>% 
  filter(str_detect(result, "evac")) %>% 
  mutate(
    full_name = fct_reorder(toupper(full_name), season, max, desc = TRUE),
    season_name = ifelse(str_detect(season_name, "K"), "Survivor: Koah Rong", season_name)
    )

df %>% 
  ggplot() +
  geom_bar(data = df, mapping = aes(x = full_name, y = 39), stat = "identity", colour = "grey50", fill = NA, alpha = 0.4) +
  geom_bar(data = df, mapping = aes(x = full_name, y = day), stat = "identity", fill = "#6d6875", alpha = 1) +
  # geom_text(data = df, mapping = aes(x = full_name, y = pmax(5, day) + 1, label = day)) +
  geom_text(data = df, mapping = aes(x = full_name, y = 1, label = paste(full_name, "\n", season_name)), fontface = "italic", hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffe8d6")
  ) +
  # coord_polar("y") +
  coord_flip() +
  ggsave("day1.png", height = 14, width = 7)

sb <- "The tale of the 15 castaways that had their Survivor journey cut short by injury and being 
medically evacuated from the island. Only a few make it the full 39 days. Most are voted out. These
souls were denied even of that" %>% 
  str_wrap(40)

df %>% 
  ggplot() +
  geom_moon(data = df, mapping = aes(x = full_name, y = 1, ratio = day/39, right = FALSE), fill = "grey90", size = 40) +
  geom_text(data = df, mapping = aes(x = full_name, y = 1.4, label = paste(day, "days\n", full_name, "\n", toupper(str_remove(season_name, "Survivor: ")))), 
            family = "Survivor Font", fontface = "italic", vjust = 0.5, hjust = 0.5, colour = "grey90", size = 8,
            lineheight = 0.7) +
  geom_text(aes(x = 0, y = 0.2, label = "CUT\nSHORT"), size = 40, colour = "grey90", face = "bold", 
            family = "Survivor Font", lineheight = 0.65) +
  geom_text(aes(x = 2.5*pi, y = 0.25, label = sb), size = 6, colour = "grey90", face = "bold", 
            family = "Segoe UI Light", lineheight = 0.9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    strip.text = element_text(colour = "grey90", face = "italic"),
    plot.caption = element_text(family = "Segoe UI Light", colour = "grey90", size = 16, hjust = 0.5)
  ) +
  coord_polar() +
  labs(
    caption = "Source: {survivoR} / @danoehm"
  ) +
  ylim(c(0, 1.4)) + 
  ggsave("day1.png", height = 14, width = 13.71)
