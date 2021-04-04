# day 3

library(tidyverse)
library(survivoR)
library(extrafont)
library(ggimage)
library(lubridate)
library(ggfx)
loadfonts(quiet = TRUE)

start <- now()


#### fonts ####
ft <- "Verdana Pro Cond Light"

viewers
df_long <- season_summary %>% 
  select(season, viewers_finale, viewers_premier) %>% 
  pivot_longer(cols = -season, names_to = "Episode", values_to = "viewers") %>% 
  mutate(Episode = ifelse(Episode == "viewers_finale", "Finale", "Premier"))

df_min <- df_long %>% 
  group_by(season) %>% 
  summarise(min = min(viewers)) %>% 
  mutate(y0 = 0)

title1 <- "From season 24 the number of viewers tuning in for the Surivor season premier is the same as the finale"
title2 <- "While Survivor viewers as a whole are on a decreasing trend, it is flattening out and those that do watch tend to stay for the whole season"
title1 <- str_wrap(title1, 40)
title2 <- str_wrap(title2, 60)

season_summary %>% 
  select(season, viewers_finale, viewers_premier) %>% 
  ggplot(aes(x = season)) +
  with_inner_glow(geom_ribbon(aes(ymin = viewers_finale, ymax = viewers_premier), fill = "white", alpha = 0.6)) +
  with_outer_glow(geom_line(data = df_long, mapping = aes(x = season, y = viewers, colour = Episode)), colour = "white", sigma = 10) +
  with_outer_glow(geom_segment(data = df_min, mapping = aes(x = season, xend = season, y = y0, yend = min)), colour = "white", sigma = 10) +
  with_outer_glow(geom_text(aes(x = 30, y = 40, label = title1), family = ft, colour = "grey50", size = 5, face = "bold"), colour = "white", sigma = 1) +
  with_outer_glow(geom_text(aes(x = 30, y = 30, label = title2), family = ft, colour = "grey50", size = 3, face = "bold"), colour = "white", sigma = 1) +
  with_outer_glow(geom_text(aes(x = 3, y = 51, label = "50 million"), family = ft, colour = "grey50", size = 3, face = "bold"), colour = "white", sigma = 1) +
  with_outer_glow(geom_text(aes(x = 40, y = 10, label = "10 million"), family = ft, colour = "grey50", size = 3, face = "bold"), colour = "white", sigma = 1) +
  scale_colour_manual(values = c("purple", "cyan")) +
  scale_x_continuous(breaks = c(20, 40), labels = c(20, 40)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = "grey50", family = ft, face = "bold"),
    legend.position = "bottom",
    # axis.title = element_text(),
    axis.text.x = element_text(size = 6)
  ) +
  ggsave("historical.png", width = 10, height = 5)

