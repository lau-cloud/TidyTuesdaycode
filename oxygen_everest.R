library(ggplot2)
library(tidyverse)
library(ggtext)
library(forcats)
library(extrafont)

#loading data
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

#tidy
dfo <- members %>% 
  filter(peak_name == "Everest") %>% 
  group_by(sex, season, solo, died, year, oxygen_used) %>% 
  summarize(n()) %>%
  rename(numero = 'n()')

dfof <- dfo %>% 
  filter(year == 2015)

#convert logical to factor
dfo$oxygen_used <- as.factor(dfo$oxygen_used)

#plot
ggplot(dfo, aes(x = year, y = numero, fill = fct_rev(oxygen_used))) +
  geom_bar(stat = "identity", alpha = 0.8) +
  xlim(1924, 2019) +
  scale_fill_manual(values= c("#393b44", "white"), labels = c("YES", "NO")) +
  scale_y_continuous(position = "right", expand = c(0, 0)) +
  theme_minimal() +
  labs(title = "Everest, give me air!",
       subtitle = "Every member of every expedition (failed or not) \nto the mount Everest, by Oxygen used or not",
       x = "",
       y = "members",
       fill = "Oxygen?",
       caption = "Data from The Himalayan Database | @LauraNavarroSol") +
  theme(plot.background = element_rect(fill = "#80bdab", color = NA),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(color = "white", size = 30, family = "Constantia", hjust = 0.11, vjust = -0.25),
        plot.subtitle = element_text(color = "white", size = 12, family = "Leelawadee UI", hjust = 0.11, vjust = -0.30),
        axis.text = element_text(size = 8, color = "white"),
        axis.title.y = element_text(size = 8, color = "white", hjust = 0),
        plot.caption = element_text(size = 8, color = "white"),
        legend.title = element_text(size = 10)) +
  annotate(
    geom = "curve", x = 1947, y = 275, xend = 1951, yend = 55, 
    curvature = .3, arrow = arrow(length = unit(2, "mm")), color = "#393b44"
  ) +
  annotate(geom = "text", x = 1925, y = 355, 
           label = "Tom Bourdillon and Charles Evans \nused closed-circuit oxygen apparatus", 
           hjust = "left", color = "#393b44", size = 3.5)


ggsave("everest.png", height = 7, width =8, dpi = 300)
