library(tidyverse)
library(janitor)
library(ggtext)
library(geofacet)
library(extrafont)
library(statebins)
#install.packages("scico") -> https://github.com/thomasp85/scico
library(scico)


# data
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv') %>% clean_names()

nurses$st <- state.abb[match(nurses$state, state.name)]

df1 <- nurses %>% 
  filter(year == 2020) %>% 
  arrange(hourly_wage_median) %>% 
  filter(state != "Guam")

#state bins

p1 <- ggplot(df1, aes(state=state, fill = hourly_wage_median)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_scico(palette = "tokyo",
                   direction = -1,
                   name = "Hourly Wage Median", 
                   limits=c(10,60), 
                   breaks = c(0,20,40,60),
                   labels = c("0$","20$", "40$", "60$"),
                   guide = "colorbar") +
  labs(title = "Hourly Wage of Nurses, by state (2020)",
       subtitle = "Which has been the hourly wage median of nurses in US\n in the year of the Covid-19 outbreak", caption = "Data from Data.World | Visualization: Laura Navarro Soler") +
  theme_void() +
  theme(legend.position = "top") +  
 # guides(fill = guide_legend(direction = "horizontal")) +
  theme(text = element_text(family = "Ebrima"),
        legend.position = c(0.5, 0.92),
        legend.key.width = unit(30, 'pt'),
        legend.key.height = unit(5, 'pt'),
        legend.direction = "horizontal",
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        panel.spacing = unit(1, 'pt'),
        plot.title = element_text(size = 16, hjust = 0.5, family = "Ebrima Bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  annotate(geom="richtext", x=0.5, y=-4, label="California <br> <span style='color: #4C185E;'>56,9 $</span>", 
           size = 3, fill = NA, label.color = NA) +
  geom_curve(
    aes(x = 0.5, y = -4.5, xend = 1.5, yend = -5), arrow = arrow(length = unit(0.015, "npc"))
  )

## saving

ggsave("nurses_salary.png", 
       p1, 
       height = 7, width = 8, 
       units = "in", dpi = 300)
