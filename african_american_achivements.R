library(tidyverse)
library(ggplot2)
library(extrafont)
font_import()

#loading data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')


# ggplot
colors <- c("grey", "purple")
ggplot(firsts, aes(x = year, y = category))+
  geom_jitter(aes(colour = gender, alpha = 0.8), width = 0.2, height = 0.3) +
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2019), position = "top") +
  scale_colour_manual(labels = c("male", "female"), values=colors)+
  labs(caption= "Source: Wikipedia | David Blackwell & Petition for David Blackwell | Plot by @LauraNavarroSol",
       x="", 
       y="",
       subtitle = "First Achievements from 1738 to 2019",
       title="African-American Achievements") +
  theme(panel.background = element_rect("black"),
        panel.grid.major.x = element_line("#616161", linetype="dashed", size = 0.05),
        panel.grid.minor = element_line("black"),
        panel.grid.major.y = element_line("black"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "black"),
        axis.ticks = element_blank(),
        text = element_text(family = "Serif"),
        axis.text.y = element_text(color="#666666", 
                                  size=14, family = "Serif"),
        axis.text.x = element_text(color = "#666666", size = 13, family ="Sans"),
        plot.title = element_text(family = "Serif", color = "white", size = 16),
        plot.subtitle = element_text(family = "Serif", color = "#666666", size = 14),
        plot.caption = element_text(color = "#666666", size = 8),
        legend.title=element_blank(),
        legend.background = element_rect(fill="black", size=.5),
        legend.key=element_blank(),
        legend.text = element_text(color = "#666666", size = 12)
        ) +
  scale_alpha_continuous(guide=FALSE)

#saving
ggsave("black_achiv.png", device = "png", type = "cairo", width = 8, height = 6, dpi = 300)
ggsave("black_achiv2.png", device = "png", type = "cairo", width = 10, height = 5, dpi = 300)

#Data to bring to light black american achievements, to pay 
# tribute to #BlackLivesMatter and to keep with #TidyTuesday.
#rstats #ggplot2 #ddj #dataviz
