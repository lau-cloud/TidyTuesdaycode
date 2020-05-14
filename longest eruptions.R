library(tidyverse)
library(dplyr)
library(hrbrthemes)

# load data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# explore
head(eruptions)
length(unique(eruptions$volcano_name))
unique(eruptions$start_year)

# get a date
eruptions$date_start <- as.Date(with(eruptions, paste(start_year, start_month, start_day,sep="-")), "%Y-%m-%d")
eruptions$date_end <- as.Date(with(eruptions, paste(end_year, end_month, end_day,sep="-")), "%Y-%m-%d")

# get the days of the eruption, order and filter from 1800
eruptions$days_eruption <- eruptions$date_end - eruptions$date_start

eruptions_20 <- eruptions %>%
  arrange(days_eruption) %>%
  filter(start_year > 1800 ) %>%
  top_n(20)

eruptions_20$volcano_name <- factor(eruptions_20$volcano_name, levels=unique(eruptions_20$volcano_name))

# lollipop plot
eruptions_plot <- eruptions_20 %>%
  ggplot() +
  geom_segment( aes(x = volcano_name, xend = volcano_name, y = date_start, yend = date_end), 
                size = 2, alpha = 0.4, color = "orange") + 
  geom_point( aes(x = volcano_name, y = date_start), color = "#F74604", fill = "#F74604", size = 3, 
              shape = 24, alpha = 0.7 ) +
  geom_point( aes(x = volcano_name, y = date_end), color = "#F74604", fill = "#F74604", size = 3, 
              shape = 24, alpha = 0.7 ) +
  coord_flip()+
  ggtitle("L  o  n  g  e  s  t   eruptions since 1800") +
  theme(
    legend.position = "none",
    panel.background = element_rect("white"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face="bold", size=9),
    axis.text.x = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line("#EEEAE7"),
    axis.title.x = element_text(size = 8),
    plot.title = element_text(size = 14, hjust = 0.5, margin=margin(0,0,30,0))
  ) +
  xlab("") +
  ylab("")

eruptions_plot  

  