library(tidyverse)
library(ggplot2)
library(extrafont)
library(scales)


#font_import(path = "C:/Users/Usuario/AppData/Local/Microsoft/Windows/Fonts")
#loadfonts(device = "win", quiet = TRUE)

theme_set(theme_minimal())


# data
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')


# analyse
colony_df <- colony %>% 
  filter(year == 2015 | year == 2020) %>%
  filter(state != "United States") %>% 
  group_by(state, year) %>% 
  summarize(mean_colony = mean(colony_n)) %>% 
  spread(year, mean_colony) %>% 
  rename("mean_2015" = "2015",
         "mean_2020" = "2020")


# Reorder data using average
colony_data <- colony_df %>% 
  rowwise() %>% 
  mutate(difference = mean_2020-mean_2015) %>% 
  arrange(difference) %>% 
  mutate(state=factor(state, state))



# Plot
ggplot(colony_data) +
  geom_rect(
    fill = "#FFA45B",
    xmin = 0,
    xmax = Inf,
    ymin = -5, 
    ymax = 19) + 
  geom_rect(
    fill = "#FFDA77",
    xmin = 0,
    xmax = Inf,
    ymin = 19, 
    ymax = 46) +
  geom_segment(aes(x=mean_2015, xend=mean_2020, y=state, yend=state), size = 0.8,
               arrow = arrow(length = unit(0.2, "cm")))+
 # geom_text(aes(x = mean_2020, y = state, label=state, hjust=1, vjust=1), size = 3,
 #           family = "mono") +
  geom_text(aes(x = mean_2020, y = state, label=difference, hjust=1.5, vjust=-0.3), size = 2) +
  geom_segment(aes(x=0, xend=mean_2015, y=state, yend=state), size = 0.1, linetype = "dotted")+
  scale_x_log10(breaks = c(1000, 3000, 10000, 50000, 300000, 1000000), labels = c("1000","3000","10000","50000","300000","1M"))+
  labs(y="", x = "number of colonies (anual mean)", 
       title = "How bee colonies have\n changed",
       subtitle = "Increase or decrease in the number of bee colonies (annual average)\n in 2020 compared to 2015",
       caption = "Horizontal axis in log10 scale | Data source: USDA | Graph: Laura Navarro Soler")+
  annotate("text", x = 500000, y = "Maine", label = "WON BEES", color = "black", size = 7,family = "mono") +
  annotate("text", x = 500000, y = "Kansas", label = "LOST BEES", color = "black", size = 7,family = "mono") +
  theme(
    legend.position = "top",
    plot.background = element_rect(color = "white", fill = "white"),
    panel.background = element_rect(color = "white", fill = "white"),
    axis.title.y = element_blank(),
   # axis.text.y = element_blank(),
   # panel.grid.minor = element_blank(),
   panel.grid = element_blank(),
    text = element_text(size = 8, color = "darkslategrey", face = "bold", family = "mono"),
    plot.title = element_text(size = 28, color = "black", margin = margin(t = 30,b = 10), hjust = 0.5, family = "serif"),
    plot.subtitle = element_text(size = 12, color = "black", margin = margin(b = 25, l = -25), family = "serif",
                                 hjust = 0.5),
    plot.caption = element_text(size = 8, margin = margin(t = 10), color = "darkslategrey", hjust = 1))


# saving
ggsave(
  "bee_colonies.png", 
  height = 8, width = 7, 
  units = "in", 
  dpi = 300
)

