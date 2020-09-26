library(ggplot2)
library(tidyverse)
library(ggtext)
library(forcats)
library(extrafont)


members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


#tidy
df <- members %>% 
  filter(peak_name == "Everest") %>% 
  group_by(sex, season, solo, died, year) %>% 
  summarize(n()) %>%
  rename(numero = 'n()')


ggplot(df, aes(x = year, y = numero, fill = fct_rev(sex))) +
  geom_bar(stat = "identity", alpha = 0.8, bin = 100) +
  xlim(1940, 2020) +
  scale_fill_manual(values= c("#393b44", "white")) +
  theme_minimal() +
  labs(title = "EVEREST: Make room!", 
       subtitle = "Members of all expeditions to the mount Everest, by sex",
       x = "",
       y= "members",
       caption = "Data from The Himalayan Database | @LauraNavarroSol") +
  theme(plot.background = element_rect(fill = "#80bdab"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(color = "white", size = 20),
        plot.subtitle = element_text(color = "white", size = 14)
        
  )
  
  
  
  
