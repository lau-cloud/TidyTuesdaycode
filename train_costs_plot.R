library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(extrafont)

#read the data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

#tidy
train_cost <- transit_cost %>% 
  filter(!is.na(e)) %>% 
  
# remove outliers_length
filter(length <= 200) %>% 
mutate(highlight_flag = ifelse(country == "ES", T, F))

#convert characters to factors
train_cost$real_cost <- as.numeric(train_cost$real_cost)
transit_cost$real_cost <- as.numeric(transit_cost$real_cost)

## length each country
class(transit_cost$start_year)
transit_cost$start_year <- as.numeric(transit_cost$start_year)
transit_cost$end_year <- as.numeric(transit_cost$end_year)

#analysis
transit_cost %>% 
  mutate(total_years = end_year-start_year) %>% 
  select(country, city, line, start_year, end_year, length, real_cost, cost_km_millions, total_years) %>% 
  arrange(desc(total_years)) %>% 
  filter(total_years >5) %>% 
  count(country) %>% 
  arrange(desc(n))

top_countries <- c("IN", "JP", "US", "CN", "TR", "TW", "CA", "ES")

long_trains <- transit_cost %>% 
  mutate(total_years = end_year-start_year) %>% 
  select(country, city, line, start_year, end_year, length, real_cost, cost_km_millions, total_years) %>% 
  arrange(desc(total_years)) %>% 
  filter(total_years >5 & country %in% top_countries)


various_countries <- long_trains %>% 
  mutate(name = fct_reorder(line, total_years)) %>%
  ggplot(aes(x = total_years, y = name, color = real_cost)) +
  geom_segment(aes(x=0, xend=total_years, y=name, yend=name, size = length)) +
  facet_wrap(~factor(country), ncol = 4) +
  xlim(0, 25) +
  coord_polar() +
  labs(title = "Long constructions (no light at the end of the tunnel)", 
       subtitle = "This visualization shows the six countries around the world that have the longest constructions of train rails.\n Turkey (TR) and Spain (ES) leader with M2-M6 and L9/10 respectively",
       x = "Years", size = "Length in km",
       color = "cost, in Millions of USD", caption = "Data: Transit Costs Project | Visualization: Laura Navarro Soler") +
  scale_color_gradient(low = "pink", high = "red", na.value = NA) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_rect(
          color="white", fill="white", size=1),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9, color = "grey", family = "Myanmar Text"),
        legend.position = "bottom",
        legend.title = element_text(size=11, family = "Myanmar Text"), 
        legend.text=element_text(size=10, family = "Myanmar Text"),
        legend.key.size = unit(0.7, 'cm'),
        axis.title.x = element_text(size = 11, color = "grey", family = "Myanmar Text"),
        plot.caption = element_text(size = 11, hjust = 0.5,color = "grey", family = "Myanmar Text"),
        plot.title = element_text(size = 26,hjust = 0.5, family = "Myanmar Text"),
        plot.subtitle = element_text(size = 14,hjust = 0.5, family = "Myanmar Text")) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5),
         size = guide_legend(title.position="top", title.hjust = 0.5,override.aes=list(colour="pink")))

various_countries


#saving
ggsave("long_trains.png", 
       various_countries, 
       height = 10, width = 10, 
       units = "in", dpi = 300)


