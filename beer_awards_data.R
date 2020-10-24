library(tidyverse)
library(extrafont)
library(ggtext)
library(geofacet)
library(reshape2)

#data
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
glimpse(beer_awards)

#### tidy and count cases
scater_data <- beer_awards %>% 
  mutate(state = toupper(state)) %>%
  group_by(state, medal, year) %>%
  summarise(total = n())

#order factor levels of medals
scater_data$medal <- factor(scater_data$medal, levels = c("Gold", "Silver", "Bronze"))

#plot
bubbles <- ggplot(scater_data, aes(x = medal, y = year, size = total, color = medal)) +
  geom_jitter(alpha = 0.8, width = 0.25)+
  scale_size(range = c(0.2,2.4), name = "medals:") +
  scale_y_continuous(breaks= c(1987, 2020)) +
  theme_minimal()  +
  scale_color_manual(values = c("white", "#F5F5F5", "#FFEEB3"), name = "") +
  facet_geo(~ state, grid = "us_state_grid2", label = "code") +
  labs(title = "Beer Awards", subtitle = "Size of circles according to the total number of medals\n won from 1987 to 2020, by state of US", 
       y = "", x = "", caption = "Viz: Laura Navarro Soler | Data: Great American Beer Festival" ) +
  theme(plot.margin = unit(c(1.2,1.2,0.3,0.5), "lines")) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_blank(),
        plot.background = element_rect(color = "#ECBF2E", fill = "#ECBF2E"),
        axis.text.y = element_text(size = 7, color = "white"),
        plot.title = element_text(family = "Leelawadee UI", size = 20, 
                                  hjust= 0, face = "bold"),
        panel.grid = element_blank(),
        strip.text.x = element_text(
          size = 9, color = "black"),
        legend.position = "bottom",
        plot.subtitle = element_text(family = "Leelawadee UI", size = 12, 
                                     hjust= 0, margin = unit(c(0.5, 0.5, 1, 0.5), "lines")),
        legend.text = element_text(family = "Leelawadee UI", size = 8),
        legend.title = element_text(family = "Leelawadee UI", 
                                    face = "bold", size = 9),
        plot.caption = element_text(family= "Leelawadee UI", size = 10, color = "white",
                                    hjust = 1))
#legend bigger
finalplot <- bubbles + guides(colour = guide_legend(override.aes = list(size=4)))

#saving
ggsave("beer_awards2.png", 
       finalplot, 
       height = 8, width = 8, 
       units = "in", dpi = 300)
