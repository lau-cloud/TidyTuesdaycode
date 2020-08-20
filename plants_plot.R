library(tidyverse)
library(ggplot2)
library(extrafont)
library(ggridges)
library(cowplot)

#loading data
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')

#tidy
plants$year_last_seen <- factor(plants$year_last_seen, 
                                levels = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"))

p_plants <- plants %>%
  group_by(continent, group, year_last_seen, red_list_category) %>% 
  summarize(count = n()) %>% 
  filter(!is.na(year_last_seen))

p_plants$red_list_category <- as.factor(p_plants$red_list_category)

p2_plants <- plants %>% 
  group_by(group, red_list_category) %>% 
  summarize(count = n())


#Plot 1

plot1 <- ggplot(p_plants, aes(x = year_last_seen, y = continent)) +
  geom_point(shape =23, aes(size = count, fill = red_list_category)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "left") +
  scale_size(
    range = c(2, 15), 
    breaks = c(10, 30, 50),
    name = "Extinct plant species"
  ) +
  scale_fill_manual(values = c("#3B4B42", "#FCF063"), name = NULL) +
  labs(shape="Exctinct plant species", colour= NULL,
    x = NULL,
    y = NULL,
    title = "Exctinct Plant Species in the world",
    subtitle = "Just like animals, plants are going through an important biodiversity crisis. In total, 500 plant species\n are considered extinct as of 2020. 19.6% of those were endemic to Madagascar, 12.8% to Hawaiian islands.\n") +
  theme_minimal() +
  theme(plot.title = element_text(family="Constantia",
                                  face = "plain",
                                  hjust = 0.5,
                                  margin = margin(15, 0, 5, 0),
                                  size = 24),
        plot.subtitle = element_text(family = "Constantia",
                                     hjust = 0.5,
                                     size = 10,
                                     color = "grey55",
                                     margin = margin(2, 0, 30, 0)),
        panel.grid.major.y = element_line(color = "grey55", size = 0.5),
        legend.title = element_text(size = 8, color = "grey40"),
        legend.text = element_text(size = 6),
        legend.key=element_blank(),
        legend.margin = margin(),
        axis.text.x = element_text(size = 8, 
                                   family = "Leelawadee UI"),
        axis.text.y = element_text(family = "Leelawadee UI",
                                   size = 12, 
                                   hjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 3.5)))


#plot2

plot2 <- ggplot(p2_plants, aes(x = count*-1, y = reorder(group, count), color = red_list_category, fill = red_list_category)) +
  geom_bar(stat = "identity", width = 0.4, alpha = 0.4, size = 1.3) +
  scale_color_manual(values = c("#3B4B42", "#FCF063")) +
  scale_fill_manual(values = c("#3B4B42", "#FCF063")) +
  scale_x_continuous(breaks = c(0, -100, -200, -300, -400),
                                labels =c("0", "100", "200", "300", "400")) +
  coord_flip() +
  theme_minimal() +
  labs( x = NULL,
        y = NULL,
        caption = "@LauraNavarroSol | Data: IUCN Red list of Threatened Species (Version 2020-1)"
  ) +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5,0,1,0), "cm"),
        axis.text.x = element_blank(),
        plot.caption = element_text(size = 8,
                                    color = "grey75",
                                    vjust = -6))


#loading images of plants
ggdraw() +
  draw_image("conifer.png", width = 1, height = 1) +
  draw_text("CONIFER", size = 9, y = 0.05, family = "Leelawadee UI Semilight")-> conifer_img
ggdraw() +
  draw_image("algae.png", width = 1, height = 1) +
  draw_text("ALGAE", size = 9, y = 0.01, family = "Leelawadee UI Semilight")-> algae_img
ggdraw() +
  draw_image("cycad.png", width = 1, height = 1) +
  draw_text("CYCAD", size = 9, y = 0.01, family = "Leelawadee UI Semilight")-> cycad_img
ggdraw() +
  draw_image("mosses.png", width = 1, height = 1) +
  draw_text("MOSSES", size = 9, y = 0.01, family = "Leelawadee UI Semilight")-> mosses_img
ggdraw() +
  draw_image("falgae.png", width = 1, height = 1) +
  draw_text("FERNS AND ALLIES", size = 9, y = 0.05, family = "Leelawadee UI Semilight")-> ferns_img
ggdraw() +
  draw_image("flowering.png", width = 1, height = 1) +
  draw_text("FLOWERING", size = 9, y = 0.01, family = "Leelawadee UI Semilight")-> flowering_img



#grid of images
image_col <- plot_grid(conifer_img, 
                       algae_img,
                       cycad_img,
                       mosses_img,
                       ferns_img, 
                       flowering_img,
                       ncol=6, nrow = 1)

image_col

#title second plot
ggdraw() +
  draw_text("Extinct by taxonomy",
            size = 14, x = 0.4, y = 0.1, hjust = 0, vjust = -0.7,
            family = "Constantia") -> middle_text

middle_text

#put together images and graph
taxonomy_plot <- plot_grid(image_col, plot2, ncol=1, rel_widths = c(0.8, 4),
                           rel_heights = c(0.7, 2.5), margin(1, 0, 0, 0))


#plot it all
final_plot <- plot_grid(
  
  plot1,
  
  middle_text,
  
  taxonomy_plot,
  
## plot settings
  rel_heights = c(5,0.3,4),
  nrow = 3
)


#saving
ggsave("plant_species_plot.png", 
       final_plot, 
       height = 8, width = 7, 
       units = "in", dpi = 300)
