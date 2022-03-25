
#libraries
extrafont::loadfonts(device="win")
library(tidyverse)
library(ggridges)
library(viridis)
library(extrafont)
library(cowplot)

theme_set(theme_light())

#loading data
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

range(babynames$year)


#50 most popular female names over all time
top_female <- babynames %>% 
  filter(sex == "F") %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  slice_max(total, n = 50) %>% 
  mutate(rank = 1:50, 
         name = forcats::fct_reorder(name, -total)) %>% 
  pull(name)

top_female


#50 most popular male names over all time
top_male <- babynames %>% 
  filter(sex == "M") %>% 
  group_by(name) %>% 
  summarise(total = sum(n)) %>% 
  slice_max(total, n = 50) %>% 
  mutate(rank = 1:50, 
         name = forcats::fct_reorder(name, -total)) %>% 
  pull(name)

top_male


# filter female names data
female_names <- babynames %>% 
  filter(sex == "F", 
         name %in% top_female) %>% 
  mutate(name = factor(name, levels = levels(top_female))) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n)) 

# filter top males
male_names <- babynames %>% 
  filter(sex == "M", 
         name %in% top_male) %>% 
  mutate(name = factor(name, levels = levels(top_male))) %>% 
  group_by(year, name) %>% 
  summarise(n = sum(n)) 



#plot females

plot1 <- ggplot(female_names, aes(year, y=fct_reorder(name, n), height = n/50000, 
                         group = name, scale =2)) + 
  geom_ridgeline(alpha=0.5, scale = 4.5, size = 0,
                          fill = "#05595B", color = "white") +
  xlim(1900,NA) +
  labs(title = 'Female', y ="", x="") +
  theme(plot.title = element_text(hjust = 0, family = "Bahnschrift", size =10),
        axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Bahnschrift", size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size =0.5),
        panel.border = element_blank()) +
geom_segment(aes(x=1967, xend=1967, y=56.7, yend=52), color = "#404040") +
  geom_segment(aes(x=1967, xend=1964, y=56.7, yend=56.7), color = "#404040") +
  annotate(geom = "text", x = 1970, y = 55.6, label = "73,982 babies called\n'Mary'in 1921", hjust = "left",
           size = 2.2, color = "#404040", family = "Bahnschrift")
  
  plot1
  


#plot males
plot2 <- ggplot(male_names, aes(year, y=fct_reorder(name, n), height = n/50000, 
                         group = name, scale =2)) + 
  geom_ridgeline(alpha=0.5, scale = 4.5, size = 0,
                          fill = "#603601", color = "white") +
  xlim(1900,NA) +
  labs(title = 'Male', y ="", x="") +
  theme(plot.title = element_text(hjust = 0, family = "Bahnschrift", size =10),
        axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Bahnschrift", size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size =0.5),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))+
  geom_segment(aes(x=1975, xend=1975, y=58, yend=53.1), color = "#404040") +
  geom_segment(aes(x=1975, xend=1972, y=58, yend=58), color = "#404040") +
  annotate(geom = "text", x = 1977, y = 56.7, label = "94,756 babies called\n'James'in 1947", hjust = "left",
           size = 2.2, color = "#404040", family = "Bahnschrift")


title_theme <- ggdraw() +
  draw_label("50 most common names\n in the US since 1900", 
             fontfamily = "Gadugi",
             fontface = "bold",
             size = 16,
             hjust = 0.5)

#caption
caption <- ggdraw() +
  draw_label("Source: {babynames} from Hadley Wickham | Graphic: Laura Navarro Soler", 
             fontfamily = "Bahnschrift",
             size = 9,
             hjust = 0.5,
             color = "#6B6B6B")


gridofplots <- plot_grid(plot1, plot2, nrow = 1)

final_plot <- plot_grid(title_theme, 
          gridofplots,
          caption,
          ncol = 1, rel_heights = c(0.2, 1.5,0.1))



#saving
ggsave("top_names.png", 
       final_plot, 
       height = 8, width = 10,
       dpi = 300,
       bg = "white")





#gradient colors (thanks to stackoverflow: https://stackoverflow.com/questions/58525699/how-can-i-add-a-vertical-colour-gradient-to-a-ridgeplot-in-ggridges)

#devtools::install_github("coolbutuseless/devout")
#devtools::install_github("coolbutuseless/devoutsvg")
#devtools::install_github("coolbutuseless/minisvg")
#devtools::install_github("coolbutuseless/poissoned")
#devtools::install_github("coolbutuseless/svgpatternsimple")
library(svgpatternsimple)
library(ggridges)

gradient_pattern <- svgpatternsimple::create_pattern_gradient(
  id      = "p1",                                  # HTML/SVG id to assign to this pattern
  angle   = 90,                                    # Direction of the gradient
  colour1 = "white",   # Starting colour
  colour2 = "#05595B"                   # Final colour
)

my_pattern_list <- list(
  `#000001` = list(fill = gradient_pattern)
)

#with new palette

devoutsvg::svgout(filename = "female.svg", pattern_list = my_pattern_list)
ggplot(female_names, aes(year, y=fct_reorder(name, n), height = n/50000, 
                         group = name, scale =2)) + 
  geom_ridgeline_gradient(alpha=0.1, scale = 4.5, size = 0,
                          fill = '#000001', color = "white") +
  xlim(1900,NA) +
  labs(title = '50 most common female baby names since 1900', y ="", x="") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  family = "Bahnschrift", 
                                  size =14, 
                                  face = "bold",
                                  margin =margin(0,0,30,0)),
        axis.ticks.y = element_blank(),
        axis.text = element_text(family = "Bahnschrift", size = 7),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size =0.5),
        panel.border = element_blank())

dev.off()



