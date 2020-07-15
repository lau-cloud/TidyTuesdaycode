library(tidyverse)
library(extrafont)
library(ggdark)
library(streamgraph)
library(devtools)

#data
datos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#tidy
age_astronauts <- datos %>% 
  mutate(age_at_mission = year_of_mission - year_of_birth) %>% 
  select(name, nationality, sex, age_at_mission, hours_mission, mission_number, year_of_mission, occupation)

unique(datos$nationality)
class(age_astronauts$mission_number)
age_astronauts$mission_number <- as.factor(age_astronauts$mission_number)
unique(age_astronauts$mission_number)

astronauts <- datos %>% 
  group_by(nationality, year_of_mission) %>% 
  summarize(count=n())
  

#plot
streamgraph(astronauts, 
            key="nationality", 
            value="count", 
            date="year_of_mission", 
            height="300px",
            width="900px",
            interpolate = "basis-open") %>% 
  sg_fill_manual(c("black", "#F4D03F", "#F39C12","#2980B9")) %>% 
  sg_axis_x(tick_interval = 5) %>% 
  sg_axis_y(5, "year")

age_astronauts %>% arrange(-desc(age_at_mission))
  
  

ggplot(age_astronauts, aes(x = year_of_mission, y = age_at_mission, color = mission_number)) + 
  geom_jitter(aes(alpha = 0.7)) +
  theme_minimal() +
  labs(title = "Age of astronauts at every mission",
       subtitle = "By number of missions done",
       caption = "Laura Navarro - @LauraNavarroSol | Astronaut database from Mariya Stavnichuk and Tatsuya Corlett") +
  scale_y_continuous(trans='log10', name = "Age at the mission") +
  scale_colour_viridis_d(option = "plasma", direction=-1) +
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "#C7C4C4"),
        legend.key = element_blank(),
        legend.title = element_text(color = "#C7C4C4"),
        legend.text = element_text(color = "#C7C4C4"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "#C7C4C4", margin = margin(0,30,0,0)),
        plot.title = element_text(color="#C7C4C4", size=14, face="bold",margin = margin(0,0,10,0)),
        plot.subtitle = element_text(color = "#C7C4C4", size=12, face="bold.italic", margin = margin(0,0,30,0)),
        plot.caption = element_text( color = "grey30", size=8),
        plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  scale_alpha(guide = 'none') +
  guides(color=guide_legend(title="Mission Number")) +
  annotate("text", x = 1985, y = 74,
    label = "John Glenn, senator of US, was 77.\n The oldest person to\ntravel in space",
    colour = "white",
    vjust = 1,
    size = 3
  ) +
  annotate(
    "curve",
    xend = 1997,
    yend = 77,
    x = 1990,
    y = 70,
    curvature = -.2,
    arrow = arrow(type = "closed", length = unit(0.40, "lines")),
    colour = "#C7C4C4"
  ) +
  annotate("text", x = 1973, y = 27,
           label = "Guerman Titov and\n Valentina Tereshkova (26)\n youngest persons to travel in space",
           colour = "white",
           vjust = 1,
           size = 3
  ) +
  annotate(
    "curve",
    xend = 1961,
    yend = 25,
    x = 1967,
    y = 25,
    curvature = -.2,
    arrow = arrow(type = "closed", length = unit(0.40, "lines")),
    colour = "#C7C4C4"
  ) +
  annotate(
    "curve",
    xend = 1963,
    yend = 25,
    x = 1967,
    y = 25,
    curvature = -.2,
    arrow = arrow(type = "closed", length = unit(0.40, "lines")),
    colour = "#C7C4C4"
  ) +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE, colour = "grey", alpha = 0.7)



ggsave("age_astronauts.png", dpi = 300, width = 11, height = 8) 
