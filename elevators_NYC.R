library(tidyverse)
library(sf)
library(osmdata)
library(janitor)
library(showtext)
library(ggtext)


#read the data manually
elevators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv')

# load font from https://fonts.google.com/
font_add_google("Inconsolata", "Inconsolata")

## use showtext to render text
showtext_auto()

#cleaning
df <- elevators |>
  clean_names() |> 
  select(borough, street_name, dv_floor_to, latitude, longitude) |> 
  mutate(
    floors = str_extract(dv_floor_to, "\\d+"),
    floors = as.numeric(floors)
  ) |> 
  filter(!is.na(floors),
         !is.na(borough),
         floors < 200,
         longitude >= -77.5) |> 
  group_by(borough) |> 
  mutate(
    avg_floors = mean(floors)
  )


# Define geographic area and get roads from OSM
roads_nyc <- getbb(place_name = "New York City")  |> 
  opq(timeout = 50)  |> 
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary"))  |> 
  osmdata_sf()


# filter dataframe to get highlighted data
highlight_df <- df %>% 
  filter(floors>=40)

#text
title <- tibble(
  x = 74.4, y = 40.6, 
  label = "<b style='color:black;font-size:62pt;'>Long elevator pitch</b><br><br>Distribution of elevators in the city. \nIn orange, the ones that arrive at the <b style='color:#F39C0F'>40th floor or higher</b>"
)


### map highlighting highest ones
map <- ggplot(df, aes(x = longitude, y = latitude)) +
  geom_sf(data = roads_nyc$osm_lines,
          inherit.aes = FALSE,
          color = "#E9E9E9",
          size = 0.2) +
  geom_point(alpha = 0.1, size = 0.5, color = "#142c44") +
  geom_point(highlight_df, mapping= aes(x = longitude, 
                                        y = latitude,
                                        alpha = floors), 
             size = 0.3, color = "#FFBA4A") +
  labs(y = "",
       x = "",
       caption = "Data from {elevators} package, created by Emil Hvitfeldt | Visualization: Laura Navarro") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill="white",
                                    color = "white"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white",
                                   color = "white"),
    plot.caption = element_text(size = 25,
                                family = "Inconsolata",
                                hjust = 0.5)
  ) +
  geom_richtext(
    aes(x = -74.265, y = 40.87,
        label = "<b style='font-size:80pt;'>Long elevator<br>pitch</b><br><span style='font-size:30pt;'>Distribution of <b style='color:#142c44'>elevators</b> in New York City (2015).<br>In orange, the ones that arrive<br>at the <b style='color:#F39C0F'>40th floor or higher</b></span>"), 
    stat = "unique", 
    family = "Inconsolata",
    hjust = 0,
    lineheight = 1.3, 
    fill = (alpha("white", .6)), 
    label.colour = NA
  )

##histogram
histo <- ggplot(df, aes (x=floors, y =..count..)) +
  geom_density(fill = "#142c44",
               alpha = 0.1,
               color = "#142c44",
               size = 0.4) +
  xlim(60,0) +
  theme_minimal() +
  labs(title = "Last floor's distribution",
       y="",
       x="") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 20,
                             face = "bold",
                             family = "Inconsolata"),
    plot.title = element_text(family = "Inconsolata",
                              size = 20,
                              face = "bold",
                              hjust=-0.7),
    plot.background = element_rect(fill = "transparent",
                                   color = "white")
  ) +
  scale_x_continuous(labels=function(x) paste0(x,"th"),
                     limits = c(0,60))

all <- map + 
  annotation_custom(ggplotGrob(histo), xmin = -74.274, xmax = -74.08, 
                  ymin = 40.65, ymax = 40.81)


#saving
ggsave("map2.jpg",
       all, 
       width = 7, 
       height = 7.5, 
       dpi = 300)
