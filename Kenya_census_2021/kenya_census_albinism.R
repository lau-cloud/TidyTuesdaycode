library(tidyverse)
library(ggplot2)
library(sf)
library(extrafont)
library(ggimage)
library(png)
# devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)
library(grid)

##read the data
demography <- rKenyaCensus::V3_T2.3
gender <- rKenyaCensus::V1_T2.2
drinking_water <- rKenyaCensus::V4_T2.15
albinism <- rKenyaCensus::V4_T2.28

map <- rKenyaCensus::KenyaCounties_SHP %>% 
  sf::st_as_sf()

glimpse(map)


#map
albins_map <- map %>% 
  st_transform(4326) %>% 
  mutate(County = as.character(County),
         County = str_to_title(County))


#filter by county and calculate rate
df <- albinism %>% 
  filter(AdminArea == "County") %>% 
  mutate(County = as.character(County),
         County = str_to_title(County)) %>% 
  select(County, TP_Total, TP_Male, TP_Female, PWA_Total, PWA_Female, PWA_Male) %>% 
  mutate(ratio = PWA_Total/TP_Total*100000) %>% 
  right_join(albins_map) 

summary(df)

# Import the image
image <- readPNG("pattern.png")

#dataviz
ggplot(df) +
  annotation_custom(rasterGrob(image, 
                               width = unit(1.5,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_sf(aes(geometry = geometry, fill = ratio), color = "white", size = 0.5) +
  scale_fill_gradient("People with albinism\nper 100,000 inhabitants",low = "transparent", high = "white") +
  scale_x_continuous(limits = c(32, 43))+
  scale_y_continuous(limits = c(-5, 8))+
  theme_void() +
  theme(legend.position = c(0.2, 0.16),
        legend.title = element_text(size = 10, color = "white"), 
        legend.text = element_text(size = 10, color = "white")) +
  geom_text(x=42.6, 
            y=7.7, 
            label="Albinism", 
            size = 12,
            hjust = 1,
            color = "white",
            family = "Bahnschrift") +
  geom_text(x=42.6, 
            y=7.1, 
            label="in Kenya", 
            size = 10,
            hjust = 1,
            color = "white",
            family = "Segoe UI Light") +
  geom_text(x=42.6, 
            y=6.1, 
            label="Albinism is an inherited genetic condition that reduces the amount of melanin pigment.\nThere are 9729 persons with albinism in Kenya (according to this data),\n514 in the Count of Garissa.", 
            size = 4,
            hjust = 1,
            color = "#B49268",
            family = "Segoe UI Light") +
  geom_text(x=42.9, 
            y=-5.3,
            hjust = 1,
            label="Data by {rkenyacensus}, Shelmith Kariuki | Viz: Laura Navarro Soler for #TidyTuesday", 
            size = 4, 
            color = "#B49268",
            alpha = 0.7,
            family = "Leelawadee UI Semilight") +
  geom_text(x=42.2, 
            y=-0.6, 
            label="GARISSA\n", 
            size = 3.3,
            hjust = 1,
            color = "#B49268",
            family = "Segoe UI Light") +
  guides(fill = guide_legend(override.aes = list(size = 0.5)))


ggsave("kenya_albinism.png", 
       height = 8, 
       width =8, 
       dpi = 300)
