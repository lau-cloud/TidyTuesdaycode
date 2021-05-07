library(tidyverse)
library(ggplot2)
library(sf)
library(rworldmap)
library(ggmap)
library(maps)
#install.packages("mapdata")
library(mapdata)
install.packages("ggExtra")
library(ggExtra)
library(extrafont)

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

# get map
world <- map_data("world")

#group types of water sources
unique(water$water_source)

water$water_source <- as.factor(water$water_source)

levels(water$water_source) <- list(Borehole  = "Borehole", Protected_Shallow_Well = "Protected Shallow Well",
                                   Protected_Spring = "Protected Spring", Shallow_Well = "Undefined Shallow Well",
                                   Surface_Water = "Surface Water (River/Stream/Lake/Pond/Dam)", Rainwater_Harvesting = "Rainwater Harvesting",
                                   Unprotected_Shallow_Well = "Unprotected Shallow Well", Other = "Undefined Spring",
                                   Other = "Piped Water", Other = "Unprotected Spring", Other = "Sand or Sub-surface Dam",
                                   Other = "Delivered Water", Other = "Packaged water")






pal <- c("#8dd3c7", "#ffffb3", "#fb8072", "#bebada", "#80b1d3", "white", "grey", "black")


#create and add water points
world_map <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "#505050") +
  geom_point(data = water, aes(x = lon_deg, y = lat_deg, color = water_source), size = 0.3) +
  #geom_density_2d(aes(x = lon_deg, y = lat_deg, color = water_source, alpha = 0.25),
                #  size = 0.5, bins = 100, data = water,
                #  geom = "polygon")+
  scale_color_manual(values = pal, name = "Sources", labels = c("Borehole", 
                                                                "Protected Shallow Well", 
                                                                "Protected Spring",
                                                                "Shallow Well",
                                                                "Surface Water",
                                                                "Rainwater Harvesting",
                                                                "Unprotected Shallow Well",
                                                                "Others")) +
  labs(title = "Water", subtitle = "Access Points") +
  coord_sf(xlim = c(-70.00, 140.00), ylim = c(-30.00, 50.00), expand = FALSE) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.position=("bottom"),
    legend.key=element_rect(fill = NA),
    legend.box.background = element_rect(fill = "black"),
    legend.text = element_text(color = "grey"),
    panel.grid = element_blank(),
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.background=element_blank(),
    plot.title = element_text(color = "white", family = "Ebrima", size = 18, hjust = 0.5),
    plot.subtitle = element_text(color = "white", family = "Ebrima", size = 14, hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size=2, fill=NA))) 

world_map
