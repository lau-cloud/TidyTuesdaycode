library(ggplot2)
library(tidyverse)
#install.packages("ggalluvial")
library(ggalluvial)
#install.packages("showtext")
library(showtext)
library(ggtext)
library(cowplot)
library(magick)



## Loading Google fonts (https://fonts.google.com/)
font_add_google("Bebas Neue", "bebas")
font_add_google("Montserrat", "montserrat")

showtext_auto()



chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')


#by origin
ranking_exports <- chocolate %>% 
  count(country_of_bean_origin) %>% 
  filter(country_of_bean_origin!="Blend") %>% 
  arrange(-n) %>% 
  top_n(5)

top_exports <- ranking_exports$country_of_bean_origin

ranking_imports <- chocolate %>% 
  count(company_location) %>% 
  arrange(-n) %>% 
  top_n(5)

top_imports <- ranking_imports$company_location

ranking_manu <- chocolate %>% 
  count(company_manufacturer) %>% 
  arrange(-n) %>% 
  top_n(10)

top_manu <- ranking_manu$company_manufacturer


top_choco <- chocolate %>% 
  filter(company_location %in% top_imports) %>% 
  filter(country_of_bean_origin%in%top_exports) %>%
  filter(company_manufacturer%in%top_manu) %>% 
  count(company_manufacturer,country_of_bean_origin, company_location) %>% 
  mutate(origin = "origin") %>% 
  rename(total = "n")


## aesthetics
color_palette  <- c("#8B9A46", 
                    "#8B9A46", 
                    "#8B9A46", 
                    "#8B9A46", 
                    "#541212", 
                    "#8B9A46",
                    "#8B9A46",
                    "#8B9A46")


#plot
plot <- ggplot(top_choco,
       aes(axis1 = country_of_bean_origin,
           axis2 = company_location,
           axis3 = company_manufacturer,
           y = total)) +
  geom_alluvium(aes(fill = country_of_bean_origin),
                curve_type = "quintic",
                width = 1/12,
                knot.pos = 0.4,
                alpha= 0.7)+
  geom_stratum(aes(fill = country_of_bean_origin),
               width = 1/6, color = "black", alpha=1, fill ="white",
               linetype="dotted") +
  scale_color_manual(values = color_palette) +
  scale_fill_manual(values = color_palette) + 
  scale_x_continuous(breaks = 1:3, 
                     labels = c("Origin", "Destination country", "Company"),
                     position = "top") + 
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            color = "black",
            family="montserrat",
            size = 3) +
    theme_minimal() +
  labs(y = "",
       x="")+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 35, 
                              family = "bebas",
                              face = "bold"),
    plot.subtitle = element_text(size=12,
                                 family = "montserrat",
                                 margin = margin(t=5,b=20)),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "montserrat", face="bold")
  )


## cocoa image
ggdraw() +
  draw_image("beans.png", width = 1, height = 1) -> bean_img



#Title and subtitle
ggdraw() +
  draw_text("Follow the cacao: from venezuela's forest to Domori's warehouse", x= 0.49, y = 0.80, size = 24, family = "bebas") +
  draw_text("Chocolate's path from the raw cocoa bean to the chocolate bar. Top importers, exporters\n and manufacturer companies. The stroke size depicts the total different types of chocolate\nimported. Venezuela is highlighted because it is the top exporter.", 
            0.04, y = 0.45, size = 12, family = "montserrat", hjust = 0,lineheight = 1.1)  -> header_plot

#grid of images
image_row <- plot_grid(header_plot, bean_img, rel_widths = c(7,1.5), ncol=2)

#caption
ggdraw() +
  draw_text("Data: Flavors of Cacao by way of Georgios and Kelsey | Visualization: Laura Navarro Soler",
            size = 10, x = 0.37, y = 0.2, hjust = 0,
            color = "#979797", family = "montserrat") -> caption
#final plot
final_plot <- plot_grid(
  
  image_row,
  
  #header_plot,
  
  plot,
  
  caption,
  
  
  ## plot settings
  rel_heights = c(2.5,5.5,1),
  nrow = 3
) 

final_plot

#saving
ggsave("chocolate_plot.pdf", 
       final_plot, 
       height = 8, width = 10, 
       units = "in", dpi = 300)


