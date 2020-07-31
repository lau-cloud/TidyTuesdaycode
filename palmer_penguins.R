library(tidyverse)
library(ggplot2)
library(ggridges)
library(cowplot)
library(scales)
library(magick)
library(extrafont)
library(ggpubr)
library(patchwork)
library(rsvg)

#read data
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

#tidy
df_penguins <- penguins %>% 
  select(species, bill_length_mm:flipper_length_mm,sex) %>% 
  pivot_longer(bill_length_mm:flipper_length_mm) %>% 
  drop_na(sex) %>% 
  mutate(
    name = name %>% 
      str_replace_all("_", " ") %>% 
      str_remove_all(pattern = " mm") %>% 
      str_to_title() %>% 
      paste("(mm)")
  ) %>% 
  filter(name == "Bill Length (mm)") 

gentoo <- df_penguins %>% 
  filter(species == "Gentoo")

chinstrap <- df_penguins %>% 
  filter(species == "Chinstrap")

adelie <- df_penguins %>% 
  filter(species == "Adelie")


#plots
p1 <- gentoo %>% 
  ggplot(aes(x = value, fill = sex)) +
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 bins = 25) +
  scale_fill_manual(values = c("#66b3ff", "#8c8c8c"))+
  ylab("number of penguins") +
  xlab("length (mm)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "white", size = 10),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text( size = 12, hjust = 1)
  )
p1

p2 <- chinstrap %>% 
  ggplot(aes(x = value, fill = sex)) +
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 bins = 25) +
  scale_fill_manual(values = c("#66b3ff", "#8c8c8c"))+
  ylab("number of penguins") +
  xlab("length (mm)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "white", size = 10),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12, hjust = 1)
  ) 
p2

p3 <- adelie %>% 
  ggplot(aes(x = value, fill = sex)) +
  geom_histogram(position = "identity", 
                 alpha = 0.5, 
                 bins = 25) +
  scale_fill_manual(values = c("#66b3ff", "#8c8c8c"))+
  theme_minimal() +
  ylab("number of penguins") +
  xlab("length (mm)") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text( size = 20),
        axis.title = element_text(size = 10, color = "grey", face = "italic"),
        plot.subtitle = element_text(size = 12, hjust = 1)
  )
p3


## Adelie Image
ggdraw() +
  draw_image("adelie.png", width = 1.2, height = 1.3) +
  draw_text("A D E L I E", y = 1.25, size = 16, family = "Leelawadee UI Semilight") -> adelie_img

## Gentoo Image
ggdraw() +
  draw_image("gentoo.png", width = 1.2, height = 1.3) +
  draw_text("G E N T O O", y = 1.25, size = 16, family = "Leelawadee UI Semilight") -> gentoo_img

## Chinstrap Image
ggdraw() +
  draw_image("chinstrap.png", width = 1.2, height = 1.3) +
  draw_text("C H I N S T R A P", y = 1.25, size = 16, family = "Leelawadee UI Semilight") -> chinstrap_img


#grid of plots
image_row <- plot_grid(adelie_img, gentoo_img, chinstrap_img, ncol=3)

#Title and subtitle
ggdraw() +
  draw_text("Palmer Penguins Bill Length", x= 0.2, y = 0.85, size = 18, family = "Leelawadee UI") +
  draw_text("Palmer Archipelago is a group of islands off the northwestern coast of the Antarctic Peninsula.", 
            y = 0.64, x = 0.39, size = 12, family = "Leelawadee UI Semilight") +
  draw_text("The histograms show that females has shorter bills than males in every species", 
            y = 0.53, x = 0.33, size = 12, family = "Leelawadee UI Semilight") -> header_plot

#legend on the bottom
row1 <- plot_grid(p3,p1 + theme(legend.position="none"),p2, ncol=3)

#caption
ggdraw() +
  draw_text("Visualization: Laura Navarro Soler | Data: Gorman, Williams & Fraser (2014)",
            size = 12, x = 0.4, y = 0.2, hjust = 0,
            color = "#8c8c8c", family = "Leelawadee UI Semilight") -> p6


#extract legend and plot it separately
legend <- get_legend(
  p1 + theme(legend.box.margin = margin(0, 0, 0, 0))
)

#final plot
final_plot <- plot_grid(
  
  header_plot,
  
  image_row,
  
  row1,
  
  legend,
  
  p6,
  

  ## plot settings
  rel_heights = c(3.1,3.5,4.5,0.7,1),
  nrow = 5
) 

final_plot

#saving
ggsave("penguinsbill_plot.png", 
       final_plot, 
       height = 8, width = 10, 
       units = "in", dpi = 300)

