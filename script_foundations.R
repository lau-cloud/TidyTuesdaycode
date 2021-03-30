library(ggplot2)
library(tidyverse)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(cowplot)

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')

unique(data$brand)

#separate categories
data_split <- data %>% 
  separate("categories", paste("category", 1:4, sep="_"), sep=",", extra="drop")


## most common categories
top_cat1 <- data_split %>% 
  group_by(category_1) %>% 
  count() %>% 
  arrange(-n)

top_cat2 <- data_split %>% 
  group_by(category_2) %>% 
  count() %>% 
  arrange(-n)

top_cat3 <- data_split %>% 
  group_by(category_3) %>% 
  count() %>% 
  arrange(-n)

top_cat4 <- data_split %>% 
  group_by(category_4) %>% 
  count() %>% 
  arrange(-n)

#most common categories:
cat <- c("descriptor", "food", "skin", "misc", "gem", 
         "color", "drink", "location", "rock", "wood",
         "compliment", "animal")

#categories
summ <- data_split %>% 
  group_by(category_1) %>% 
  summarize(mean = mean(lightness)) %>% 
  arrange(mean)


 

category <- data %>% 
   filter(categories %in% cat)




#defining colors
min(data$lightness) #0.154902
max(data$lightness) #0.9960784

#lightness-hexcolors
#0-0.125 == #3A2115
#0.125-0.25 == #382725
#0.25-0.375 == #7C4123
#0.375-0.5 == #8E6D51
#0.5-0.625 == #B78B68
#0.625-0.75 == #D5AB89
#0.75-0.875 == #F0C8AE
#0.875-1 == #FDECE1

#order categories
category$categories <- factor(category$categories, levels = c("wood",
                                                              "drink",
                                                              "food",
                                                              "location",
                                                              "descriptor",
                                                              "skin",
                                                              "animal",
                                                              "rock",
                                                              "color",
                                                              "misc",
                                                              "gem"
                                                              ))


#boxplot
plot1 <- ggplot(category, aes(x = lightness, y = categories, fill = categories)) +
  geom_rect(aes(xmin = 0,
                xmax = 0.125,
                ymin = -Inf, ymax = Inf), fill = "#3A2115", alpha = 0.01)+
  geom_rect(aes(xmin = 0.125,
                xmax = 0.25,
                ymin = -Inf, ymax = Inf), fill = "#382725", alpha = 0.01)+
  geom_rect(aes(xmin = 0.25,
                xmax = 0.375,
                ymin = -Inf, ymax = Inf), fill = "#7C4123", alpha = 0.01)+
  geom_rect(aes(xmin = 0.375,
                xmax = 0.5,
                ymin = -Inf, ymax = Inf), fill = "#8E6D51", alpha = 0.01)+
  geom_rect(aes(xmin = 0.5,
                xmax = 0.625,
                ymin = -Inf, ymax = Inf), fill = "#B78B68", alpha = 0.01)+
  geom_rect(aes(xmin = 0.625,
                xmax = 0.75,
                ymin = -Inf, ymax = Inf), fill = "#D5AB89", alpha = 0.01)+
  geom_rect(aes(xmin = 0.75,
                xmax = 0.875,
                ymin = -Inf, ymax = Inf), fill = "#F0C8AE", alpha = 0.01)+
  geom_rect(aes(xmin = 0.875,
                xmax = 1,
                ymin = -Inf, ymax = Inf), fill = "#FDECE1", alpha = 0.01)+
  
  geom_density_ridges(alpha = 0.8, fill = "white", size=0.5) +
 # geom_jitter(alpha = 0.1, size = 0.2) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(family = "Leelawadee UI Semilight"))


#image
ggdraw() +
  draw_image("cream.png", width = 1, height = 1.3)-> cream_png

#title
ggdraw() +
  draw_text("B I A S    I N    B E A U T Y", x= 0.5, y = 0.8, size = 20, family = "Leelawadee UI") +
  draw_text("From the hundreds of different foundations we can find in the make up stores, about the 80% are among the lighter ones. \n Moreover, while dark tones lack 'natural' adjectives, they are offset by words related to food, drink or woods", 
            y = 0.4, x = 0.5, size = 10, family = "Leelawadee UI Semilight") -> header

#caption
ggdraw() +
  draw_text("Visualization: Laura Navarro Soler | Data: The Pudding, 'The Naked Truth'",
            size = 10, x = 0.4, y = 0.2, hjust = 0,
            color = "#8c8c8c", family = "Leelawadee UI Semilight") -> caption

plot_grid(header,
          cream_png, 
          plot1,
          caption,
          rel_heights = c(1,0.6,4,0.3),
          ncol=1)


ggsave("foundation.png", dpi = 300, width = 8, height = 6)
