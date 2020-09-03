library(tidyverse)
library(ggplot2)
library(ggridges)
library(treemap)
library(cowplot)
library(extrafont)
library(ghibli)


key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

spain <- key_crop_yields %>% 
  filter(Entity == "Spain") %>% 
  gather("Crop", "tonnes", 4:14, na.rm = TRUE, factor_key = TRUE) %>% 
  select(Year, Crop, tonnes) %>% 
  mutate(Crop = gsub("(tonnes per hectare)", "", Crop)) %>%
  mutate(Crop = gsub(" ", "", Crop)) %>% 
  mutate(Crop = gsub("[^[:alnum:]]", "", Crop))




reduced <- spain[ ! spain$Crop %in% c("Beans","Peas"), ]
reduced$Crop <- as.factor(reduced$Crop)

reduced$Crop <- factor(reduced$Crop, levels = c("Bananas", "Potatoes", "Maize", "Rice", "Wheat","Barley", "Soybeans"))

plot1 <- ggplot(reduced, aes(x = Year, y = tonnes, fill = Crop, color = Crop)) +
  geom_area(alpha = 0.6, size = 1) +
  scale_fill_manual(values = c("#cec917", "#274637", "#44a57c", "#1d271c", "#58a449", "#58a449", "#cec917", "#819a7a", "#2c715f")) +
  scale_color_manual(values = c("#cec917", "#274637", "#44a57c", "#1d271c", "#58a449", "#58a449", "#cec917", "#819a7a", "#2c715f")) +
  labs(x = "", y = "tonnes per year") +
  xlim(1961, 2024)+
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
    panel.grid = element_line(linetype = "dashed"),
    axis.title.y = element_text(color="grey", size=9),
    axis.text.x = element_text(color = "black", size = 8, angle=45),
    legend.position = "none") +
  annotate(geom="text", x=2016, y=75, label="B A N A N A S",
           color="black", size = 2, hjust = 1) +
  annotate(geom="text", x=2016, y=43, label="P O T A T O E S",
           color="grey", size = 2, hjust = 1) +
  annotate(geom="text", x=2016, y=23, label="M A I Z E",
           color="black", size = 2, hjust = 1) +
  annotate(geom="text", x=2016, y=13, label="R I C E",
           color="white", size = 2, hjust = 1) +
  annotate(geom="text", x=2019.5, y=9, label="W H E A T",
           color="black", size = 2, hjust = 0) +
  annotate(geom="text", x=2019.5, y=5, label="B A R L E Y",
           color="black", size = 2, hjust = 0) +
  annotate(geom="text", x=2019.5, y=1, label="S O Y B E A N S",
           color="black", size = 2, hjust = 0)
  
plot1

#2018
spain$Crop <- as.factor(spain$Crop)
levels(spain$Crop) <- c("B A N A N A S", "B A R L E Y", "B E A N S", "M A I Z E","P E A S", "P O T A T O E S", "R I C E", "S O Y B E A N S","W H E A T")


spain_2018 <- spain %>% 
  filter(Year == 2018)

png(filename="crop_tree.png", width=1200, height=600)
treemap(spain_2018,
                index="Crop",
                vSize="tonnes",
                type="index",
                title = "Tonnes per hectare in Spain \n 2018",
                fontsize.title = 36,
                fontsize.labels = 16,
                fontfamily.title = "Javanese Text",
                fontfamily.labels = "Leelawadee UI Semilight",
                border.col = "white",
                border.lwds = 10,
                alpha = 0.95,
                align.labels = c("left", "top"),
                ymod.labels = -0.08,
                xmod.labels = 0.15,
                aspRatio = 8/3,
                palette= ghibli_palette("MarnieMedium2", direction = -1)
)

dev.off()



#loading treeplot
ggdraw() +
  draw_image("crop_tree.png", width = 1, height = 1) -> treecrop_img

ggdraw() +
  draw_text("Visualization: Laura Navarro Soler | Data: Our Wolrd in Data",
            size = 12, x = 0.35, y = 0.2, hjust = 0,
            color = "#8c8c8c", family = "Leelawadee UI Semilight") -> caption

ggdraw() +
  draw_text("Crops in Spain",
            size = 24, hjust = 0.5,
            color = "black", family = "Javanese Text") -> title

#all together
final_plot <- plot_grid(
  title,
  treecrop_img,
  plot1,
  caption,
  
  ## plot settings
  rel_heights = c(1,3,4,0.5),
  nrow = 4,
  ncol = 1
)
final_plot


#saving
ggsave("crops.png", 
       final_plot, 
       height = 8, width = 7, 
       units = "in", dpi = 300)
