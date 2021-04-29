library(tidyverse)
library(ggplot2)
library(waffle)
library(cowplot)
library(extrafont)

 #install.packages("remotes")
 #remotes::install_github("davidsjoberg/ggstream")
library(ggstream)


#data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')


# wrangling
ceo_waffle <- departures %>% 
  mutate(
    reason = case_when(
      departure_code == 1 ~ "death",
      departure_code == 2 ~ "illness",
      departure_code == 3 ~ "dismissed for job performance",
      departure_code == 4 ~ "dismissed for legal violations or concerns",
      departure_code == 5 ~ "retired",
      departure_code == 6 ~ "new opportunity",
      departure_code == 7 ~ "other",
      departure_code == 8 ~ "missing",
      departure_code == 9 ~ "execucomp error",))%>% 
  drop_na(reason) %>% 
  filter(reason != "execucomp error") %>% 
  filter(reason != "other") %>% 
  filter(reason!= "missing") %>% 
  filter(fyear>2017) %>% 
  count(reason, sort = T) 


ceo_waffle$reason <- factor(ceo_waffle$reason, levels = c("retired", 
                                                          "dismissed for job performance", 
                                                          "dismissed for legal violations or concerns",
                                                          "illness",
                                                          "new opportunity",
                                                          "death"))


#geom_waffle
waffle <- ggplot(ceo_waffle, aes(fill = reason, values = n)) +
  geom_waffle(n_rows = 11, size = 0.03, colour = "black") +
  scale_fill_manual(name = NULL,
                    values = c("retired" = "#ffc93c", 
                               "dismissed for job performance" = "#ffee93",
                               "dismissed for legal violations or concerns" = "grey",
                               "illness" = "#1b2021",
                               "new opportunity" = "#31326f",
                               "death" = "white")) +
  coord_equal() +
  labs(title = "Reasons CEOs leave the company", subtitle = "counting from 2017 untill 2020") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7, color = "#8E8E8E", family="Bahnschrift"),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.key.size = unit(0.7, "line"),
    legend.key = element_rect(size = 0.1, color = NA),
    legend.background = element_rect(color = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5, family="Bahnschrift"),
    plot.subtitle = element_text(hjust = 0.5, family="Bahnschrift")
    ) 

waffle

# area chart
ceo <- departures %>% 
  mutate(
    reason = case_when(
      departure_code == 1 ~ "death",
      departure_code == 2 ~ "illness",
      departure_code == 3 ~ "dismissed for job performance",
      departure_code == 4 ~ "dismissed for legal violations or concerns",
      departure_code == 5 ~ "retired",
      departure_code == 6 ~ "new opportunity",
      departure_code == 7 ~ "other",
      departure_code == 8 ~ "missing",
      departure_code == 9 ~ "execucomp error",))%>% 
  drop_na(reason) %>% 
  filter(reason != "execucomp error") %>% 
  filter(reason != "other") %>% 
  filter(reason != "missing") %>% 
  count(reason, fyear, sort = T)

lines <- ggplot(ceo, aes(x = fyear, y = n, color = reason)) +
  geom_line(size = 2)+
  scale_color_manual(values = c("retired" = "#ffc93c", 
                               "dismissed for job performance" = "#ffee93",
                               "dismissed for legal violations or concerns" = "grey",
                               "illness" = "#1b2021",
                               "new opportunity" = "#31326f",
                               "death" = "#8ac4d0")) +
  theme_minimal() +
  labs(y = "number of CEOs", title = "Historical record",
       caption= "Laura Navarro Soler | Data from Gentry et al. by way of DataIsPlural. Reasons for CEO departure in S&P 1500 firms.") +
  theme(
    axis.text = element_text(size = 8, color = "grey", family="Bahnschrift"),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7, color = "grey", family="Bahnschrift"),
    plot.title = element_text(color = "grey", family="Bahnschrift"),
    plot.caption = element_text(size = 8, color = "grey", hjust = 1, family="Bahnschrift")
                                
    )

lines


#all together
final_plot <- plot_grid(
  waffle,
  lines,
  ## plot settings
  rel_heights = c(4,2),
  nrow = 2,
  ncol = 1
)
final_plot


#saving
ggsave("ceos.png", 
       final_plot, 
       height = 5, width = 7, 
       units = "in", dpi = 300)


