## senitment analysis
# libraries
library(ggtext)
library(readr)
library(textdata)
library(tidytext)
library(tidyverse)
library(tidyr)

#data
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')


### tidy
tidy_lyrics <- lyrics %>%
  group_by(song_name) %>%
  mutate(
    linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, line)

#sentiment
#positive-negative
sent_bing <- get_sentiments("bing")
unique(sent_bing$sentiment)


#bing (positive-negative)
spice_index <- tidy_lyrics %>%
  inner_join(get_sentiments("bing")) %>%
  filter(album_name == "Spice") %>% 
  count(song_name, index = linenumber, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


##theme
theme_sentiment <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_text(size = 9,color = "#EEEEEE"),
        legend.position = "top",
        axis.text.x = element_text(vjust = 3, color = "#EEEEEE",
                                   size = 5),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 24, face = "bold",
                                  color = "#EEEEEE", hjust = 0.5,
                                  margin=margin(0,0,10,0)),
        plot.subtitle = element_text(size = 14, color = "#EEEEEE",
                                     hjust = 0.5, margin=margin(0,0,20,0)),
        plot.background = element_rect(fill = "#231E23",
                                       color = "transparent"),
        panel.background = element_rect(fill = "#231E23",
                                        color = "transparent"),
        plot.caption = element_text(size = 9, color = "#EEEEEE"),
        legend.box="horizontal",
        legend.text=element_text(size=10, color = "#EEEEEE"),
        strip.text.x = element_text(colour = "#EEEEEE", size=11,
                                    hjust = 0)
  )


plot <- ggplot(spice_index, aes(index, y =1, fill = sentiment)) +
  geom_tile() +
  facet_wrap(~song_name, ncol = 2, scales = "free_x")+
  scale_fill_gradient2(low = "#F21170",
                       mid = "#393E46",
                       high = "#3EDBF0",
                       midpoint = 1,
                       breaks = c(-2, 6),
                       labels = c("- negative", "+ positive"))+
  theme_sentiment +
  labs(title = "Sentiments behind\n Spice Girls' lyrics",
       subtitle = "Positivity/negativity index of each lyrics line\n that appears in Spice Girls album 'Spice'",
       caption = "Data: Jacquie Tran, from Spotify and Genius | @LauraNavarroSol",
       fill="Sentiment Index") +
  theme(
    legend.position = "top"
  ) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5,
                                barwidth = 5, barheight = 0.6,
                                ticks = FALSE))


##save
ggsave("sentiment_plot.png",
       plot, 
       height = 10, width = 7, 
       units = "in", dpi = 300)

