library(tidyverse)
library(ggplot2)
library(zoo)
library(scales)
library(lubridate)


#get the data
tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')


#wrangle
tuits <- tweets %>% 
  arrange(datetime) %>% 
  select(datetime, retweet_count, like_count, quote_count) %>% 
  mutate(retweet_count = cumsum(retweet_count), 
         quote_count = cumsum(quote_count),
         like_count = cumsum(like_count)) %>% 
  gather(type, total, retweet_count, quote_count, like_count) %>% 
  drop_na()


#reorder types
tuits$type <- factor(tuits$type, levels = c("like_count", "retweet_count", "quote_count"))


#dates
tuits$datetime <- as.POSIXct(tuits$datetime) 


#cumulative plot
ggplot(tuits, aes( x = datetime, y = total, fill = type, group = type)) +
  geom_area(position = "identity") +
 # scale_fill_manual(labels = c("Enfermedades zim?ticas", "Heridas de guerra", "Otras"), values = c("#474444", "#DF9D43", "#FED398")) +
  theme_classic() +
  labs(title="TWEETS OF DU BOIS CHALLENGE 2020",
       subtitle="Cumulative frequency",
       y="count", caption = "VIZ: LAURA NAVARRO SOLER | DATA:  ANTHONY STARKS, ALLEN HILLERY, SEKOU TYLER") +
  scale_fill_manual(values = palette, labels = c("likes", "retweets", "quote")) +
  theme(legend.position = "none",
        axis.text.x=element_text(hjust=1, family = "mono"),
        axis.text.y=element_text(hjust=1, family = "mono"),
        axis.title.y=element_text(family = "mono"),
        plot.background = element_rect(fill = "#f3ebe0"),
        panel.background = element_rect(fill = "#f3ebe0", size = 1),
        axis.title.x=element_blank(),
        strip.text=element_text(colour= "white"),
        panel.grid.major.y = element_line(size = 0.5, colour = "grey", linetype = "dotted"),
        plot.title = element_text(family = "mono",hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(family = "mono",hjust = 0.5),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "#f3ebe0"),
        legend.text = element_text(family = "mono"),
        legend.title = element_text(family = "mono", face = "bold"),
        plot.caption = element_text(family = "mono", size = 9),
        plot.margin = margin(0.5, 0.3, 0.3, 0.5, "cm")) +
  
  guides(fill = guide_legend(title="type")) +
  annotate(geom = "text", x = as.POSIXct("2021-04-20 22:07:13"), y = 5000, 
           label = "LIKES", color = "WHITE", family = "mono", fontface = "bold",
           size = 4, hjust = 0.5) +
  annotate(geom = "text", x = as.POSIXct("2021-04-20 22:07:13"), y = 850, 
           label = "RETWEETS", color = "black", family = "mono", fontface = "bold",
           size = 4, hjust = 0.5) +
  annotate(geom = "text", x = as.POSIXct("2021-04-20 22:07:13"), y = 190, 
           label = "QUOTES", color = "black", family = "mono", fontface = "bold",
           size = 3, hjust = 0.5) +
  geom_vline(xintercept = as.POSIXct("2020-03-16"), linetype="dotted", 
             color = "white", size=1)


#save
ggsave("cum_dubois.png", 
       dpi = 300, 
       width = 9, 
       height = 7)

