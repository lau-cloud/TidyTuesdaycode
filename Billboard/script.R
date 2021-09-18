library(ggplot2)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(plotly)
library(extrafont)
library(grid)


#loading data
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
audio_features <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

#merging both datasets
songs <- left_join(billboard, audio_features, by = c('song_id', 'song', 'performer')) %>%
  mutate(date = mdy(week_id)) %>% 
  mutate(year = year(date))



#split genres and clean

songs <- songs %>% 
  select(year, spotify_genre)

genres <- transform(songs, test=do.call(rbind, strsplit(spotify_genre, ',', fixed=TRUE)), stringsAsFactors=F)

genres <- genres %>% 
  select(year, test.1)

genres$test.1 <- gsub("","",as.character(genres$test.1))
genres$test.1 <- gsub("'","",as.character(genres$test.1))
genres$test.1 <- gsub('"',"",as.character(genres$test.1))


genres$test.1 <- substr(genres$test.1, 2, nchar(genres$test.1)-1)

g_by_year <- genres %>% 
  count(year, test.1) %>% 
  rename(total = "n")

# Top N highest values by group
g_by_year2 <- g_by_year %>%                                      
  arrange(desc(total)) %>% 
  group_by(year) %>%
  slice(1:3)


#remove empty cells and NA
g_by_year2 <- g_by_year2[!(is.na(g_by_year2$test.1) | g_by_year2$test.1==""), ]


#improve gaps
g_by_year2 <- setkey(setDT(g_by_year2), year, test.1)[CJ(year=unique(year), test.1=unique(test.1))
][is.na(data), data:=0L]

g_by_year2[is.na(g_by_year2)] <- 0



#plot

p <- ggplot(g_by_year2, aes(x = year, y = total, group = test.1, fill = test.1)) +
  geom_area(position="fill") +
  scale_fill_manual(values = c('#2A363B','#019875','#99B898','#FECEA8','#FF847C','#E84A5F','#C0392B','#96281B',
                               '#7c898f', '#7fd4c0', '#050505', '#ffa761', '#ff9ca9',
                               '#82372f', '#ededed', '#825f5f')) +
  scale_x_continuous(breaks = seq(1958, 2021, by = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(
    legend.position="none",
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0), size = 8),
    axis.text.x = element_text(margin = margin(t = -12, r = 0, b = 30, l = 0)),
    axis.ticks = element_blank(),
    plot.caption = element_text(size=8, color = "grey", hjust = 1, margin = margin(t = -8, r = 20, b = 5, l = 0)),
    plot.margin = margin(t = 20, r = 0, b = 0, l = 0)
  ) +
  labs(y = "%", caption = "I took in consideration only the first genre that appear in the spotify-genre category.\nSource: Data.World by way of Sean Miller, Billboard.com and Spotify. Viz: Laura Navarro @LauraNavarroSol") +
  annotate("text", x = 1960, y = 0.59, label = "adult standard\n(popular, well-known songs\nfor 'mature' adults)", family="Times New Roman",size = 2.6, color = "white", hjust = 0) +
  annotate("text", x = 1967, y = 0.25, label = "brill \nbuilding",family="Times New Roman",size = 2.6,color = "white",
           hjust = 1) +
  annotate("text", x = 1982, y = 0.43, label = "album rock", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 2000, y = 0.25, label = "dance pop", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 2010, y = 0.75, label = "contemporary \ncountry", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 1970.5, y = 0.10, label = "classic soul", family="Times New Roman",size = 2.6, color = "white")+
  annotate("text", x = 1979.5, y = 0.05, label = "disco", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 1985, y = 0.10, label = "dance rock", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 1987, y = 0.70, label = "boy band", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 1989, y = 0.88, label = "alt rock", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 2003, y = 0.90, label = "alt metal", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 2011, y = 0.07, label = "hip hop", family="Times New Roman",size = 2.6, color = "white") +
  annotate("text", x = 2018.4, y = 0.10, label = "pop", family="Times New Roman",size = 2.6, color = "white") +
  geom_curve(aes(x = 1989, y = 0.70, xend = 1995, yend = 0.70), color = "white", arrow = arrow(length = unit(0.01, "npc"))) +
  geom_curve(aes(x = 1991, y = 0.88, xend = 1996, yend = 0.93), color = "white", arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = 2017, y = 0.35, label = "contemporary \ncountry",family="Times New Roman",size = 2.6, color = "white", hjust = 1) +
  geom_curve(aes(x = 2017, y = 0.35, xend = 2020.5, yend = 0.37), color = "white", arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = 2019, y = 0.90, label = "alt\nhip\nhop",family="Times New Roman",size = 2.6, color = "white", hjust = 0) +
  geom_curve(aes(x = 2018.5, y = 0.90, xend = 2017, yend = 0.93), color = "white", arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = 1960, y = 0.81, label = "Most common genre of the songs that\nappeared in the Billboard Hot 100 each week\nfrom 1958 to 2021", family="Leelawadee UI Semilight", size = 3.5, color = "white", hjust = 0) +
  annotate("text", x = 1960, y = 0.93, label = "Evolution of genres", family="Times New Roman", size = 8, color = "white", hjust = 0, fontface = "bold")


#saving
ggsave("genres_billboard_2.png", 
       p, 
       height = 5.5, width = 9, 
       units = "in", dpi = 300)

  
  




