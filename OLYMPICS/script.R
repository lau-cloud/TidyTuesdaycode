# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(devtools)
#install.packages("remotes")
#remotes::install_github("hrbrmstr/streamgraph", force = TRUE)
library(streamgraph)
library(cowplot)
library(htmlwidgets)


olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

medals <- c("Gold", "Silver", "Bronze")

oly <- olympics %>%
  filter(medal %in% medals) %>% 
  filter(season == "Summer") %>%
  filter(sport == "Swimming") %>% 
  count(team, year) %>% 
  rename(total = "n")

#remove the ones with total less than 10
tops <- olympics %>% 
  filter(medal %in% medals) %>% 
  filter(season == "Summer") %>%
  filter(sport == "Swimming") %>% 
  count(team) %>% 
  rename(total_total = "n") %>% 
  arrange(total_total)

#merge to filter
oly <- merge(x = oly, y = tops, by = "team", all.y = TRUE)

#filter an add empty years
oly <- oly %>% 
  filter(total_total>20) %>% 
  drop_na() %>% 
  add_row(year = 1916, total = 0) %>%
  add_row(year = 1940, total = 0)

class(oly$year)
oly <- transform(oly, year = as.Date(as.character(year), "%Y"))

## streamgraph

#plot
pal <- c("#065874","#1b9dbf", "#043b52", "#4eb8d7","#086a8d", "#044364",
         "#0a7796", "#044c5c", "#35b6dc", "#065874",
         "#1b9dbf","#043b52", "#4eb8d7", "#086a8d","#044364",
         "#0a7796", "#044c5c", "#F97141", "#065874")

# Shape: classic
p1 <- streamgraph(oly, key="team", value="total", date="year",
                  width="1000px", height="300px", interactive = TRUE) %>%
  sg_axis_x(tick_interval=5, tick_units = "year", tick_format="%Y") %>%
  sg_legend(TRUE, "Team: ") %>% 
  sg_axis_y(0) %>%
  sg_fill_manual(values=pal) %>% 
  sg_annotate(label="I World War", x=as.Date("1913-08-01"), 
              y=50, color="black", size=10) %>% 
  sg_annotate(label="II World War", x=as.Date("1938-08-01"), 
              y=50, color="black", size=10)

p1

# save the widget
saveWidget(p1, "widget.html")






