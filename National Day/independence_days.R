library(ggplot2)
library(tidyverse)
library(calendR)
library(stringr)
library(lubridate)
library(scales)
library(extrafont)



holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')



#calendar
year(holidays$date_parsed) <- 2021

Sys.setlocale("LC_TIME", "English")

days_count <- holidays %>% 
  rename(d_date = date_parsed) %>%
  group_by(d_date) %>%
  summarise(n_holidays = n()) %>% 
  ungroup() %>% 
  drop_na() %>% 
  complete(d_date = seq.Date(min(d_date), max(d_date), by="day")) %>% 
  mutate(wday = weekdays(d_date),
         month_day = day(d_date),
         month = month(d_date),
         week_increment = ifelse(month_day == 1 | wday == "Sunday", 1, 0)) %>% 
  group_by(month) %>% 
  mutate(week = cumsum(week_increment),
         text_month = months(d_date)) %>% 
  ungroup()


#set factor levels
wday_vec <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
days_count$wday <- factor(days_count$wday, levels = wday_vec)
month_vec <- c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December") 
days_count$text_month <- factor(days_count$text_month, 
                                levels =  month_vec)

#days_count <- days_count[-nrow(daily_totals),]


days_calendar <- ggplot(days_count, aes(x = wday, y = week)) + 
  geom_tile(aes(fill = n_holidays), colour = "#6A6A6A") +
  # geom_tile(fill="#bfbf3f", colour = "black") +
  # geom_point(aes(size= n_holidays, color = n_holidays), alpha = 0.7)+
  facet_wrap(~text_month) + 
  scale_y_reverse() + 
  theme_minimal() + 
  scale_color_gradient(low= "#928A97", high = "#D3273E", na.value="#FBE8D3") + 
  scale_fill_gradient(low= "#E57685", high = "black", na.value="#FECEA8",
                      breaks=c(1,3,5),labels=c(1,3,5))+
  scale_size_continuous(range = c(3, 7))+
  scale_x_discrete(position = "top") +
  ylab("") + xlab("") + labs(fill= "Independence Days & Others") + 
  labs(title = "When Independence Days take place?", subtitle = "This calendar shows the frequency of special days (independence day, republic\n day, national day) of a total of 189 countries. 2021 calendar distribution.",
       caption = "Font: Wikipedia and Isabella Velasquez | Viz: Laura Navarro Soler for #TidyTuesday") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FECEA8", color = "#FECEA8"),
        plot.background = element_rect(fill = "#FECEA8"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(
          size = 10, color = "black", face = "bold", family = "Georgia"), 
        plot.title = element_text(size = 24, hjust = 0.5, family = "Georgia"),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        legend.position = "top",
        legend.title = element_text(size = 10),
        plot.margin = margin(1, 2, 1, 2, "cm"),
        plot.caption = element_text(size = 8, hjust = 1)) 

days_calendar +
  guides(fill = guide_colourbar(barheight = 1,ticks = FALSE,title.position="top", title.hjust = 1
                                ))
