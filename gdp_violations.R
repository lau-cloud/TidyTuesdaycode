library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(lubridate)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

violations <- gdpr_violations %>% 
  mutate(newdate = mdy(date)) %>%
  filter(newdate > '2000-01-01')

violations$year = as.numeric(format(violations$newdate, "%Y"))

big_fins <- filter(violations, price > 1000)
big_fins$year <-as.factor(big_fins$year)
big_fins <- arrange(big_fins, desc(price))

cols <- c("2018" = "#3333cc", "2019" = "#e68a00", "2020" = "#b30000")


ggplot(big_fins, aes(x = price, y = name, size = price, fill = year)) +
  geom_point(alpha=0.6, shape=21, color="black") +
  scale_x_log10(breaks = c(1000, 100000, 1000000, 10000000, 100000000),
                limits = c(1000, 100000000),
                labels = c("1k", "10k", "100k", "1M", "100M")) +
  scale_size(range = c(4, 18), name="Fine price") +
  scale_fill_manual(values = cols) +
  labs(title = "Violations of GDPR (the General Data Protection Regulation)") +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        plot.margin = margin(25, 70, 30, 25),
        legend.title = element_text(size = 9),
        title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
  ylab("Country") +
  xlab("Fine amount (in euros)") +
  guides(size = FALSE) +
  annotate("text", x = 50000000, y = "France" , label = "Google Inc.\n 50.000.000???", size = 2 ) +
  annotate("text", x = 27802946, y = "Italy", label = "TIM\n 27.802.946???", size = 1.8 ) +
  annotate("text", x = 18000000, y = "Austria", label = "Austrian Post\n 18.000.000???", size = 1.8 )

ggsave("laura.jpeg", units="in", width=7, height=4.34, dpi=500)



