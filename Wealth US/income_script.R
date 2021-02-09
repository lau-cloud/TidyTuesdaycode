library(tidyverse)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(cowplot)
library(extrafont)
library(ggtext)

income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

unique(income$year)
class(income$year)

income <- income %>% 
  filter(race != "All Races" & year == 2019) 

income$income_bracket <- factor(income$income_bracket, levels = c("Under $15,000",
                                                                  "$15,000 to $24,999",
                                                                  "$25,000 to $34,999",
                                                                  "$35,000 to $49,999",
                                                                  "$50,000 to $74,999",
                                                                  "$75,000 to $99,999",
                                                                  "$100,000 to $149,999",
                                                                  "$150,000 to $199,999",
                                                                  "$200,000 and over"))

#white and black
income$income_distribution <- ifelse(income$race == "White Alone", -1*income$income_distribution, income$income_distribution)

plot1 <- ggplot(income, aes(x = income_bracket, y = income_distribution)) +
  geom_bar(data = dplyr::filter(income, race == "White Alone"), stat = "identity", fill = "#96bb7c", alpha = 0.95) + 
  geom_bar(data = dplyr::filter(income, race == "Black Alone"), stat = "identity", fill = "#eebb4d", alpha = 0.95) +
  coord_flip() +
  labs(title = "<span style='color:#96bb7c;'>WHITE ALONE |</span> <span style='color:#eebb4d;'>BLACK ALONE</span>",x="", y="") +
  scale_y_continuous(limits = c(-20,20), breaks = c(-15,0,15), labels=c("15%","0","15%")) +
  scale_x_discrete(labels = c("Under $15K", "$15-$25K", "$25-$35K",
                              "$35-$50K", "$50K-$75K", "$75-$100K",
                              "$100-$150K", "$150-$200K", "$200K and over")) +
  theme_minimal() +
 # annotate("text", x = "$200,000 and over", y = -15, label = "W H I T E\n  A L O N E", color = "#d6efc7", size = 4, alpha = 0.9) +
#annotate("text", x = "$200,000 and over", y = 15, label = "B L A C K\n   A L O N E", color = "#eebb4d", size = 4, alpha = 0.9) +
  theme(panel.background = element_rect(fill = "#184d47", color = "NA"),
        plot.background = element_rect(fill = "#184d47", color = "NA"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_markdown(hjust = 0.5, size =11, color = "grey", margin = margin(0,0,20,0)))

plot1
#ASIAN AND WHITE
income$income_distribution <- ifelse(income$race == "Asian Alone", -1*income$income_distribution, income$income_distribution)

plot2 <- ggplot(income, aes(x = income_bracket, y = income_distribution)) +
  geom_bar(data = dplyr::filter(income, race == "Asian Alone"), stat = "identity", fill = "#d6efc7", alpha = 0.95) + 
  geom_bar(data = dplyr::filter(income, race == "White Alone"), stat = "identity", fill = "#96bb7c", alpha = 0.95) +
  coord_flip() +
  labs(title = "<span style='color:#d6efc7;'>ASIAN  ALONE</span> | <span style='color:#96bb7c;'>WHITE ALONE</span>",x="", y="") +
  scale_y_continuous(limits = c(-20,20), breaks = c(-15,0,15), labels=c("15%","0","15%")) +
  theme_minimal() +
  #annotate("text", x = "$200,000 and over", y = -15, label = "A S I A N\n O R C O M B I N A T I O N", color = "#96bb7c", size = 4, alpha = 0.9) +
  #annotate("text", x = "$200,000 and over", y = 20, label = "W H I T E\n  A L O N E", color = "#d6efc7", size = 4, alpha = 0.9) +
  theme(panel.background = element_rect(fill = "#184d47", color = "NA"),
        plot.background = element_rect(fill = "#184d47", color = "NA"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8, color = "grey"),
        axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = 0.5, size =11, color = "grey", margin = margin(0,0,20,0)))
plot2

##
income$income_distribution <- ifelse(income$race == "Hispanic (Any Race)", -1*income$income_distribution, income$income_distribution)

plot3 <- ggplot(income, aes(x = income_bracket, y = income_distribution)) +
  geom_bar(data = dplyr::filter(income, race == "Hispanic (Any Race)"), stat = "identity", fill = "#f2aaaa", alpha = 0.95) + 
  geom_bar(data = dplyr::filter(income, race == "Asian Alone or in Combination"), stat = "identity", fill = "#f7f5dd", alpha = 0.95) +
  coord_flip() +
  labs(title = "<span style='color:#f2aaaa;'>HISPANIC</span> | <span style='color:#f7f5dd;'>ASIAN A.C.</span>",x="", y="") +
  scale_y_continuous(limits = c(-20,20), breaks = c(-15,0,15), labels=c("15%","0","15%")) +
  theme_minimal() +
  #annotate("text", x = "$200,000 and over", y = -15, label = "A S I A N\n O R C O M B I N A T I O N", color = "#96bb7c", size = 4, alpha = 0.9) +
  #annotate("text", x = "$200,000 and over", y = 20, label = "W H I T E\n  A L O N E", color = "#d6efc7", size = 4, alpha = 0.9) +
  theme(panel.background = element_rect(fill = "#184d47", color = "NA"),
        plot.background = element_rect(fill = "#184d47", color = "NA"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8, color = "grey"),
        axis.text.y = element_blank(),
        plot.title = element_markdown(hjust = 0.5, size =11, color = "grey", margin = margin(0,0,20,0)))
plot3

##BLACK and ASIAN
income$income_distribution <- ifelse(income$race == "Asian Alone", -1*income$income_distribution, income$income_distribution)

plot4 <- ggplot(income, aes(x = income_bracket, y = income_distribution)) +
  geom_bar(data = dplyr::filter(income, race == "Black Alone"), stat = "identity", fill = "#eebb4d", alpha = 0.95) + 
  geom_bar(data = dplyr::filter(income, race == "Asian Alone"), stat = "identity", fill = "#d6efc7", alpha = 0.95) +
  coord_flip() +
  labs(title = "<span style='color:#d6efc7;'>ASIAN ALONE</span> <span style='color:#d6efc7;'>| <span style='color:#eebb4d;'> 
       BLACK ALONE</span>",x="", y="") +
  scale_y_continuous(limits = c(-20,20), breaks = c(-15,0,15), labels=c("15%","0","15%")) +
  theme_minimal() +
  scale_x_discrete(labels = c("Under $15K", "$15-$25K", "$25-$35K",
                              "$35-$50K", "$50K-$75K", "$75-$100K",
                              "$100-$150K", "$150-$200K", "$200K and over")) +
 theme(panel.background = element_rect(fill = "#184d47", color = "NA"),
        plot.background = element_rect(fill = "#184d47", color = "NA"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 8, color = "grey"),
        plot.title = element_markdown(hjust = 0.5, size =11, color = "grey", margin = margin(0,0,20,0)))
plot4


ggdraw() +
  draw_text("Visualization: Laura Navarro Soler | Data: Urban Institute and US Census",
            size = 10, x = 0.35, y = 0.2, hjust = 0,
            color = "grey", family = "Leelawadee UI Semilight") -> caption

ggdraw() +
  draw_text("Income breaks",
            size = 28,
            color = "grey", family = "Georgia") -> title
ggdraw() +
  draw_text("in the US, 2019",
            size = 14,
            color = "grey", family = "Georgia") -> subtitle

titular <- plot_grid(title, subtitle, ncol=1)

#row of plots
row_plots <- plot_grid(plot1, plot2, plot3, ncol=3)

#all together
final_plot <- plot_grid(
  titular,
  plot4,
  row_plots,
  caption,
  
  ## plot settings
  rel_heights = c(1.2,4,2,0.3),
  nrow = 4,
  ncol = 1
) +
  theme(plot.background = element_rect(fill = "#184d47", colour = NA))
final_plot


#saving
ggsave("income.png", 
       final_plot, 
       height = 8, width = 9, 
       units = "in", dpi = 300)
