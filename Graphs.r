# UK Electoral History

library(tidyverse)

theme_base <- function (X) { 
  theme_bw(base_family = X) %+replace% 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background= element_rect(fill = "transparent", colour = NA)
    )
}


data <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/UK-Elections/main/Elections.csv")

data <- pivot_longer(data, -c(Year, Winner), names_to = "variable", values_to = "value")

ggplot(data[data$variable %in% c('Turnout'),], aes(x = Year, y = value)) +
  geom_col(aes(colour = Winner, fill = Winner)) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "Turnout at UK General Elections", caption = "Source: House of Commons Library") +
  geom_smooth(method = lm, se = FALSE, colour = 'black', size=0.5) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_manual(values=c("Conservative" = "blue", "Labour"="red"),
                    name = "Winner:", labels = c("Tory","Labour")) +
  scale_colour_manual(values=c("Conservative" = "blue", "Labour"="red"),
                      name = "Winner:", labels = c("Tory","Labour")) +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"))


ggplot(data[data$variable %in% c('Lab_vote','Con_vote'),], aes(x = Year, y = value, colour = variable, group = variable)) +
  geom_line() +
  xlab(NULL) + ylab(NULL) +
  labs(title = "Vote share at UK General Elections", caption = "Source: House of Commons Library") +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_manual(values=c("Con_vote" = "blue", "Lab_vote" = "red"), 
                      name = "Vote share:", labels = c("Tory","Labour")) +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"))

ggplot(data[data$variable %in% c('Lab_seats','Con_seats'),], aes(x = Year, y = value, colour = variable, group = variable)) +
  geom_line() +
  xlab(NULL) + ylab(NULL) +
  labs(title = "Seat share at UK General Elections", caption = "Source: House of Commons Library") +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_manual(values=c("Con_seats" = "blue", "Lab_seats" = "red"), 
                      name = "Seat share:", labels = c("Tory","Labour")) +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"))


ggplot(drop_na(data[data$variable %in% c('MPs_Business','MPs_Political','MPs_Professionals','MPs_Manual'),]),
       aes(x = Year, y = value, colour = variable, group = variable)) +
  geom_line() +
  xlab(NULL) + ylab(NULL) +
  labs(title = "MPs occupational backgrounds", caption = "Source: House of Commons Library") +
  scale_y_continuous(limits = c(0,100)) +
  scale_colour_manual(values = c('MPs_Business' = 'blue','MPs_Political' = 'purple',
                                 'MPs_Professionals' = 'green','MPs_Manual' = 'red'),
                      name = NULL, labels = c("Business", "Manual labour", "Political work", "Professional")) +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"))


ggplot(data[data$variable %in% c('Con_seats','Con_vote'),], aes(x = Year, y = value)) +
  geom_point(aes(shape = variable), colour = "blue") +
  geom_path(arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  xlab(NULL) + ylab(NULL) +
  labs(title = "The Effect of First Past The Post: Conservative Party", caption = "Source: House of Commons Library") +
  scale_y_continuous(limits = c(0,100)) +
  scale_shape_manual(values=c("Con_seats" = 15, "Con_vote" = 16),
                     name = NULL, labels = c("Tory Seats", "Tory Votes")) +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"))

ggplot(data[data$variable %in% c('Lab_seats','Lab_vote'),], aes(x = Year, y = value)) +
  geom_point(aes(shape = variable), colour = "red") +
  geom_path(arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  xlab(NULL) + ylab(NULL) +
  labs(title = "The Effect of First Past The Post: Labour Party", caption = "Source: House of Commons Library") +
  scale_y_continuous(limits = c(0,100)) +
  scale_shape_manual(values=c("Lab_seats" = 15, "Lab_vote" = 16),
                     name = NULL, labels = c("Labour Seats", "Labour Votes")) +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"))

data[!(data$Year %in% c('1918','1922','1923','1924','1929','1931','1935')),] -> data1

ggplot(data = data1[data1$variable %in% c('Lab_seats_raw', "Lab_vote"),], aes(x = Year, y = value)) +
  geom_col(fill = 'grey80') +
  xlab(NULL) + ylab(NULL) +
  labs(title = "The Labour Party's Electoral Performance", caption = "Source: House of Commons Library") +
  theme_base('sans') +
  facet_grid(rows = vars(variable), scales = "free", space = "fixed") +
  geom_col(data = data1[data1$variable %in% c('Lab_seats_raw', "Lab_vote") & data1$Year == "2019",], aes(x = Year, y = value), fill = 'red') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


devtools::install_github("jackobailey/britpol")
library(britpol)

theme_base <- function (X) {
  theme_bw(base_family = X) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0, vjust = -1),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background= element_rect(fill = "transparent", colour = NA)
    )
}

data("pollbase")
data("pollbasepro")

data1 <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/UK-Elections/main/Elections.csv")

data1 <- pivot_longer(data1, -c(Year, Winner), names_to = "variable", values_to = "value")
data1 <- data1[!(data1$Year %in% c('1918','1922','1923','1924','1929','1931','1935')),]

ggplot(data = data1[data1$variable %in% c('Lab_vote_adjusted'),], aes(x = Year, y = value)) +
  geom_line(group = 1, colour = 'lightpink') +
  xlab(NULL) + ylab(NULL) +
  labs(title = "Labour's share of the electorate, 1945 - 2019", caption = "Source: House of Commons Library") +
  theme_base('sans') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> Figure0

ggsave('Figure0.png', plot = Figure0, device = 'png', dpi = 320, width=5, height=4, bg = "white")


######################

pollbasepro$year <- substr(pollbasepro$date,1,4)

elections <- tibble(date = as.Date(c('2019-12-12','2017-06-08','2015-05-07','2010-05-06','2005-05-05','2001-06-07','1997-05-01','1992-04-09','1987-06-11')),
               Year = c(2019,2017,2015,2010,2005,2001,1997,1992,1987))
elections <- merge(elections, data),
                   by = 'Year', all.x = TRUE, all.y = FALSE)

pollbasepro2 <- merge(pollbasepro, elections,
                      by = 'date', all.x = TRUE, all.y = FALSE)
pollbasepro2$Turnout <- zoo::na.approx(pollbasepro2$Turnout, na.rm = FALSE)
pollbasepro2 <- pollbasepro2[pollbasepro2$year>1991,]
pollbasepro2$Turnout[is.na(pollbasepro2$Turnout)] <- seq(from = 67.3, by = -0.00164, length.out = sum(is.na(pollbasepro2$Turnout)))


##########################

ggplot(pollbasepro[pollbasepro$year>=1996 & pollbasepro$year<=2010,], aes(x = date, y = lab_est)) +
  geom_line(colour = 'lightpink') +
  geom_smooth(method = "lm", se = FALSE, colour = 'darkgrey', size = 0.2) +
  geom_point(data = elections, aes(x = date, y = Lab_vote/100), shape = 2) +
  scale_x_date(limits = c(as.Date('1997-04-01'), as.Date('2010-06-06'))) +
  scale_y_continuous(limits = c(0.15, 0.59)) +
  theme_base('sans') + ylab('Labour') + xlab(NULL) +
  labs(title = "Elections & poll averages under New Labour") -> L1

ggplot(pollbasepro[pollbasepro$year>1996 & pollbasepro$year<=2010,], aes(x = date, y = con_est)) +
  geom_line(colour = 'lightblue') +
  geom_smooth(method = "lm", se = FALSE, colour = 'darkgrey', size = 0.2) +
  geom_point(data = elections, aes(x = date, y = Con_vote/100), shape = 2) +
  scale_x_date(limits = c(as.Date('1997-04-01'), as.Date('2010-06-06'))) +
  scale_y_continuous(limits = c(0.15, 0.59)) +
  theme_base('sans') + ylab('Tory') + xlab(NULL) +
  labs(caption = "Source: Jack Bailey's BritPol database") -> T1

gridExtra::grid.arrange(L1, T1, nrow = 2, ncol = 1) -> Figure1

ggsave('Figure1.png', plot = Figure1, device = 'png', dpi = 320, width=5, height=4, bg = "white")


ggplot(pollbasepro2[pollbasepro2$year>=2010,], aes(x = date, y = lab_est)) +
  geom_line(colour = 'lightpink') +
  geom_smooth(method = "lm", se = FALSE, colour = 'darkgrey', size = 0.2) +
  geom_point(data = elections, aes(x = date, y = Lab_vote/100), shape = 2) +
  scale_x_date(limits = c(as.Date('2010-04-01'), as.Date('2022-01-22'))) +
  scale_y_continuous(limits = c(0.15, 0.59)) +
  theme_base('sans') + ylab('Labour') + xlab(NULL) +
  labs(title = "Elections & poll averages since 2010")-> L2

ggplot(pollbasepro2[pollbasepro2$year>=2010,], aes(x = date, y = con_est)) +
  geom_line(colour = 'lightblue') +
  geom_smooth(method = "lm", se = FALSE, colour = 'darkgrey', size = 0.2) +
  geom_point(data = elections, aes(x = date, y = Con_vote/100), shape = 2) +
  scale_x_date(limits = c(as.Date('2010-04-01'), as.Date('2022-01-22'))) +
  scale_y_continuous(limits = c(0.15, 0.59)) +
  theme_base('sans') + ylab('Tory') + xlab(NULL) +
  labs(caption = "Source: Jack Bailey's BritPol database") -> T2

gridExtra::grid.arrange(L2, T2, nrow = 2, ncol = 1) -> Figure2

ggsave('Figure2.png', plot = Figure2, device = 'png', dpi = 320, width=5, height=4, bg = "white")



##########################

ggplot(pollbasepro[pollbasepro$date>as.Date('2010-04-01'),], aes(x = date, y = lab_est)) +
  geom_line(colour = 'lightpink') +
  geom_point(data = elections, aes(x = date, y = Lab_vote/100), shape = 2) +
  geom_segment(aes(x = as.Date('2010-09-25'), y = min(pollbasepro$lab_est[pollbasepro$date>=as.Date('2010-09-25') & pollbasepro$date<=as.Date('2015-05-08')]), xend = as.Date('2015-05-08'),
                   yend = min(pollbasepro$lab_est[pollbasepro$date>=as.Date('2010-09-25') & pollbasepro$date<=as.Date('2015-05-08')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2010-09-25'), y = max(pollbasepro$lab_est[pollbasepro$date>=as.Date('2010-09-25') & pollbasepro$date<=as.Date('2015-05-08')]), xend = as.Date('2015-05-08'),
                   yend = max(pollbasepro$lab_est[pollbasepro$date>=as.Date('2010-09-25') & pollbasepro$date<=as.Date('2015-05-08')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2015-09-12'), y = min(pollbasepro$lab_est[pollbasepro$date>=as.Date('2015-09-12') & pollbasepro$date<=as.Date('2020-04-04')]), xend = as.Date('2020-04-04'),
                   yend = min(pollbasepro$lab_est[pollbasepro$date>=as.Date('2015-09-12') & pollbasepro$date<=as.Date('2020-04-04')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2015-09-12'), y = max(pollbasepro$lab_est[pollbasepro$date>=as.Date('2015-09-12') & pollbasepro$date<=as.Date('2020-04-04')]), xend = as.Date('2020-04-04'),
                   yend = max(pollbasepro$lab_est[pollbasepro$date>=as.Date('2015-09-12') & pollbasepro$date<=as.Date('2020-04-04')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2020-06-04'), y = min(pollbasepro$lab_est[pollbasepro$date>=as.Date('2020-06-04') & pollbasepro$date<=as.Date('2022-01-22')]), xend = as.Date('2022-01-22'),
                   yend = min(pollbasepro$lab_est[pollbasepro$date>=as.Date('2020-06-04') & pollbasepro$date<=as.Date('2022-01-22')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2020-06-04'), y = max(pollbasepro$lab_est[pollbasepro$date>=as.Date('2020-06-04') & pollbasepro$date<=as.Date('2022-01-22')]), xend = as.Date('2022-01-22'),
                   yend = max(pollbasepro$lab_est[pollbasepro$date>=as.Date('2020-06-04') & pollbasepro$date<=as.Date('2022-01-22')])), size = 0.02, colour = 'darkgrey') +
  scale_x_date(limits = c(as.Date('2010-05-01'), as.Date('2022-01-22'))) +
  theme_base('sans') + ylab('Labour') + xlab(NULL) +
  labs(title = "Elections & poll averages since 2010",
       caption = "Source: Jack Bailey's BritPol database") -> Figure3

ggsave('Figure3.png', plot = Figure3, device = 'png', dpi = 320, width=5, height=4, bg = "white")

ggplot(pollbasepro2[pollbasepro2$year>=2010,], aes(x = date, y = con_est)) +
  geom_line(colour = 'lightblue') +
  geom_point(data = elections, aes(x = date, y = Con_vote/100), shape = 2) +
  geom_segment(aes(x = as.Date('2010-04-01'), y = min(pollbasepro$con_est[pollbasepro$date>=as.Date('2010-04-01') & pollbasepro$date<=as.Date('2016-07-13')]), xend = as.Date('2016-07-13'),
                   yend = min(pollbasepro$con_est[pollbasepro$date>=as.Date('2010-04-01') & pollbasepro$date<=as.Date('2016-07-13')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2010-04-01'), y = max(pollbasepro$con_est[pollbasepro$date>=as.Date('2010-04-01') & pollbasepro$date<=as.Date('2016-07-13')]), xend = as.Date('2016-07-13'),
                   yend = max(pollbasepro$con_est[pollbasepro$date>=as.Date('2010-04-01') & pollbasepro$date<=as.Date('2016-07-13')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2016-07-13'), y = min(pollbasepro$con_est[pollbasepro$date>=as.Date('2016-07-13') & pollbasepro$date<=as.Date('2019-07-24')]), xend = as.Date('2019-07-24'),
                   yend = min(pollbasepro$con_est[pollbasepro$date>=as.Date('2016-07-13') & pollbasepro$date<=as.Date('2019-07-24')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2016-07-13'), y = max(pollbasepro$con_est[pollbasepro$date>=as.Date('2016-07-13') & pollbasepro$date<=as.Date('2019-07-24')]), xend = as.Date('2019-07-24'),
                   yend = max(pollbasepro$con_est[pollbasepro$date>=as.Date('2016-07-13') & pollbasepro$date<=as.Date('2019-07-24')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2019-07-24'), y = min(pollbasepro$con_est[pollbasepro$date>=as.Date('2020-06-04') & pollbasepro$date<=as.Date('2022-01-22')]), xend = as.Date('2022-01-22'),
                   yend = min(pollbasepro$con_est[pollbasepro$date>=as.Date('2019-07-24') & pollbasepro$date<=as.Date('2022-01-22')])), size = 0.02, colour = 'darkgrey') +
  geom_segment(aes(x = as.Date('2019-07-24'), y = max(pollbasepro$con_est[pollbasepro$date>=as.Date('2019-07-24') & pollbasepro$date<=as.Date('2022-01-22')]), xend = as.Date('2022-01-22'),
                   yend = max(pollbasepro$con_est[pollbasepro$date>=as.Date('2019-07-24') & pollbasepro$date<=as.Date('2022-01-22')])), size = 0.02, colour = 'darkgrey') +
  scale_x_date(limits = c(as.Date('2010-04-01'), as.Date('2022-01-22'))) +
  theme_base('sans') + ylab('Tory') + xlab(NULL) +
  labs(title = "Elections & poll averages since 2010",
       caption = "Source: Jack Bailey's BritPol database") -> Figure4

ggsave('Figure4.png', plot = Figure4, device = 'png', dpi = 320, width=5, height=4, bg = "white")
