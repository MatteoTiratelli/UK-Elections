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
