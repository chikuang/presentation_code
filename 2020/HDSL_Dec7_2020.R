library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(lubridate)
# Election ----------------------------------------------------------------

library(fivethirtyeight)

governor_national_forecast %>% filter(party == "D") %>%
  ggplot(aes(x = forecastdate, y = win_probability, 
             col = model)) +
  geom_line() +
  ylab("Win probability of Democracy") + 
  xlab("Date") + 
  ggtitle("2018 USA National Race Forecast by 538") + 
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.5)),
        legend.text  = element_text(size = rel(1.25)),
        plot.title = element_text(hjust = 0.5,
                                  size= rel(1.75)))


# NBA game ----------------------------------------------------------------
source("D:/My_Documents/Github/RTPForNBA/utility.R")
grid <- seq(0, 1, 1 / 2880 / 2)
my_grid <- seq(0, 1, 1 / 2880 * 4)

df_temp <- read_csv("D:/My_Documents/Github/RTPForNBA/data/nba_2019.csv") %>% 
  filter(grepl("lakers", home_team),grepl("celtics", away_team)) %>% 
  time_conversion() %>% 
  avg_tp()

df_nba_2019 <- load_nba_data("D:/My_Documents/Github/RTPForNBA/data/nba_2019.csv", 
                             grid, my_grid)

# df_nba_2019%>% filter(game_num == 293) 
fig <- df_nba_2019 %>% filter(game_num == 11) %>% 
  ggplot(aes(grid, phat_espn)) + geom_line(size = 1) + 
  geom_point(col = "red", size = 2) + 
  geom_point(aes(x = 1, y = Y ), col = "blue", size = 5) +  
  ggtitle("Probabilitistic Forecast of a Basketball Game") + 
  ylab("Probability of home team winning") + 
  xlab("In-game time elapsed (%)") + 
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.5)),
        legend.text  = element_text(size = rel(1.25)),
        plot.title = element_text(hjust = 0.5,
                                  size= rel(1.75))) 


fig <- df_nba_2019 %>% filter(game_num == 11) %>% 
  ggplot(aes(grid, phat_espn)) + 
  geom_line(aes(col = "Interpolated \nprob."), size = 1) + 
  geom_point(aes(col = "Obs."), size = 2) + 
  geom_point(aes(x = 1, y = Y, col = "Outcome" ), size = 5) +  
  ggtitle("Probabilitistic Forecast of a Basketball Game") + 
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.5)),
        legend.text  = element_text(size = rel(1.25)),
        plot.title = element_text(hjust = 0.5,
                                  size= rel(1.75))) 


colors <- c("Outcome" = "blue", "Obs." = "red", "Interpolated \nprob." = "black")
fig + 
  labs(x = "In-game time elapsed (%)",
       y = "Probability of home team winning",
       color = "") +
  scale_color_manual(values = colors)

N <- 5000
df <- tibble(var_norm = rnorm(N, sd = 5), var_unif = runif(N)) %>% 
  mutate(var_norm = var_norm/(max(var_norm) - min(var_norm)),
         var_norm = var_norm - min(var_norm))

ggplot(df, aes(x = var_norm)) + 
  geom_histogram(aes(y= ..count../sum(..count..)), binwidth = 0.1) +
  xlab("Probabilistic forecast") + 
  ylab("Proportion") + 
  ylim(0, 0.35) +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.5)),
        legend.text  = element_text(size = rel(1.25)),
        plot.title = element_text(hjust = 0.5,
                                  size= rel(1.75)))

ggplot(df, aes(x = var_unif)) + 
  geom_histogram(aes(y= ..count../sum(..count..)), binwidth = 0.1) +
  xlab("Probabilistic forecast") + 
  ylab("Proportion") + 
  ylim(0, 0.35) +
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.5)),
        legend.text  = element_text(size = rel(1.25)),
        plot.title = element_text(hjust = 0.5,
                                  size= rel(1.75)))
