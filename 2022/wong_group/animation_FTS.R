library(gganimate)
library(dplyr)
library(rainbow)
library(reshape2)
library(readr)
library(pbapply)
library(tidyr)
library(magick)
library(gifski)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             legend.position = "none")
dat <- read_csv('./jap_mor_raw.csv')
df_female_log <- pbsapply(1947:2017, function(i){
  dat %>% filter(Year == i, Age %in% (0:100)) %>% 
    mutate(Female_log = log(as.numeric(Female))) %>% pull(Female_log)
}) %>% t()

# 3D plot
df <- df_female_log %>% melt() %>% as_tibble()
colnames(df) <- c("Year", "Age", "val")
df <- df %>% group_by(Age) %>% mutate(Year = 1947:2017)
library("zoo")
df <- mutate(df, Year_index = Year,
             Year = as.Date(as.yearmon(Year)))

N_year <- group_by(df, Year) %>% summarise(n=n()) %>% nrow()
N_age <- group_by(df, Age) %>% summarise(n=n()) %>% nrow()
  

plot1 <- df %>% mutate(Year_index = factor(Year_index)) %>% ggplot(aes(x = Age, y = val,  colour = Year_index)) +
  geom_line() +
  scale_colour_manual(values = rainbow(71)) 

play <- plot1 +  transition_reveal(Year_index)
animate(play,renderer = gifski_renderer())

anim <- plot1 +
    labs(title = "Japanese mortality data", x = "Age", y = "Log mortality") +
  transition_reveal(Year) +
  ease_aes('linear')
anim
anim_save("fts.gif", last_animation())

## Let's also make one gif for scalar time series data
library(astsa)
N <- 200
df2 <- tibble(day = 1:N, val = nyse[1:N])
df2 <- mutate(df2, date = as.Date(day))
plot2 <- ggplot(df2, aes(x = day, y = val)) +
  geom_point(aes(group = seq_along(date))) +
  geom_point(col = "red", size = 2) 
# + geom_line(col = "black")


anim2 <- plot2 +
  labs(title = "New york stock", x = "Day", y = "Value") +
  transition_reveal(date, keep = TRUE) +
  ease_aes('linear')
anim2
anim_save("sts.gif", last_animation())
