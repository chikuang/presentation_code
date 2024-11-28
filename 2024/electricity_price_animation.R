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
theme_update(plot.title = element_blank(),
             legend.position = "none", 
             axis.line=element_blank(),axis.text.x=element_blank(),
             axis.text.y=element_blank(),axis.ticks=element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             panel.background = element_blank(),panel.border=element_blank(),
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(), plot.background=element_blank())

library(FTSgof)
library(tidyr)
library(dplyr)
data(Spanish_elec)

data_df <- as.data.frame(Spanish_elec)
colnames(data_df) <- paste0(1:ncol(data_df))

long_df <- data_df %>%
  pivot_longer(
    cols = everything(),       # Include all columns
    names_to = "Day",          # Column for day names
    values_to = "Value"        # Column for values
  ) %>%
  mutate(Hour = rep(1:nrow(data_df), each = ncol(data_df))) # Add Hour column

my_df <- long_df %>% mutate(Day = as.numeric(Day), Hour = as.numeric(Hour)) %>% 
  arrange(Day, Hour)
# Display the resulting long dataframe
print(my_df)

# Create animation --------------------------------------------------------
plot1 <- my_df %>% mutate(Day_index = as.Date(Day, format = "%Y-%m-%d"),
                            Day = factor(Day)) %>% 
  ggplot(aes(x = Hour, y = Value,  colour = Day)) +
  geom_line() +
  scale_colour_manual(values = rainbow(365)) 
play <- plot1 +  transition_reveal(Day)
animate(play,renderer = gifski_renderer())

anim <- plot1 +
  # labs(title = "Japanese mortality data", x = "Age", y = "Log mortality") +
  transition_reveal(Day_index, keep=TRUE) +
  ease_aes('linear') 
anim
anim_save("fts_spanish_electricity.gif", last_animation())
