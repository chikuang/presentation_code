library(ggplot2)
library(tibble)
library(dplyr)
library(forcats)
library(tidyr)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             legend.position="none",
             axis.text=element_text(size=12),
             axis.title=element_text(size=16,face="bold"))
y <- x <- c(-1, 0, 1)
df <- tibble(expand.grid(x1 = x, x2 = y)) %>% rowid_to_column("id") %>% 
  mutate(id = as_factor(id))
df %>% mutate(id = fct_relevel(id, "7","8","9","4","5","6",
                              "1","2","3")) %>% 
  ggplot(aes(x1, x2)) + geom_point(aes(col = id, size = 2)) +
  labs(title = "Spatial locations") + 
  xlab(bquote(x[1])) +
  ylab(bquote(x[2]))

set.seed(777)
nt <- 20
m <- matrix(rnorm(nrow(df)*nt,4), nrow = nrow(df))
m[3,] <- rexp(nt)
m[6,] <- rchisq(nt, 3)
colnames(m) <- c(1:nt)
df_val <- as_tibble(m)

df_wider <- bind_cols(df, df_val)
df_longer <- pivot_longer(df_wider, !c(id, x1, x2),
             names_to = "time", values_to = "val") %>% 
  mutate(time = as.numeric(time)) 

common_trend <- c(rep(rexp(1),4), rexp(1), -rexp(5), -0.7*rep(rexp(1),2), rep(rexp(1),3), rexp(5))
df_longer %>% mutate(val = case_when(time == 1 ~ val + common_trend[1],
                                     time == 2 ~ val + common_trend[2],
                                     time == 3 ~ val + common_trend[3],
                                     time == 4 ~ val + common_trend[4],
                                     time == 5 ~ val + common_trend[5],
                                     time == 6 ~ val + common_trend[6],
                                     time == 7 ~ val + common_trend[7],
                                     time == 8 ~ val + common_trend[8],
                                     time == 9 ~ val + common_trend[9],
                                     time == 10 ~ val + common_trend[10],
                                     time == 11 ~ val + common_trend[11],
                                     time == 12 ~ val + common_trend[12],
                                     time == 13 ~ val + common_trend[13],
                                     time == 14 ~ val + common_trend[14],
                                     time == 15 ~ val + common_trend[15],
                                     time == 16 ~ val + common_trend[16],
                                     time == 17 ~ val + common_trend[17],
                                     time == 18 ~ val + common_trend[18],
                                     time == 19 ~ val + common_trend[19],
                                     time == 20 ~ val + common_trend[20])) %>% 
  ggplot() + geom_line(aes(x = time, y = val, col = id)) + 
  labs(title = "Raw data")

df_trend <- tibble(val = common_trend, time = 1:nt)
df_trend %>% ggplot(aes(x = time, y = val)) +
  geom_smooth(col = "red") + 
  labs(title = "Common trend")

hospital_names <- c(
  `1` = "(-1,-1)",
  `2` = "(0,-1)",
  `3` = "(1,-1)",
  `4` = "(-1,0)",
  `5` = "(0,0)",
  `6` = "(1,0)",
  `7` = "(-1,1)",
  `8` = "(0,1)",
  `9` = "(1,1)"
)

library(ggh4x)
strip <- strip_themed(background_x = elem_list_rect(fill = rainbow(9)))
df_longer %>% 
  mutate(id = fct_relevel(id, "7","8","9","4","5","6",
                                                    "1","2","3")) %>% 
  group_by(id) %>% ggplot(aes(x=time, y=val, col = id)) + 
  geom_point(linewidth=1.5) + 
  geom_smooth() + 
  labs(title = "Temporal process on each location") + 
  # facet_wrap2(~id, labeller = as_labeller(hospital_names), strip = strip)
  facet_wrap2(~id, labeller = as_labeller(hospital_names))
