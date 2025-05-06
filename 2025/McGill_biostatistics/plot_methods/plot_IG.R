#' Purpose: To calculate the relative Brier score
#'   Between ESPN model v.s. the Score difference (value) only model 
#' Author: Chi-Kuang Yeh
#' Modified Date: Jun14, 2020

# Initialization -----------------------------------------------------------
library(plyr)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(reshape2)
library(pbapply)
library(latex2exp)
source("./utility.R")


# Import NBA data ---------------------------------------------------------
grid <- seq(0, 1, length.out = 5761)
my_grid <- seq(0, 1, length.out = 721)

nba_2018_df <- load_nba_data("./data/nba_2018.csv", grid, my_grid) 
nba_2019_df <- load_nba_data("./data/nba_2019.csv", grid, my_grid) 

ngame <- n_games(nba_2019_df)
nsamp <- length(my_grid)
D <- 10
N_MC <- 5000

rm(grid)

# try ---------------------------------------------------------------------
model_logit_SD <- nba_2018_df %>%
  group_by(grid) %>% group_split() %>%
  pblapply(function(x) {
    gfit = stats::glm(
      as.factor(Y) ~ SD,
      data = x,
      family = binomial
    )
    gfit$data <- NULL
    return(gfit)
  })

try <- nba_2019_df %>%
  group_by(grid) %>% group_split()



my_df <- pblapply(seq_along(my_grid), function(x) {
  pr = predict(object = model_logit_SD[[x]],
               newdata = try[[x]],
               type = "response")
  try[[x]] %>% dplyr::select(grid, game_num) %>% mutate(phat_glm_SD = pr)
}) %>% bind_rows() %>% 
  inner_join(nba_2019_df, by = c("game_num", "grid")) %>% 
  dplyr::select(grid, game_num, Y, phat_espn, phat_glm_SD)

# calculate Z using our test ----------------------------------------------
df_equ <- my_df %>% group_by(grid) %>% 
  rename(phat_1 = phat_espn, phat_2 = phat_glm_SD) %>% 
  mutate(p_bar_12 = mean(phat_1 - phat_2), 
         diff_A = phat_1 - phat_2,
         diff_B = phat_2 - phat_2 - p_bar_12) %>% ungroup()


Z <- df_equ %>% calc_Z()

p_hat <- df_equ %>% calc_p_val()


my_label <- latex2exp::TeX(sprintf("P-value of test of $H_0:$ $||\\Delta_N||^2=0$ is $%.0f$", p_hat))

# Smoothing ---------------------------------------------------------------
ma_coef <- floor(nsamp * 0.05)
df_equ %>% 
  calc_L_s2() %>%  L_smoothing(., ma_coef) %>%
  ggplot(aes(x = grid, y = L)) + geom_line(colour = "black", size = 2) +
  geom_ribbon(aes(ymin = LB_t, 
                  ymax = UB_t),
              fill = "transparent", linetype = 2 , color = "red") +
  geom_hline(aes(yintercept = 0), colour = "blue", size = 2) + 
  geom_segment(aes(x = 1.02, xend = 1.02, y = 0, yend = -0.042), colour = "purple", 
               arrow = arrow(length = unit(0.45, "cm")), size = 2) +
  geom_segment(aes(x = 1.02, xend = 1.02, y = 0, yend = 0.010), colour = "orange", 
               arrow = arrow(length = unit(0.45, "cm")), size = 2) +
  xlab("") +
  ylab(TeX('$\\hat{\\Delta}_N(t)$')) +  
  theme(axis.title = element_text(size = rel(2.5)),
        axis.text = element_text(size = rel(2.0))) + 
  coord_cartesian(expand = 0.5) +
  annotate(geom = "text", x = 0.65, y = 0.004, label = "Favour In Game",
           color = "orange", size = rel(6)) + 
  annotate(geom = "text", x = 0.70, y = -0.035, label = "Favour ESPN",
           color = "purple", size = rel(6)) +
  annotate("label", -Inf, Inf, label = my_label,
           hjust = 0, vjust = 1, size = rel(6)) + 
  annotate("text", 
           x = c(0.125, 0.375, 0.625, 0.875), 
           y = -0.042,   # ⬅ slightly above
           label = c("Q1", "Q2", "Q3", "Q4"), 
           size = rel(5)) + 
  # Vertical dotted lines at 0.25, 0.5, 0.75
  geom_vline(xintercept = vlines, linetype = "dotted", color = "gray40")

