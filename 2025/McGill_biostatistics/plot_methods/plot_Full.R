# Investigation of CB in real NBA data ------------------------------------

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(reshape2)
library(tibble)
library(latex2exp)
library(pbapply)
library(rlist)
library(RSpectra)
source("./utility.R")


grid <- seq(0, 1, length.out = 5761)
my_grid <- seq(0, 1, length.out = 721)

nba_2018_df <- load_nba_data("./data/nba_2018.csv", grid, my_grid) 
nba_2019_df <- load_nba_data("./data/nba_2019.csv", grid, my_grid) 

ngame <- n_games(nba_2019_df)
nsamp <- length(my_grid)
rm(grid)

#' Parameters for the data generation


# In the following plots of the LOGIT fit, I delete the last time point which the game is over (t=1) and we know the result.

#' Parameter for generating the eigenvalues, and p-values
D <- 10 # Number of eigenvalues to keep
N_MC <- 5000 # for simulating the p-value

# Fit model -----------------------------------------------------------
model_logit <- nba_2018_df %>%
  group_by(grid) %>% group_split() %>%
  pblapply(function(x) {
    gfit = stats::glm(
      as.factor(Y) ~ IP + SD,
      data = x,
      family = binomial(link = "logit")
    )
    gfit$data <- NULL
    return(gfit)
  })


try <- nba_2019_df %>%
  group_by(grid) %>% group_split()

ind_na <- mod_coef_err(model_logit, my_grid)
result <- pblapply(seq_along(my_grid), function(x) {
  pr = predict(object = model_logit[[x]],
               newdata = try[[x]],
               type = "response")
  try[[x]] %>% dplyr::select(grid, game_num, IP) %>% 
    mutate(phat_logit = ifelse(grid %in% ind_na, IP, pr)) 
}) %>% bind_rows() %>%
  inner_join(nba_2019_df, by = c("game_num", "grid", "IP")) %>% 
  mutate(phat_logit = ifelse( ( grid %in% 1 & phat_espn %in% 0.5), 0.5, phat_logit)) %>% 
  dplyr::select(grid, game_num, phat_espn, phat_logit, Y, IP, SD) # for reorder the covariates

rm(ind_na, model_logit)

df_equ <- result %>% group_by(grid) %>% 
  rename(phat_1 = phat_espn, phat_2 = phat_logit) %>% 
  mutate(p_bar_12 = mean(phat_1 - phat_2), 
         diff_A = phat_1 - phat_2,
         diff_B = phat_2 - phat_2 - p_bar_12) %>% ungroup()

# Apply our test -----------------------------------------------------------------------------------
Z <- df_equ %>% calc_Z()

p_hat <- df_equ %>% calc_p_val()


library(ggplot2)
library(dplyr)
library(latex2exp)

vlines <- c(0.25, 0.5, 0.75)
breaks <- c(0.125, 0.375, 0.625, 0.875)
labels <- c("Q1", "Q2", "Q3", "Q4")

my_label <- latex2exp::TeX(sprintf("P-value of $H_0:$ $||\\Delta_N||^2=0$ is $%.3f$", p_hat))

ma_coef <- floor(nsamp * 0.05)

df_equ %>%
  calc_L_s2() %>%  
  L_smoothing(ma_coef) %>%
  ggplot(aes(x = grid, y = L)) + 
  geom_line(colour = "black", size = 2) +
  geom_ribbon(aes(ymin = LB_t, ymax = UB_t),
              fill = "transparent", linetype = 2, color = "red") +
  geom_hline(yintercept = 0, colour = "blue", size = 2) + 
  
  # Arrows
  annotate("segment", x = 1.02, xend = 1.02, y = 0, yend = -0.005, colour = "purple", 
           arrow = arrow(length = unit(0.45, "cm")), size = 2) +
  annotate("segment", x = 1.02, xend = 1.02, y = 0, yend = 0.008, colour = "orange", 
           arrow = arrow(length = unit(0.45, "cm")), size = 2) +
  
  # Vertical dotted lines at 0.25, 0.5, 0.75
  geom_vline(xintercept = vlines, linetype = "dotted", color = "gray40") +
  
  xlab("") +
  ylab(TeX('$\\hat{\\Delta}_N(t)$')) +  
  scale_x_continuous(breaks = breaks, labels = labels, limits = c(0, 1.05)) +
  
  theme(axis.title = element_text(size = rel(2.5)),
        axis.text = element_text(size = rel(2.0))) +
  # Annotations
  annotate(geom = "text", x = 0.66, y = 0.0065, label = expression("Favour Full"),
           color = "orange", size = rel(6)) + 
  annotate(geom = "text", x = 0.70, y = -0.004, label = "Favour ESPN",
           color = "purple", size = rel(6)) +
  annotate("label", -Inf, Inf, label = my_label,
           hjust = 0, vjust = 1, size = rel(6))  + 
  annotate("text", 
           x = c(0.125, 0.375, 0.625, 0.875), 
           y = -0.006,   # ⬅ slightly above
           label = c("Q1", "Q2", "Q3", "Q4"), 
           size = rel(5))
 
