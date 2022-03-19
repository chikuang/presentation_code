library(sde)
library(dplyr)
library(ggplot2)
library(fdaACF)
library(pbapply)
library(tibble)
library(tidyr)
source("./util.R")
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Simulate Brownian bridge
N <- 1000
Nt <- 101
H <- 30
alpha <- 0.05
s <- seq(0, 1, length.out = Nt)
set.seed(520)
obs <- pbsapply(1:N, function(i){
  sde::BBridge()
}) %>% t()

# Plot the observations 
Nplot <- 50
df_obs <- cbind(time = s, val = t(obs[1:Nplot, ])) %>% as_tibble()
names(df_obs) <-  c("time", paste0("obs_", 1:50))

df_res <- pivot_longer(df_obs, !time, 
                       names_to = "obs", values_to = "val")
ggplot(df_res, aes(x = time, y = val, col = obs)) +
  geom_line() +
  theme(legend.position = "none") + xlab("Time (t)") + ylab("") +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Brownian bridge")

# FACF
FACF <- obtain_FACF(Y = obs, v = s, nlags = H, ci = 1 - alpha,
                    figure = FALSE)
h <- 1:H
df_facf <- tibble(rho = FACF$rho, h = h, blueline = FACF$Blueline)
ggplot(data = df_facf, mapping = aes(x = h, y = rho)) +
  geom_hline(aes(yintercept = blueline), 
             linetype = "longdash", color = "blue") + 
  geom_segment(mapping = aes(xend = h, yend = 0), 
               size = 1, col = "black") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Lag (h)", y = " ") + 
  ggtitle("FACF of Brownian bridge")

# DACF
df_dacf <- my_new_receipt(obs, H) %>% 
  mutate(lb = qnorm(alpha/2) * std_0_cen/sqrt(N), 
         ub = qnorm(1 - alpha/2) * std_0_cen/sqrt(N))

ggplot(data = df_dacf, mapping = aes(x = h, y = rho_cen)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = h, yend = 0), size = 1, 
               col = "black") + 
  geom_ribbon(aes(ymin = lb, ymax = ub), 
              linetype = "longdash", fill = NA, color = "blue") + 
  labs(x = "Lag (h)", y = " ")  + 
  ggtitle("DACF of Brownian bridge")
