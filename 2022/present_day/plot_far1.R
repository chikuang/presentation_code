library(sde)
library(dplyr)
library(ggplot2)
library(fdaACF)
library(pbapply)
library(tibble)
library(tidyr)
source("./util.R")
N <- 1000
Nt <- 101
H <- 30
alpha <- 0.05
s <- seq(0, 1, length.out = Nt)
ker <- function(tt, s){
  exp( -(tt^2 + s^2)/2)
}
norm_const <- calc_ker_norm_const(-0.2, ker = ker)


# Simulate data

# par(mfrow = c(1, 1))
set.seed(888)
H <- 30

burn_in <- 50
N1 <- N + burn_in
tt <- s <- seq(0, 1, length.out = Nt)
Z <- rnorm(2)
my_sin <- sin(2*pi*s)
my_cos <- cos(2*pi*s)
x <- Z[1] * sqrt(2) * my_sin + Z[2] * sqrt(2) * my_cos
x1 <- rep(0, Nt)
X <- matrix(0, nrow = N1, ncol = Nt)
X[1, ] <- x
for(i in 2:N1){
  err_t <- as.numeric(sde::BBridge())
  
  
  x1 <- norm_const/(Nt - 1) * sum(exp(-s^2/2)*X[i-1,])  *exp(-s^2/2)
  X[i, ] <- x1 + err_t
}

obs <- X[-c(1:burn_in), ]



# Plot the observations 
Nplot <- 50
df_obs <- cbind(time = s, val = t(obs[1:Nplot, ])) %>% as_tibble()
names(df_obs) <-  c("time", paste0("obs_", 1:50))

df_res <- pivot_longer(df_obs, !time, 
                       names_to = "obs", values_to = "val")
ggplot(df_res, aes(x = time, y = val, col = obs)) +
  geom_line() + labs(x = "Time (t)", y = "")

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
  ggtitle("FACF of FAR(1)")

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
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Lag (h)", y = " ")  + 
  ggtitle("DACF of FAR(1)")
